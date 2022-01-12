#lang racket/base
;;;
;;; Arrays inspired by srfi 179.
;;;

;; This implementation of multi-dimensional arrays follow the conventions
;; in srfi 179 closely. In a few places I have followed the srfi-179-followup
;; rather than the srfi itself.

;;;
;;; TODO
;;;
;;  [x] = done
;;  [/] = half done
;;  [ ] = not done

;; SRFI
;;  [x] respect safe
;;  [x] compose-indexers
;;  [/] use compose-indexers everywhere relevant
;;      [x] specialized-array-share
;;      [x] make-array
;;      [x] array-extract
;;      [x] array-tile
;;      [x] array-translate (for specialized arrays)
;;      [ ] array-permute
;;      [ ] array-curry
;;      [ ] array-reverse
;;      [ ] array-sample
;;  [x] u1-storage-class
;;  [ ] implement reshape

;; FOLLOWUP
;;  [x] parameters for specialized defaults
;;  [x] array-copy requires same domains
;;  [x] make-specialized-array now has an optional initial value
;;  [/] array-foldl and array-foldr instead of array-fold and array-fold-right
;;  [x] array-assign! now requires source and destination to have same domain
;;  [x] array-copy handles optional arguments for specialized arrays differently
;;  [x] index-rotate, index-first, index-last
;;  [x] array-outer-product
;;  [x] array-inner-product
;;  [/] array-append : support optional storage class
;;  [/] array-stack
;;  [ ] array-reshape

;; QUALITY
;;  [x] port test suite
;;  [/] specialize where possible: mostly done
;;  [/] export safe and unsafe versions (i.e. w/wo contract checking)
;;  [ ] refactor: implement a mover
;;  [ ] documentation

;; RACKET
;;  [x] implement in-array-axis
;;  [x] implement in-array
;;  [x] implement for/array
;;  [x] custom array printer
;;  [ ] custom interval printer
;;  [ ] conversion : array <-> vectors (nested vectors)
;;  [/] conversion : array <-> lists   (nested lists)
;;  [ ] flomat as backing store

;; OTHER
;;  [x] make github repo

;; DESIGN QUESTIONS
;;  * explict strides representation?
;;  * explict indexes
;;  * make-specialized-array : could body be an optional argument?
;;  * array-drop-axis : reduce the dimension of an array (remove an axis with size 1)
;;  *                   (shows intent)

(provide interval-ls interval-us ; internal, used by test-suite

         translation?
         permutation?
         vector-permute
         permutation-inverse
         
         ; export the safe versions
         (rename-out [safe-make-interval                         make-interval]
                     [safe-interval-subset?                      interval-subset?]
                     [safe-interval-intersect                    interval-intersect]
                     [safe-interval-contains-multi-index?        interval-contains-multi-index?]
                     [safe-interval-contains-vector-multi-index? interval-contains-vector-multi-index?] ; extra
                     [safe-interval-scale                        interval-scale]
                     [safe-interval-translate                    interval-translate]
                     [safe-interval-lower-bound                  interval-lower-bound]
                     [safe-interval-upper-bound                  interval-upper-bound]
                     [safe-index-rotate                          index-rotate]
                     [safe-index-first                           index-first]
                     [safe-index-last                            index-last])
         interval?
         interval-dimension
         interval-lower-bounds->list
         interval-upper-bounds->list
         interval-lower-bounds->vector
         interval-upper-bounds->vector
         interval-volume
         interval=
         interval-for-each
         interval-projections
         interval-dilate
         interval-permute
         interval-rotate
         interval-cartesian-product
         interval-strides
         interval->indexer
         compose-indexers

         in-interval
         in-interval/internal-vector
         for/array
         
         (struct-out storage-class)
         generic-storage-class
         u1-storage-class
         ; the other predefined storage classes are
         ; provided where they are defined

         (except-out (struct-out array)
                     array-storage-class
                     array-body
                     array-indexer
                     array-safe?
                     array-elements-in-order?)
         (rename-out [safe-array-storage-class      array-storage-class]
                     [safe-array-body               array-body]
                     [safe-array-indexer            array-indexer]
                     [safe-array-safe?              array-safe?]
                     [safe-array-elements-in-order? array-elements-in-order?]
                     [safe-array-dimension          array-dimension]
                     [safe-array-sample             array-sample]
                     [safe-array->list              array->list]
                     [safe-list->array              list->array]
                     [safe-array-inner-product      array-inner-product]
                     [safe-array-append             array-append]
                     [safe-array-stack              array-stack]
                     )
         array->lists ; extra
         the-unknown-value
         compute-array-elements-in-order?
         specialized-array-default-safe?
         specialized-array-default-mutable?
         mutable-array?
         specialized-array?
         (rename-out [safe-make-specialized-array   make-specialized-array]
                     [safe-specialized-array-share  specialized-array-share]
                     [safe-array-copy               array-copy]
                     [safe-array-tile               array-tile]
                     [safe-array-outer-product      array-outer-product]
                     [safe-array-reverse            array-reverse])
         make-array
         array-curry
         array-extract
         array-translate
         permutation-inverse
         vector-permute
         array-permute
         rotation-permutation
         array-rotate

         array-map
         array-for-each
         array-fold
         array-fold-right
         array-foldl
         array-foldr
         array-reduce
         array-any
         array-every
         
         array-assign!
         specialized-array-reshape
         array-ref
         array-set!
         )

;;;
;;; DEPENDENCIES
;;;

(require racket/vector racket/list ffi/vector racket/contract)
(require data/bit-vector) ; for the u1 storage class
(require (for-syntax racket/base syntax/parse racket/syntax racket/list))

;;;
;;; REPRESENTATION
;;;

; (struct interval (dimension ls us) #:transparent)
;   ls = vector of lower bounds
;   us = vector of upper bounds
;   d  = dimension i.e. the shared length of ls and us

; (struct storage-class (getter setter checker maker copier length default) #:transparent)

; (struct array (domain getter setter storage-class body indexer safe? elements-in-order?))


;;;
;;; SYNTAX UTILITIES
;;;

; A common theme in the code is to specialize the cases for 1, 2, 3 and 4 arguments.
; This avoids both a call to a apply and the allocation of a temporary list.
; A typical example looks like:

;; (define getter (case d
;;                  [(1)  (λ (i)       (specialized-getter body (indexer i)))]
;;                  [(2)  (λ (i j)     (specialized-getter body (indexer i j)))]
;;                  [(3)  (λ (i j k)   (specialized-getter body (indexer i j k)))]
;;                  [(4)  (λ (i j k l) (specialized-getter body (indexer i j k l)))]
;;                  [else (λ is        (specialized-getter body (apply indexer is)))]))

; The macro `specialize` below allow us to write this instead:

;; (define getter (specialize d (i j k l) _ 
;;                            (λ (_) (specialized-getter body (indexer _)))
;;                            (λ is  (specialized-getter body (apply indexer is)))))

(define-syntax (specialize stx)
  (struct splice ())
  (define (replace s from to)
    ; In the syntax object `s` look for `from` and splice `to`.
    ; The marker `from` can appear multiple times.
    (syntax-parse s
      [x:id    (if (free-identifier=? #'x from)
                   (splice)
                   #'x)]
      [(a . d) (define A (replace #'a from to))
               (define D (replace #'d from to))
               (if (splice? A)
                   (datum->syntax s (append to D) from)
                   (datum->syntax s (cons A D) s))]
      [else    s]))
  
  (syntax-parse stx
    [(_specialize dimension:expr (id:id ...) where specialized:expr general:expr)
     (define ids (syntax->list #'(id ...)))
     (define n   (length ids))
     (define ns  (for/list ([i (in-range 1 n)]) i))
     (with-syntax ([(n ...)      ns]
                   [(spec ...)   (for/list ([i ns])
                                   (replace #'specialized #'where (take ids i)))])
     (syntax/loc stx
       (case dimension
         [(n) spec]
         ...
         [else general])))]))


;;;
;;; Intervals
;;;

;; A 1-dimensional interval [l,u[ consists of exact integers l, l+1, ..., u-1.
;; The names l and u stand for the lower bound and upper bound of the interval respectively.
;; We require each interval to have at least one element, so u>l+1.
;; An element of the interval is called an index.

;; Similarly, a 2-dimensional interval [l0,u0[ x [l1,u1[ consists of pairs of exact integers.
;; If the pair is named i0, i1 then i0 is in the first interval and i1 in the second.
;;    l0 ≤ i0 < u0,   l1 ≤ i1 < u1

;; The general case is an d-dimensional interval [l0,u0[ x ... x [lk,uk[ where k=d-1.
;;    l0 ≤ i0 < u0,   ..., lk ≤ ik < uk

;; The number number d is called the dimension of the interval.
;; The total number of elements in an interval is called the volume.
;;    volume = (u0-l0) * ... * (uk-lk)

; An interval is represented as a structure. The dimension field is strictly reduntant,
; but the value is needed over and over.

(struct interval (dimension ls us) #:transparent)
; dimension = the shared length of ls and us
; ls        = vector of lower bounds
; us        = vector of upper bounds

; check-bounds : vector [boolean?] -> boolean?
;   Check that the values in the vector bs can be used as bounds.
;   If pos? is #t, then we also chack that all bounds are positive.
(define (check-bounds bs [pos? #f])
  (and (vector? bs) (> (vector-length bs) 0)
       (for/and ([b (in-vector bs)])
         (if pos?
             (exact-positive-integer? b)
             (exact-integer? b)))))

; safe-make-interval : [ls] us -> interval
;   Return an interval with lower and upper bounds given by the vectors ls and us.
(define safe-make-interval
  (case-lambda
    [(us) (unless (check-bounds us #t)
            (raise-arguments-error
             'safe-make-interval
             "the upper bounds must be a vector of exact positive integers"
             "upper bounds" us))
          (make-interval us)]
    [(ls us) (unless (check-bounds ls)
               (raise-arguments-error
                'safe-make-interval
                "the lower bounds must be a vector of exact integers"
                "lower bounds" ls))
             (unless (check-bounds us)
               (raise-arguments-error
                'safe-make-interval
                "the upper bounds must be a vector of exact integers"
                "upper bounds" us))
             (unless (= (vector-length ls) (vector-length us))
               (raise-arguments-error 'make-interval
                                      "the number of lower and upper bounds must be the same"
                                      "lower bounds" ls
                                      "upper bounds" us))
             (unless (for/and ([l (in-vector ls)]
                               [u (in-vector us)])
                       (< l u))
               (raise-arguments-error 'make-interval
                                      "all lower bounds must be strictly smaller than the corresponding upper bound"
                                      "lower bounds" ls
                                      "upper bounds" us))
             (make-interval ls us)]))

; make-interval : [ls] us -> interval
;  Return an interval with lower and upper bounds given by ls and us.
(define make-interval
  (case-lambda
    [(us)    (define d  (vector-length us))
             (define ls (make-vector d 0))
             (interval d ls us)]
    [(ls us) (define d (vector-length ls))
             (interval d ls us)]
    [else    (raise-argument-error 'make-interval "expected one or two arguments")]))


(define/contract (safe-interval-lower-bound interval index)
  (-> interval? exact-integer?
      any) ; exact integer
  (interval-lower-bound interval index))

; interval-lower-bound : interval index -> exact-integer
;  Return the i'th lower bound of the interval.
(define (interval-lower-bound interval i)
  (define who 'interval-lower-bound)
  (define ls (interval-ls interval))
  (define d  (interval-dimension interval))
  (unless (>= i 0)
    (raise-arguments-error who
                           "the index must be non-negative" 
                           "index" i
                           "interval" interval))
  (unless (< i d)
    (raise-arguments-error who
                           "the index must be less than the dimension of the interval"
                           "interval" interval
                           "index"    i))
  (vector-ref ls i))


(define/contract (safe-interval-upper-bound interval index)
  (-> interval? exact-integer?
      any) ; exact integer
  (interval-upper-bound interval index))

; interval-upper-bound : interval index -> exact-integer
;  Return the i'th upper bound of the interval.
(define (interval-upper-bound interval i)
  (define us (interval-us interval))
  (define d  (interval-dimension interval))
  (unless (< -1 i d)
    (raise-arguments-error 'interval-upper-bound
                           "the index must be less than the dimension of the interval"
                           "interval" interval
                           "index"    i))
  (vector-ref us i))

(define (interval-lower-bounds->list interval)
  (define ls (interval-ls interval))
  (vector->list ls))

(define (interval-upper-bounds->list interval)
  (define us (interval-us interval))
  (vector->list us))

(define (interval-lower-bounds->vector interval)
  (define ls (interval-ls interval))
  (vector-copy ls))

(define (interval-upper-bounds->vector interval)
  (define us (interval-us interval))
  (vector-copy us))

; interval-volume : interval -> exact-integer
;   Compute the number of elements in the interval.
;   This number is called the volumne of the interval.
(define (interval-volume interval)
  (define ls (interval-ls interval))
  (define us (interval-us interval))
  (for/fold ([volume 1])
            ([l (in-vector ls)]
             [u (in-vector us)])
    (* volume (- u l))))

; interval= : interval interval -> boolean
;   Return #t if the intervals represents the same interval.
(define (interval= interval1 interval2)
  (or (eq? interval1 interval2)
      (and (=      (interval-dimension interval1) (interval-dimension interval2))
           (equal? (interval-ls        interval1) (interval-ls        interval2))
           (equal? (interval-us        interval1) (interval-us        interval2)))))


(define/contract (safe-interval-subset? interval1 interval2)
  (-> interval? interval? boolean?)
  (interval-subset? interval1 interval2))

; interval-subset? : interval interval -> boolean
;  Return #t if interval1 is a subset of interval2.
;  That is, check that all elements of interval1 belongs to interval2.
(define (interval-subset? interval1 interval2)
  (unless (= (interval-dimension interval1)
             (interval-dimension interval2))
    (raise-arguments-error interval-subset?
                           "the intervals must have the same dimension"
                           "first  interval " interval1
                           "second interval " interval2))
  (for/and ([l1 (in-vector (interval-ls interval1))]
            [l2 (in-vector (interval-ls interval2))]
            [u1 (in-vector (interval-us interval1))]
            [u2 (in-vector (interval-us interval2))])
    (and (>= l1 l2)
         (<= u1 u2))))

(define/contract (safe-interval-contains-multi-index? interval . indices)
  (-> interval? exact-integer? ... boolean?)
  (define d (interval-dimension interval))
  (unless (= d (length indices))
    (raise-argument-error 'safe-interval-contains-multi-index?
                          "The dimension of the interval and the number of indices must be the same"
                          "interval" interval
                          "indices"  indices))     
  (apply interval-contains-multi-index? interval indices))

; interval-contains-multi-index? interval exact-integer ... -> boolean
;   Return #t if the indices given belong to the interval.
(define (interval-contains-multi-index? interval . indices)
  (define d (interval-dimension interval))
  (and (= d (length indices))
       (for/and ([l (in-vector (interval-ls interval))]
                 [u (in-vector (interval-us interval))]
                 [i (in-list indices)])
         (and (<= l i) (< i u)))))


(define/contract (safe-interval-contains-vector-multi-index? interval indices)
  (-> interval? (vectorof exact-integer?)
      boolean?)
  (define d (interval-dimension interval))
  (unless (= d (vector-length indices))
    (raise-argument-error 'safe-interval-contains-vector-multi-index?
                          "The dimension of the interval and the number of indices must be the same"
                          "interval" interval
                          "indices"  indices))     
  (interval-contains-vector-multi-index? interval indices))

; interval-contains-vector-multi-index? interval vector -> boolean
;   Return #t if the indices given belong to the interval.
(define (interval-contains-vector-multi-index? interval indices)
  (define d (interval-dimension interval))
  (and (= d (vector-length indices))
       (for/and ([l (in-vector (interval-ls interval))]
                 [u (in-vector (interval-us interval))]
                 [i (in-vector indices)])
         (and (<= l i) (< i u)))))

; interval-projections : interval index -> interval interval
;   Split the interval in two parts, where the second interval has the dimension `right-dimension`.
;   That is, the interval  [l0,u0[ x ... x [lk,uk[ is split into
;      [l0,u0[ x ... x [l_k-d,u_k-d[   and  [l_k-d,u_k-d[ x ... x [lk,uk[
(define (interval-projections interval right-dimension)
  (define d (interval-dimension interval))
  (unless (< 0 right-dimension d)
    (raise-argument-error 'interval-projections
                          "the right dimension must be strictly between zero and the interval dimension"
                          "interval"        interval
                          "right dimension" right-dimension))
  (define ls (interval-ls interval))
  (define us (interval-us interval))
  (define-values (left-ls right-ls) (vector-split-at ls (- d right-dimension)))
  (define-values (left-us right-us) (vector-split-at us (- d right-dimension)))
  (values (make-interval left-ls  left-us)
          (make-interval right-ls right-us)))

(define (interval-for-each f interval)
  (unless (interval? interval)
    (raise-argument-error 'interval-for-each
                          "expected an interval as the second argument"
                          "interval" interval))
  (unless (procedure? f)
    (raise-argument-error 'interval-for-each
                          "expected a procedure as the first argument"
                          "procedure" f))
  (define d (interval-dimension interval))
  (specialize d (i j k l) _
              (for ([(_) (in-interval interval)]) (f _))
              (for ([is  (in-interval/internal-vector interval)])
                (apply f (vector->list is)))))
  

(define (interval-dilate interval lower-diffs upper-diffs)
  (define d (interval-dimension interval))
  (unless (= d (vector-length lower-diffs))
    (raise-arguments-error
     'interval-dilate 
     "the dimension of the interval and the length of the vector of lower differences must be equal"
     "interval" interval
     "lower differences" lower-diffs))
  (unless (= d (vector-length upper-diffs))
    (raise-arguments-error
     'interval-dilate 
     "the dimension of the interval and the length of the vector of upper differences must be equal"
     "interval" interval
     "upper differences" upper-diffs))
  (define new-ls (for/vector #:length d
                     ([l (in-vector (interval-ls interval))]
                      [L (in-vector lower-diffs)])
                   (+ l L)))
  (define new-us (for/vector #:length d
                     ([u (in-vector (interval-us interval))]
                      [U (in-vector upper-diffs)])
                   (+ u U)))
  (define non-empty? (for/and ([l (in-vector new-ls)]
                               [u (in-vector new-us)])
                       (< l u)))
  (unless non-empty?
    (raise-arguments-error 'interval-dilate
                           "the resulting interval must be non-empty"
                           "interval" interval
                           "lower differences" lower-diffs
                           "upper differences" upper-diffs))
  (make-interval new-ls new-us))


(define/contract (safe-interval-intersect interval1 . intervals)
  (-> interval? interval? ...
      (or/c interval? boolean?))
  (apply interval-intersect interval1 intervals))


(define (interval-intersect interval1 . intervals)
  (cond
    [(empty? intervals)        interval1]
    [(empty? (rest intervals)) (interval-intersect2 interval1 (first intervals))]
    [else                      (define tmp (interval-intersect2 interval1 (first intervals)))
                               (and tmp
                                    (apply interval-intersect
                                           tmp
                                           (rest intervals)))]))


(define (interval-intersect2 interval1 interval2)
  (define d (interval-dimension interval1))
  (unless (= d (interval-dimension interval2))
    (raise-arguments-error 'interval-intersect
                           "the two intervals must have the same dimension"
                           "first interval"  interval1
                           "second interval" interval2))
  (define new-ls (for/vector #:length d
                     ([l1 (in-vector (interval-ls interval1))]
                      [l2 (in-vector (interval-ls interval2))])
                   (max l1 l2)))
  (define new-us (for/vector #:length d
                     ([u1 (in-vector (interval-us interval1))]
                      [u2 (in-vector (interval-us interval2))])
                   (min u1 u2)))
  (define non-empty? (for/and ([l (in-vector new-ls)]
                               [u (in-vector new-us)])
                       (< l u)))
  (and non-empty? 
       (make-interval new-ls new-us)))  

(define (translation? v)
  (and (vector? v)
       (for/and ([x (in-vector v)])
         (exact-integer? x))))

(define (permutation? v)
  (and (vector? v)
       (let ()
         (define n    (vector-length v))
         (define seen (make-vector n #f))
         (and (for/and ([x (in-vector v)])
                (and (exact-integer? x)
                     (<= 0 x (- n 1))
                     (vector-set! seen x #t)))
              (for/and ([x (in-vector seen)])
                x)
              #t))))

(define/contract (safe-interval-translate interval translation)
  (-> interval? translation?
      interval?)
  (interval-translate interval translation))


(define (interval-translate interval translation)
  (define d (interval-dimension interval))
  (unless (= d (vector-length translation))
    (raise-arguments-error
     'interval-translate 
     "the dimension of the interval and the length of the translation vector must be equal"
     "interval" interval
     "translation" translation))  
  (define new-ls (for/vector #:length d
                     ([l (in-vector (interval-ls interval))]
                      [t (in-vector translation)])
                   (+ l t)))
  (define new-us (for/vector #:length d
                     ([u (in-vector (interval-us interval))]
                      [t (in-vector translation)])
                   (+ u t)))
  (make-interval new-ls new-us))


(define (interval-permute interval permutation)
  (define d (interval-dimension interval))
  (unless (= d (vector-length permutation))
    (raise-arguments-error
     'interval-permute 
     "the dimension of the interval and the length of the permutation vector must be equal"
     "interval" interval
     "permutation" permutation))
  (define ls (interval-ls interval))
  (define us (interval-us interval))
  (define new-ls (for/vector #:length d ([p (in-vector permutation)])
                   (vector-ref ls p)))
  (define new-us (for/vector #:length d ([p (in-vector permutation)])
                   (vector-ref us p)))
  (make-interval new-ls new-us))

(define (interval-rotate interval dim)
  (define d (interval-dimension interval))
  (unless (and (<= 0 dim) (< dim d))
    (raise-arguments-error
     'interval-rotate 
     "the number of dimensions to rotate must be less than the interval dimension"
     "interval" interval
     "rotation" dim))
  (define ls (interval-ls interval))
  (define us (interval-us interval))
  (define new-ls (for/vector #:length d ([i (in-range d)])
                   (vector-ref ls (remainder (+ i dim) d))))
  (define new-us (for/vector #:length d ([i (in-range d)])
                   (vector-ref us (remainder (+ i dim) d))))
  (make-interval new-ls new-us))


(define (zero-vector? v)
  (and (vector? v)
       (for/and ([x (in-vector v)])
         (zero? x))))

(define (scales-vector? v)
  (and (vector? v)
       (for/and ([x (in-vector v)])
         (exact-positive-integer? x))))

(define/contract (safe-interval-scale interval scales)
  (-> interval? scales-vector?
      interval?)  
  (interval-scale interval scales))

(define (interval-scale interval scales)
  ; scales is a vector of positive exact integers
  (define d  (interval-dimension interval))
  (define ls (interval-ls interval))
  (define us (interval-us interval))
  (define sd (vector-length scales))
  (unless (zero-vector? ls)
    (raise-arguments-error
     'interval-scale  "all lower bounds must be zero" "interval" interval))
  (unless (= d sd)
    (raise-arguments-error
     'interval-scale "the vector of scales must have the same dimension as the interval"
     "interval" interval
     "scales"   scales))
  (define new-us
    (for/vector #:length d
        ([u (in-vector us)]
         [s (in-vector scales)])
      (inexact->exact (ceiling (/ u s)))))
  (make-interval ls new-us))


(define (interval-cartesian-product interval . intervals)
  (make-interval
   (apply vector-append (map interval-lower-bounds->vector (cons interval intervals)))
   (apply vector-append (map interval-upper-bounds->vector (cons interval intervals)))))



; next-index! : vector vector vector -> vector
;   The vector xs contains indices in the interval given by lower and upper bounds ls and us.
;   Update xs so it contains the next index (in lexicographic order).
;   If there is no next index, then return #f.
(define (next-index! xs ls us)
  ; xs, ls, and us are indices, lower bounds and upper bounds respectively
  ; xs should be a mutable vector and  l_i <= x_i <= u_i
  ; Note: returns false when xs is the last legal index
  (define d (vector-length xs))
  (let loop ([i (- d 1)])
    (cond
      [(< i 0)
       #f]
      [else
       (define xi     (vector-ref xs i))
       (define ui- (- (vector-ref us i) 1))
       (cond
         [(< xi ui-) (vector-set! xs i (+ xi 1))
                     xs]
         [else       (define li (vector-ref ls i))
                     (vector-set! xs i li)
                     (loop (- i 1))])])))

; interval-last-index : interval -> vector
;   Return a vector containing the last index of the interval.
(define (interval-last-index interval)
  (define us (interval-us interval))
  (for/vector #:length (vector-length us)
      ([u (in-vector us)])
    (- u 1)))


; These checker functions are called by in-interval.

(define (check-interval who x)
  (unless (interval? x)
    (raise-argument-error who "interval" x)))

(define (check-interval-dimension who d x)
  (unless (and (interval? x) (= (interval-dimension x) d))
    (raise-argument-error
     who (string-append "interval of dimension " (number->string d)) x)))



; We want to use intervals `for`-loops. For that purpose, we define `in-interval`.
; The general syntax for a for-clause becomes:
;    [(a ...) (in-interval interval)]
; where a ... are identifiers that will be bound to indices from the interval.
; The number of identifers must be the same as the dimension of the interval.
; The sequence loops through the interval in lexicographic order.

; > (for/list ([x (in-interval (make-interval #(2) #(4)))]) x)
; '(2 3)

; > (for/list ([(x y) (in-interval (make-interval #(2 12) #(4 14)))]) (list x y))
; '((2 12) (2 13) (3 12) (3 13))

; > (for/list ([(x y z) (in-interval (make-interval #(2 12 22) #(4 14 24)))]) (list x y z))
; '((2 12 22) (2 12 23) (2 13 22) (2 13 23) (3 12 22) (3 12 23) (3 13 22) (3 13 23))


(define-sequence-syntax in-interval
  (lambda () #'in-interval/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(a) (_in-interval int)]
       #'[(a)
          (:do-in
           ([(l) (begin (check-interval-dimension 'in-interval 1 int)
                        (vector-ref (interval-ls int) 0))]
            [(u) (vector-ref (interval-us int) 0)]) 
           #t
           ([i l])
           (< i u)
           ([(a) i])
           #t #t
           [(+ i 1)])]]
      [[(a b) (_in-interval int)]
       #'[(a b)
          (:do-in
           ([(l0) (begin (check-interval-dimension 'in-interval 2 int) 
                         (vector-ref (interval-ls int) 0))]
            [(l1) (vector-ref (interval-ls int) 1)]
            [(u0) (vector-ref (interval-us int) 0)]
            [(u1) (vector-ref (interval-us int) 1)])
           #t
           ([i l0] [j l1])
           (not (= i u0))
           ([(a) i] [(b) j])
           #t #t
           [(if (< j (- u1 1))  i (+ i 1))
            (if (= j (- u1 1)) l1 (+ j 1))])]]
      [[(a b c) (_in-interval int)]
       #'[(a b c)
          (:do-in
           ([(l0)     (begin (check-interval-dimension 'in-interval 3 int)
                             (vector-ref (interval-ls int) 0))]
            [(l1)     (vector-ref (interval-ls int) 1)]
            [(l2)     (vector-ref (interval-ls int) 2)]
            [(u0)     (vector-ref (interval-us int) 0)]
            [(u0-) (- (vector-ref (interval-us int) 0) 1)]
            [(u1-) (- (vector-ref (interval-us int) 1) 1)]
            [(u2-) (- (vector-ref (interval-us int) 2) 1)])
           #t
           ([i l0] [j l1] [k l2])
           (not (= i u0))
           ([(a) i] [(b) j] [(c) k])
           #t #t
           [(if (or (< j u1-) (< k u2-))  i
                (+ i 1))
            (if (< k u2-) j
                (if (= j u1-) l1
                    (+ j 1)))
            (if (= k u2-) l2
                (+ k 1))])]]
      [[(a b c d) (_in-interval int)]
       #'[(a b c d)
          (:do-in
           ([(l0)     (begin (check-interval-dimension 'in-interval 4 int)
                             (vector-ref (interval-ls int) 0))]
            [(l1)     (vector-ref (interval-ls int) 1)]
            [(l2)     (vector-ref (interval-ls int) 2)]
            [(l3)     (vector-ref (interval-ls int) 3)]
            [(u0)     (vector-ref (interval-us int) 0)]
            [(u0-) (- (vector-ref (interval-us int) 0) 1)]
            [(u1-) (- (vector-ref (interval-us int) 1) 1)]
            [(u2-) (- (vector-ref (interval-us int) 2) 1)]
            [(u3-) (- (vector-ref (interval-us int) 3) 1)])
           #t
           ([i l0] [j l1] [k l2] [l l3])
           (not (= i u0))
           ([(a) i] [(b) j] [(c) k] [(d) l])
           #t #t
           [(if (or (< j u1-) (< k u2-) (< l u3-))  i
                (+ i 1))
            (if (or           (< k u2-) (< l u3-))  j
                (if (= j u1-) l1 (+ j 1)))
            (if                         (< l u3-)   k
                (if (= k u2-) l2 (+ k 1)))
            (if (= l u3-) l3 (+ l 1))])]]
      ; more than four
      [[(a ...) (_in-interval int)]
       (let ([d (length (syntax->list #'(a ...)))])
         (with-syntax ([d d]
                       [(j ...) ; 0, 1, ..., d-1
                        (for/list ([j (in-range 0 d)]) j)])
           #'[(a ...)
              (:do-in
               ; outer ids
               ([(ls)  (begin (check-interval 'in-interval int)
                              (interval-ls int))]
                [(us)  (interval-us int)]
                [(is)  (vector-copy (interval-ls int))] ; aka the first index
                #;[(end) (interval-last-index int)])
               ; outer-check
               (check-interval-dimension 'in-interval d int)
               ; loop ids
               ()
               ; positive guard (i.e. continue while)
               is ; note : becomes false, by next-index!
               ; inner ids
               ([(a) (vector-ref is j)] ...)
               #t
               (next-index! is ls us) ; post guard (used for side effect)
               [])]))]
      [_ #f])))

; This version uses a vector to represent the running index.

(define-sequence-syntax in-interval/internal-vector
  ; This version is unsafe. It exposes the vector `is`,
  ; we use to iterate through the indices.
  (λ () #'in-interval/internal-vector/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(as) (_in-interval/internal-vector int)]
       #'[(as)
          (:do-in
           ; outer ids
           ([(ls)  (begin (check-interval 'in-interval/internal-vector int)
                          (interval-ls int))]
            [(us)  (interval-us int)]
            [(is)  (vector-copy (interval-ls int))] ; aka the first index
            #;[(end) (interval-last-index int)])
           ; outer-check
           #t 
           ; loop ids
           ()
           ; positive guard (i.e. continue while)
           is ; becomes #f when the last index is reached
           ; inner ids
           ([(as) is]) ; we don't copy is here - assumes user doesn't mutate it
           #t
           (next-index! is ls us) ; post guard (used for side effect)
           [])]])))


; The sequence array-axis loops through the elements of a single axis of the array.
; A `for`-clause of the form
;     [(x) (in-array-axis array axis)]
; will loop through the elements on the given axis of the array.

; In the 1-dimensioncal case, we only have one axis:
;; > (let ()
;;     (define dom (make-interval #(2) #(4)))
;;     (define arr (for/array #:domain dom ([x (in-interval dom)]) x))
;;     (for/list ([x (in-array-axis arr 0)]) x))
;; '(2 3)

; In the 2-dimensional case, we have axis 0 and axis 1.
; > (let ()
;     (define dom (make-interval #(2 12) #(4 14)))
;     (define arr (for/array #:domain dom ([(x y) (in-interval dom)]) (list x y)))
;     (for/list ([z (in-array-axis arr 0)]) z))
; '((2 12) (3 12))

; > (let ()                               
;     (define dom (make-interval #(2 12) #(4 14)))
;     (define arr (for/array #:domain dom ([(x y) (in-interval dom)]) (list x y)))
;     (for/list ([z (in-array-axis arr 1)]) z))
; '((2 12) (2 13))

; TODO: This was an experiment. What should x be bound to when
;       running through the axis? Use array-curry or array-tile instead?

#;(define-sequence-syntax in-array-axis
  (λ ()    #'in-array-axis/proc)
  (λ (stx) (syntax-case stx ()
             ; The general case: loop through the elements of array axis.
             ; No assumptions of the array dimension is made.
             [[(x) (_in-array-axis array axis)]
              #'[(x) (:do-in
                      ; outer ids
                      ([(A)      array]
                       [(a)      axis]
                       ; these depend on A and a
                       [(domain) #f]
                       [(ls)     #f]
                       [(us)     #f]
                       [(get)    #f]
                       [(la)     #f]
                       [(ua)     #f]
                       [(is)     #f])
                      ; outer check
                      (begin
                        (unless (array? A)
                          (raise-argument-error 'in-array-axis "array" A))
                        (unless (<= 0 a (- (array-dimension A) 1))
                          (raise-argument-error
                           'in-array-axis
                           "the axis must be less than the array dimension"
                           "axis" a))
                        ; we can now safely acess A
                        (set! get    (array-getter A))
                        (set! domain (array-domain A))
                        (set! ls     (interval-ls domain))
                        (set! us     (interval-us domain))
                        (set! la     (vector-ref ls a))
                        (set! ua     (vector-ref us a))
                        ; the running index:
                        (set! is     (vector-copy ls)))
                      ; loop ids
                      ([i la])
                      ; pos guard (aka while test)
                      (< i ua)
                      ; inner ids
                      ([(x) (begin (vector-set! is a i)
                                   (apply get (vector->list is)))])
                      ; pre-guard
                      #t 
                      ; post-guard
                      #t 
                      ; loop arguments
                      ((+ i 1)))]]
             [[(x) (_in-array-axis array axis #:dim 1)]
              ; For a one dimensional array, we don't need to represent
              ; the running index as a vector is.
              ; Also we know the axis is 0 ...
              #'[(x) (:do-in
                      ; outer ids
                      ([(A)      array]
                       [(a)      axis]
                       ; these depend on A and a
                       [(domain) #f]
                       [(ls)     #f]
                       [(us)     #f]
                       [(get)    #f]
                       [(la)     #f]
                       [(ua)     #f])
                      ; outer check
                      (begin
                        (unless (array? A)
                          (raise-argument-error 'in-array-axis "array" A))
                        (unless (<= 0 a (- (array-dimension A) 1))
                          (raise-argument-error
                           'in-array-axis
                           "the axis must be less than the array dimension"
                           "axis" a))
                        ; we can now safely access A
                        (set! get    (array-getter A))
                        (set! domain (array-domain A))
                        (set! ls     (interval-ls domain))
                        (set! us     (interval-us domain))
                        (set! la     (vector-ref ls 0))
                        (set! ua     (vector-ref us 0)))
                      ; loop ids
                      ([i la])
                      ; pos guard (aka while test)
                      (< i ua)
                      ; inner ids
                      ([(x) (get i)])
                      ; pre-guard
                      #t 
                      ; post-guard
                      #t 
                      ; loop arguments
                      ((+ i 1)))]]
             [[(x) (_in-array-axis array axis #:dim 2 #:others (other-index))]
              ; For a one dimensional array, we need to know the value of the non-running index.
              ; Also we know the axis is 0 or 1.
              #'[(x) (:do-in
                      ; outer ids
                      ([(A)      array]
                       [(a)      axis]
                       [(j)      other-index]
                       ; these depend on A and a
                       [(domain) #f]
                       [(ls)     #f]
                       [(us)     #f]
                       [(get)    #f]
                       [(la)     #f]
                       [(ua)     #f]
                       [(lj)     #f]
                       [(uj)     #f])
                      ; outer check
                      (begin
                        (unless (array? A)
                          (raise-argument-error 'in-array-axis "array" A))
                        (unless (<= 0 a (- (array-dimension A) 1))
                          (raise-argument-error
                           'in-array-axis
                           "the axis must be less than the array dimension"
                           "axis" a))
                        ; we can now safely access A
                        (set! get    (array-getter A))
                        (set! domain (array-domain A))
                        (set! ls     (interval-ls domain))
                        (set! us     (interval-us domain))
                        (set! la     (vector-ref ls a))
                        (set! ua     (vector-ref us a))
                        (set! lj     (vector-ref ls (- 1 a)))
                        (set! uj     (vector-ref us (- 1 a))))
                      ; loop ids
                      ([i la])
                      ; pos guard (aka while test)
                      (< i ua)
                      ; inner ids
                      ([(x) (if (= a 0) (get i j) (get j i))])
                      ; pre-guard
                      #t 
                      ; post-guard
                      #t 
                      ; loop arguments
                      ((+ i 1)))]])))



(define-sequence-syntax in-array
  (λ ()    #'in-array/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_in-array array)]
       #'[(x) (:do-in
               ; outer ids
               ([(A)      array]
                [(domain) #f]
                [(ls)     #f]
                [(us)     #f]
                [(get)    #f]
                [(l0)     #f]
                [(u0)     #f])
               ; outer check
               (begin
                 (unless (array? A)
                   (raise-argument-error 'in-array "array" A))
                 (unless (= (array-dimension A) 1)
                   (raise-argument-error
                    'in-array
                    "based on the number of identifers, expected an one dimensional array"
                    A))
                 (set! get    (array-getter A))
                 (set! domain (array-domain A))
                 (set! ls     (interval-ls domain))
                 (set! us     (interval-us domain))
                 (set! l0     (vector-ref ls 0))
                 (set! u0     (vector-ref us 0)))
               ; loop ids
               ([i  l0])
               ; pos guard (aka while test)
               (< i u0)
               ; inner ids
               ([(x) (get i)])
               ; pre-guard
               #t 
               ; post-guard
               #t 
               ; loop arguments
               ((+ i 1)))]]
      )))

; SYNTAX
; (for/array #:domain        dom-expr
;            #:storage-class storage-class-expr
;            #:safe?         safe?-expr
;            (for-clause ...)
;            body-or-break ... body)

; This comprehension constructs a specialized array with domain given by dom-expr.
; The storage-class and safe? are optional.
; The default storage class is generic-storage-class.
; The default value for safe? is taken from (specialized-array-default-safe?).

(define-syntax (for/array stx)
  (syntax-parse stx
    [(_for/array #:domain        dom-expr:expr
                 #:storage-class storage-class-expr:expr
                 #:safe?         safe?-expr:expr
                 (for-clause ...)
                 body-or-break ... body)
     (syntax/loc stx
       (let ()
         (define dom           dom-expr)
         (define safe?         safe?-expr)
         (define storage-class storage-class-expr)

         (unless (interval? dom)
           (raise-argument-error 'for/array
                                 "expected an interval"
                                 "domain" dom))
         (unless (storage-class? storage-class)
           (raise-argument-error 'for/array
                                 "expected a storage class"
                                 "storage class" storage-class))
         (unless (boolean? safe?)
           (raise-argument-error 'for/array
                                 "expected a boolean"
                                 "safe?" safe?))
         
         (define A (make-specialized-array dom storage-class safe?))
         (define the-body (array-body A))

         (define set (storage-class-setter storage-class))
         (define vol (interval-volume dom))
         (for ([i vol]
               for-clause ...)
           body-or-break ...
           (define v (let () body))
           (set the-body i v))
         A))]
    [(_for/array #:domain        dom-expr:expr
                 #:safe?         safe?-expr:expr
                 (for-clause ...) body-or-break ... body)
     (syntax/loc stx
       (for/array #:domain        dom-expr
                  #:storage-class generic-storage-class
                  #:safe?         safe?-expr
                  (for-clause ...) body-or-break ... body))]
    [(_for/array #:domain        dom-expr:expr
                 (for-clause ...) body-or-break ... body)
     (syntax/loc stx
       (for/array #:domain        dom-expr
                  #:storage-class generic-storage-class
                  #:safe?         (specialized-array-default-safe?)
                  (for-clause ...) body-or-break ... body))]
    [(_for/array #:domain        dom-expr:expr
                 #:storage-class storage-class-expr:expr
                 #:safe?         safe?-expr:expr
                 (for-clause ...) body-or-break ... body)
     (syntax/loc stx
       (for/array #:domain        dom-expr
                  #:storage-class storage-class-expr
                  #:safe?         (specialized-array-default-safe?)
                  (for-clause ...) body-or-break ... body))]))


;;;
;;; Storage Classes
;;;

; We call arrays which use vector-like datastructures to store the elements for *specialized arrays*.
; Specialized arrays can be implemented more efficiently than general arrays.
; A storage class represents an interface to a vector-like data structure.

(struct storage-class (getter setter checker maker copier length default))
(define make-storage-class storage-class)

; The archetypical storage class is a vector.

(define generic-storage-class
  (make-storage-class vector-ref         ; getter
                      vector-set!        ; setter
                      (lambda (arg) #t)  ; checker
                      make-vector        ; maker
                      vector-copy!       ; copier
                      vector-length      ; length
                      #f))               ; default value

; - (maker n value) returns a linearly addressed object containing n elements of value `value`.

; - `copier` may be #f or a procedure; if a procedure then if `to` and `from` were created by `maker`,
;   then (copier to at from start end) copies elements from `from` beginning at `start` (inclusive)
;   and ending at `end` (exclusive) to `to` beginning at `at`. It is assumed that all the indices
;   involved are within the domain of `from` and `to`, as needed. The order in which the elements
;   are copied is unspecified.

; - If v is an object created by (maker n value) and 0 <= i < n, then (getter v i) returns the
;   current value of the i'th element of v, and (checker (getter v i)) => #t.

; - If v is an object created by (maker n value), 0 <= i < n, and (checker val) => #t,
;   then (setter v i val) sets the value of the i'th element of v to val.

; - If v is an object created by (maker n value) then (length v) returns n.


(define-syntax (define-storage-class stx)
  (syntax-parse stx
    [(_define-storage-class prefix default)
     (with-syntax ([pre-vector-ref    (format-id #'prefix "~avector-ref"     #'prefix #:source stx)]
                   [pre-vector-set!   (format-id #'prefix "~avector-set!"    #'prefix #:source stx)]
                   [make-pre-vector   (format-id #'prefix "make-~avector"    #'prefix #:source stx)]
                   [pre-vector-copy!  (format-id #'prefix "~avector-copy!"   #'prefix #:source stx)]
                   [pre-vector-length (format-id #'prefix "~avector-length"  #'prefix #:source stx)]
                   [pre-storage-class (format-id #'prefix "~a-storage-class" #'prefix #:source stx)]
                   [pre-checker       (format-id #'prefix "~achecker"        #'prefix #:source stx)])
     (syntax/loc stx
       (begin
         ; Note: This really ought to in ffi/vector
         (provide pre-storage-class)
         (define (pre-vector-copy! dest dest-start src
                                   [src-start 0] [src-end (pre-vector-length src)])
           (for ([i (in-naturals dest-start)]
                 [j (in-range src-start src-end)])
             (pre-vector-set! dest i (pre-vector-ref src j))))
         (define (pre-checker arg) #t)
         (define pre-storage-class
           (make-storage-class pre-vector-ref
                               pre-vector-set!
                               pre-checker
                               make-pre-vector
                               pre-vector-copy!
                               pre-vector-length
                               default)))))]))

(define-syntax (define-storage-class* stx)
  (syntax-parse stx
    [(_define-storage-class (~and clause [(prefix ...) default]) ...)
     (with-syntax ([((default ...) ...)
                    (for/list ([clause (syntax->list #'(clause ...))])
                      (syntax-parse clause
                        [[prefixes default]
                         (make-list (length (syntax->list #'prefixes)) #'default)]))])
       (syntax/loc stx
         (begin
           (begin (define-storage-class prefix default) ...)
           ...)))]))
              

(define-storage-class*
  [(s8 s16 s32 s64) 0] ; signed, exact
  [(u8 u16 u32 u64) 0] ; unsigned, exact
  ; [(f32) 0.f]        ; floating point, inexact
  [(f64) 0.0]          ; floating point, inexact
  )

; These are not available from ffi/vector
; u1        ; unsigned 1 bit numbers
; f8 f16    ; 8 and 16 bit floating point
; c64 c128  ; 64 and 128 bit complex numbers

(define u1-storage-class
  (let ()
    (define (maker n value)
      (define who 'u1-storage-class:maker)
      (case value
        [(0)   (make-bit-vector n #f)]
        [(1)   (make-bit-vector n #t)]
        [else  (raise-arguments-error
                who "value can't be stored in u1 body; 0 or 1 expected" "value" value)]))
    (define (getter body i)
      (define who 'u1-storage-class:getter)
      (unless (bit-vector? body)
        (raise-arguments-error who "bit-vector expected" "body" body))
      (case (bit-vector-ref body i)
        [(#f) 0] [(#t) 1]))
    (define (setter body i value)
      (define who 'u1-storage-class:setter)
      (unless (bit-vector? body)
        (raise-arguments-error who "bit-vector expected" "body" body))      
      (case value
        [(0)   (bit-vector-set! body i #f)]
        [(1)   (bit-vector-set! body i #t)]
        [else  (raise-arguments-error
                who "value can't be stored in u1 body; 0 or 1 expected" "value" value)]))
    (define (checker val) (case val [(0 1) #t] [else #f]))
    (define (length body) (bit-vector-length body))
    (define (copier to at from start end)
      (define who 'u1-storage-class:copier)
      (unless (and (< -1 start (length from))
                   (< -1 end   (length from)))
        (raise-arguments-error who
                         "the source start and end must be between 0 and length (exclusive)"
                         "start " start
                         "end   " end
                         "length" (length from)))
      (unless (< (+ at (- end start)) (length to))
        (raise-arguments-error who "not enough room destination"
                               "destination length" (length to)
                               "destination start " at
                               "source start      " start
                               "source end        " end))
      
      (for ([i (in-range start end)]
            [j (in-naturals at)])
        (bit-vector-set! to j (bit-vector-ref from i))))
    (define default 0)
    (storage-class getter setter checker maker copier length default)))


;;;
;;; Arrays
;;;

; An array is conceptually a mapping from an interval indices to an element.
; If the element associated with a given set of indices can be updated, the array is *mutable*.

; The terminology used is:
; A `getter` is a map from a set of interval indices to an element.
; A `setter` given a set of indices and a value updates the array.

; Specialized arrays are called *safe*, if the `getter` and `setter` checks it arguments.
; The indices must be exact integers and for the setter, the new value needs to be storable. 

; For generalized arrays, it is up to the programmer that provides the getter and setter
; wheter to check the arguments or not.

; The default settings for specialzed arrays are stored in the parameters
; `specialized-array-default-safe?` and `specialized-array-default-mutable?`.

; For a specialized array (which stores the elements in a vector-like backing store),
; the elements an arrays may or may not occupy a contiguous area of the backing store.
; If they do, we will say that the elements are stored "in order".
; The representation of an array will have a field `elements-in-order?` to store
; this information. The field value is computed "lazily" in the sense, that the
; initial value is `the-unknown-value` and will be updated to #t or #f, only
; if the information is needed.


; The parameters containing the default settings for specialized arrays.

(define ((make-boolean-guard who) x)
  (unless (boolean? x)
    (raise-argument-error who "boolean" x))
  ; parameter guards returns the new value after cheking
  x)

(define specialized-array-default-safe?    (make-parameter #t (make-boolean-guard 'specialized-array-default-safe?)))
(define specialized-array-default-mutable? (make-parameter #t (make-boolean-guard 'specialized-array-default-mutable?)))

; The unknown value.

(struct unknown ()) ; used in the field elements-in-order?
(define the-unknown-value (unknown))

; We represent both generalized and specialized arrays with the same structure.
; We add a printer. Arrays with a volume less than  current-max-array-print-volume
; will display the elements. 

(define current-max-array-print-volume (make-parameter 100))

(define (array-print array port mode)
  (define print (if mode write display))
  (define dom   (array-domain array))
  (print 
   (if (< (interval-volume dom) (current-max-array-print-volume))
       ; constructor style printing:
       (list 'array: 
             (array->lists array))
       ; omit actual elements
       (list 'array "..."))
   port))

(struct array (domain                          ; an interval
               getter                          ; a map from interval to elements
               [setter #:mutable]              ; #f means immutable
               storage-class                   ; for specialized arrays: the storage class
               body                            ; for specialized arrays: the backing store
               indexer                         ; for specialized arrays: affine map from indices to index
               safe?                           ; for specialized arrays: boolean
               [elements-in-order? #:mutable]) ; for specialized arrays: boolean or the-unknown value
  ; #:transparent
  #:methods gen:custom-write 
  [(define write-proc array-print)]
  )

; mutable-array? : array -> boolean
(define (mutable-array? array)
  (and (array? array)
       (array-setter array)
       #t))

; specialized-array? : array -> boolean
(define (specialized-array? array)
  (and (array? array)
       (array-body array)
       #t))

(define/contract (safe-array-body array)
  (-> specialized-array? any)
  (array-body array))

(define/contract (safe-array-indexer array)
  (-> specialized-array? any)
  (array-indexer array))

(define/contract (safe-array-safe? array)
  (-> specialized-array? any)
  (array-safe? array))

(define/contract (safe-array-elements-in-order? array)
  (-> specialized-array? any)
  (define order? (array-elements-in-order? array))
  (case order?
    [(#t #f) order?]
    [else (safe-compute-array-elements-in-order? array)]))

(define/contract (safe-array-storage-class array)
  (-> specialized-array? any)
  (array-storage-class array))

; make-array : interval? procedure [procedure-or-#f] -> array
;   Make a generalized array. 
(define (make-array interval getter [setter #f])
  (define who 'make-array)
  (unless (interval? interval)
    (raise-argument-error who "non-empty interval" 0 interval getter setter))
  (unless (procedure? getter)
    (raise-argument-error who "getter procedure" 1 interval getter setter))
  (unless (or (not setter) (procedure? setter))
    (raise-argument-error who "setter procedure" 2 interval getter setter))

  (array interval getter setter
         ; the values for specialized arrays are all false
         #f #f #f #f
         the-unknown-value))

(define/contract (safe-array-dimension array)
  (-> array? any)
  (array-dimension array))

; array-dimension : array -> natural
;   Return the dimension of the array.
;   The dimension of an array is the dimension of the interval.
(define (array-dimension array)
  (interval-dimension (array-domain array)))


;;;
;;; Indexers
;;;

; Specialized arrays store the elements in a vector-like backing store.
; The backing store is 1-dimensional, so an map from the interval indices
; to the store index is needed. Such a map is called an *indexer*.

; Formally an indexer is an affine 1-1 function from the domain (interval)
; into [0,<dimension_of_body>[. 

; The indexers are called frequently, so it is important that they are fast.
; Here we make specialized versions from 1- and 2-dimensional arrays.
; TODO: implement a version for 3-dimensional arrays as well.


; An indexer for a 1-dimensional interval into a backing storage.
;   start  = index into backing storage (not always 0)
;   l      = lower bound for in interval
;   stride = amount to add to get to next element
(define (make-indexer-1-dim start l stride)
  ; The general case is:
  ;     (λ (i) (+ start (* (- i l) stride)))
  ; Since the indexer must be fast, let's specialize.
  (case start
    [(0) (case l
           [(0) (case stride
                  ; (+ start (* (- i l) stride)) = i, when stride=1
                  [(1)  (λ (i) i)]
                  ; since start is 0, the stride -1 doesn't make sense here
                  [else (λ (i) (* i stride))])]
           [else (case stride
                   [(1)  (λ (i) (- i l))]
                   [else (λ (i) (* (- i l) stride))])])]
    [else (case l
           [(0) (case stride
                  [( 1)  (λ (i) (+ start i))]
                  [(-1)  (λ (i) (- start i))]                  
                  [else  (λ (i) (+ start (* i stride)))])]
           [else (case stride
                   [( 1)  (λ (i) (+ start (- i l)))]
                   [(-1)  (λ (i) (+ start (- l i)))]
                   [else  (λ (i) (+ start (* (- i l) stride)))])])]))

; todo: write a randomizing tester for this function
(define (make-indexer-2-dim start l0 l1 stride0 stride1)
  ; The general case is:
  #;(λ (i j)  (+ start
               (* (- i l0) stride0) 
               (* (- j l1) stride1)))
  ; Since the indexer must be fast, let's specialize.
  (case start
    [(0) (case l0
           [(0) (case l1
                  [(0) (case stride0
                         [(1)  (case stride1
                                 [(1)  (λ (i j) (+ i j))]
                                 [(-1) (λ (i j) (- i j))]
                                 [else (λ (i j) (+ i (* j stride1)))])]
                         [(-1) (case stride1
                                 [(1)  (λ (i j) (- j i))]
                                 [(-1) (λ (i j) (- (+ i j)))]
                                 [else (λ (i j) (- (* j stride1) i))])]
                         [else (case stride1
                                 [(1)  (λ (i j) (+ (* i stride0) j))]
                                 [(-1) (λ (i j) (- (* i stride0) j))]
                                 [else (λ (i j) (+ (* i stride0) (* j stride1)))])])]
                  [else (case stride0
                          [(1)  (case stride1
                                  [(1)  (λ (i j) (+ i (- j l1)))]
                                  [(-1) (λ (i j) (- i (- j l1)))]
                                  [else (λ (i j) (+ i (* (- j l1) stride1)))])]
                          [(-1) (case stride1
                                  [(1)  (λ (i j) (- (- j l1) i))]
                                  [(-1) (λ (i j) (- (+ i (- j l1))))]
                                  [else (λ (i j) (- (* (- j l1) stride1) i))])]
                          [else (case stride1
                                  [(1)  (λ (i j) (+ (* i stride0) (- j l1)))]
                                  [(-1) (λ (i j) (- (* i stride0) (- j l1)))]
                                  [else (λ (i j) (+ (* i stride0) (* (- j l1) stride1)))])])])]
           [else (case l1
                  [(0) (case stride0
                         [(1)  (case stride1
                                 [(1)  (λ (i j) (+ (- i l0) j))]
                                 [(-1) (λ (i j) (- (- i l0) j))]
                                 [else (λ (i j) (+ (- i l0) (* j stride1)))])]
                         [(-1) (case stride1
                                 [(1)  (λ (i j) (- j (- i l0)))]
                                 [(-1) (λ (i j) (- (+ (- i l0) j)))]
                                 [else (λ (i j) (- (* j stride1) (- i l0)))])]
                         [else (case stride1
                                 [(1)  (λ (i j) (+ (* (- i l0) stride0) j))]
                                 [(-1) (λ (i j) (- (* (- i l0) stride0) j))]
                                 [else (λ (i j) (+ (* (- i l0) stride0) (* j stride1)))])])]
                  [else (case stride0
                          [(1)  (case stride1
                                  [(1)  (λ (i j) (+ (- i l0) (- j l1)))]
                                  [(-1) (λ (i j) (- (- i l0) (- j l1)))]
                                  [else (λ (i j) (+ (- i l0) (* (- j l1) stride1)))])]
                          [(-1) (case stride1
                                  [(1)  (λ (i j) (- (- j l1) (- i l0)))]
                                  [(-1) (λ (i j) (- (+ (- i l0) (- j l1))))]
                                  [else (λ (i j) (- (* (- j l1) stride1) (- i l0)))])]
                          [else (case stride1
                                  [(1)  (λ (i j) (+ (* (- i l0) stride0) (- j l1)))]
                                  [(-1) (λ (i j) (- (* (- i l0) stride0) (- j l1)))]
                                  [else (λ (i j) (+ (* (- i l0) stride0) (* (- j l1) stride1)))])])])])]
    [else (case l0
           [(0) (case l1
                  [(0) (case stride0
                         [(1)  (case stride1
                                 [(1)  (λ (i j) (+ start (+ i j)))]
                                 [(-1) (λ (i j) (+ start (- i j)))]
                                 [else (λ (i j) (+ start (+ i (* j stride1))))])]
                         [(-1) (case stride1
                                 [(1)  (λ (i j) (+ start (- j i)))]
                                 [(-1) (λ (i j) (+ start (- (+ i j))))]
                                 [else (λ (i j) (+ start (- (* j stride1) i)))])]
                         [else (case stride1
                                 [(1)  (λ (i j) (+ start (+ (* i stride0) j)))]
                                 [(-1) (λ (i j) (+ start (- (* i stride0) j)))]
                                 [else (λ (i j) (+ start (+ (* i stride0) (* j stride1))))])])]
                  [else (case stride0
                          [(1)  (case stride1
                                  [(1)  (λ (i j) (+ start (+ i (- j l1))))]
                                  [(-1) (λ (i j) (+ start (- i (- j l1))))]
                                  [else (λ (i j) (+ start (+ i (* (- j l1) stride1))))])]
                          [(-1) (case stride1
                                  [(1)  (λ (i j) (+ start (- (- j l1) i)))]
                                  [(-1) (λ (i j) (+ start (- (+ i (- j l1)))))]
                                  [else (λ (i j) (+ start (- (* (- j l1) stride1) i)))])]
                          [else (case stride1
                                  [(1)  (λ (i j) (+ start (+ (* i stride0) (- j l1))))]
                                  [(-1) (λ (i j) (+ start (- (* i stride0) (- j l1))))]
                                  [else (λ (i j) (+ start (+ (* i stride0) (* (- j l1) stride1))))])])])]
           [else (case l1
                  [(0) (case stride0
                         [(1)  (case stride1
                                 [(1)  (λ (i j) (+ start (+ (- i l0) j)))]
                                 [(-1) (λ (i j) (+ start (- (- i l0) j)))]
                                 [else (λ (i j) (+ start (+ (- i l0) (* j stride1))))])]
                         [(-1) (case stride1
                                 [(1)  (λ (i j) (+ start (- j (- i l0))))]
                                 [(-1) (λ (i j) (+ start (- (+ (- i l0) j))))]
                                 [else (λ (i j) (+ start (- (* j stride1) (- i l0))))])]
                         [else (case stride1
                                 [(1)  (λ (i j) (+ start (+ (* (- i l0) stride0) j)))]
                                 [(-1) (λ (i j) (+ start (- (* (- i l0) stride0) j)))]
                                 [else (λ (i j) (+ start (+ (* (- i l0) stride0) (* j stride1))))])])]
                  [else (case stride0
                          [(1)  (case stride1
                                  [(1)  (λ (i j) (+ start (+ (- i l0) (- j l1))))]
                                  [(-1) (λ (i j) (+ start (- (- i l0) (- j l1))))]
                                  [else (λ (i j) (+ start (+ (- i l0) (* (- j l1) stride1))))])]
                          [(-1) (case stride1
                                  [(1)  (λ (i j) (+ start (- (- j l1) (- i l0))))]
                                  [(-1) (λ (i j) (+ start (- (+ (- i l0) (- j l1)))))]
                                  [else (λ (i j) (+ start (- (* (- j l1) stride1) (- i l0))))])]
                          [else (case stride1
                                  [(1)  (λ (i j) (+ start (+ (* (- i l0) stride0) (- j l1))))]
                                  [(-1) (λ (i j) (+ start (- (* (- i l0) stride0) (- j l1))))]
                                  [else (λ (i j) (+ start (+ (* (- i l0) stride0) (* (- j l1) stride1))))])])])])]))

(define (make-indexer-n-dim start ls strides)
  (λ is
    (for/fold ([index start])
              ([i (in-list is)]
               [l (in-vector ls)]
               [s (in-vector strides)])
      (+ index (* (- i l) s)))))

(define (make-indexer start ls strides)
  (case (vector-length ls)
    [(1)  (make-indexer-1-dim start (vector-ref ls 0) (vector-ref strides 0))]
    [(2)  (make-indexer-2-dim start
                              (vector-ref ls      0) (vector-ref ls      1)
                              (vector-ref strides 0) (vector-ref strides 1))]
    [else (make-indexer-n-dim start ls strides)]))

(define (interval-strides interval)
  (define ls       (interval-ls interval))
  (define us       (interval-us interval))
  (define d        (vector-length ls))

  (define diffs   (for/list ([l (in-vector ls)] [u (in-vector us)]) (- u l)))

  (list->vector
   (let loop ([diffs (reverse diffs)]
              [strides '()]
              [stride 1])
     (cond
       [(null? diffs) strides]
       [else          (define diff (car diffs))
                      (cond
                        [(= diff 1) (loop (rest diffs)
                                          (cons 0 strides)
                                          stride)]
                        [else       (loop (rest diffs)
                                          (cons stride strides)
                                          (* diff stride))])]))))

; (interval-strides (make-interval #(5))) '(1)
; (interval-strides (make-interval #(5 10))) '(10 1)
; arrays.rkt> (interval-strides (make-interval #(10 5)))
; '(5 1)
; (interval-strides (make-interval #(1)))
; '(0)

;; arrays.rkt> (interval-strides (make-interval #(5 5 1)))
;; '(5 1 0)
;; arrays.rkt> (interval-strides (make-interval #(5 1 5)))
;; '(5 0 1)
;; arrays.rkt> (interval-strides (make-interval #(5 1 1)))
;; '(1 0 0)


; vector-reverse! : vector -> void
;   Reverse the order of the elements in the vector.
(define (vector-reverse! v)
  (define d   (vector-length v))
  (define d-1 (- d 1))
  (for ([i (in-range 0 (quotient d 2))]
        [a (in-vector v)])
    (define j (- d-1 i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j a)))


#;(define (interval-strides interval)
  (define ls       (interval-ls interval))
  (define us       (interval-us interval))
  (define d        (vector-length ls))

  (define stride   1)
  (define strides  (for/vector #:length d
                       ([l (in-vector ls)]
                        [u (in-vector us)])
                     (begin0
                         stride
                       (set! stride (* stride (- u l))))))
  (vector-reverse! strides)
    strides)



; Given a map from one domain (interval) to another, it is more efficient
; to compute a new indexer (and use that indexer repeatedly).

; The situation is as follows:
;     new-domain  ->  old-domain -> store-index
; Note: the store-index is a single natural number.

(define (compose-indexers old-indexer new-domain new-domain->old-domain)
  ; An indexer is an affine map from an interval into [0;n[.
  ; The strategy is to compute the strides as differences between
  ; two neighbour indices (where only one axis is incremented).
  
  (define dim (interval-dimension new-domain))
  (define ls  (interval-ls new-domain))
  (define us  (interval-us new-domain))

  (case dim
    [(1) ; 1-dim-interval -> old-domain -> [0;n[
     (define (new->index i) (call-with-values (λ () (new-domain->old-domain i)) old-indexer))
     (define l0 (vector-ref ls 0))
     (define u0 (vector-ref us 0))
     (define i  (new->index l0))
     (define j  (if (= (- u0 l0) 1) l0 (new->index (+ l0 1))))
     (define start  i)
     (define stride (- j i))
     (make-indexer-1-dim start l0 stride)]
    [(2) ; 2-dim-interval -> old-domain -> [0;n[
     (define (new->index i j) (call-with-values (λ () (new-domain->old-domain i j)) old-indexer))
     (define l0 (vector-ref ls 0))
     (define u0 (vector-ref us 0))
     (define l1 (vector-ref ls 1))
     (define u1 (vector-ref us 1))
     (define i  (new->index l0 l1))
     (define j  (if (= (- u0 l0) 1) l0 (new->index (+ l0 1) l1)))
     (define k  (if (= (- u1 l1) 1) l1 (new->index l0 (+ l1 1))))
     (define start  i)
     (define stride0 (- j i))
     (define stride1 (- k i))
     (make-indexer-2-dim start l0 l1 stride0 stride1)]
    [(3) ; 3-dim-interval -> old-domain -> [0;n[
     (define (new->index i j k) (call-with-values (λ () (new-domain->old-domain i j k)) old-indexer))
     (define l0 (vector-ref ls 0))
     (define u0 (vector-ref us 0))
     (define l1 (vector-ref ls 1))
     (define u1 (vector-ref us 1))
     (define l2 (vector-ref ls 2))
     (define u2 (vector-ref us 2))
     (define i  (new->index l0 l1 l2))
     (define j  (if (= (- u0 l0) 1) l0 (new->index (+ l0 1)     l1       l2)))
     (define k  (if (= (- u1 l1) 1) l1 (new->index    l0     (+ l1 1)    l2)))
     (define l  (if (= (- u2 l2) 1) l2 (new->index    l0        l1    (+ l2 1))))
     (define start  i)
     (define stride0 (- j i))
     (define stride1 (- k i))
     (define stride2 (- l i))
     ; TODO: implement make-indexer-3-dim
     ; (make-indexer-3-dim start l0 l1 l2 stride0 stride1 stride2)
     (make-indexer-n-dim start (vector l0 l1 l2) (vector stride0 stride1 stride2))]
    [(4) ; 4-dim-interval -> old-domain -> [0;n[
     (define (new->index i j k l) (call-with-values (λ () (new-domain->old-domain i j k l)) old-indexer))
     (define l0 (vector-ref ls 0))
     (define u0 (vector-ref us 0))
     (define l1 (vector-ref ls 1))
     (define u1 (vector-ref us 1))
     (define l2 (vector-ref ls 2))
     (define u2 (vector-ref us 2))
     (define l3 (vector-ref ls 3))
     (define u3 (vector-ref us 3))
     (define i  (new->index l0 l1 l2 l3))
     (define j  (if (= (- u0 l0) 1) l0 (new->index (+ l0 1) l1 l2 l3)))
     (define k  (if (= (- u1 l1) 1) l1 (new->index l0 (+ l1 1) l2 l3)))
     (define l  (if (= (- u2 l2) 1) l2 (new->index l0 l1 (+ l2 1) l3)))
     (define m  (if (= (- u3 l3) 1) l3 (new->index l0 l1 l2 (+ l3 1))))
     (define start  i)
     (define stride0 (- j i))
     (define stride1 (- k i))
     (define stride2 (- l i))
     (define stride3 (- m i))
     ; TODO: implement make-indexer-4-dim
     ; (make-indexer-4-dim start l0 l1 l2 stride0 stride1 stride2)
     (make-indexer-n-dim start (vector l0 l1 l2 l3) (vector stride0 stride1 stride2 stride3))]
    [else
     (define (new->index is) (call-with-values (λ () (apply new-domain->old-domain is)) old-indexer))
     (define (increment-nth xs n)
       (let loop ([xs xs] [n n])
         (if (= n 0)
             (cons (+ (car xs) 1) (rest xs))
             (cons (car xs) (loop (cdr xs) (- n 1))))))
     (define lls (vector->list ls))
     (define lus (vector->list ls))
     (define i   (new->index lls))
     (define js  (for/list ([l (in-list lls)]
                            [u (in-list lus)]
                            [n (in-naturals)])
                   (if (= (- u l) 1)
                       l
                       (new->index (increment-nth lls n)))))
     (define strides (for/vector #:length (vector-length ls)
                         ([j (in-list js)]) (- j i)))
     (make-indexer-n-dim i ls strides)]))

  
;;;
;;; Specialized Arrays
;;;

; The default indexer for a specialized array starts with index 0.

; interval->indexer : interval -> indexer
;   Return the indexer that begins with index 0.
(define (interval->indexer interval #:start [start 0]) ; internal
  (define strides (interval-strides interval))
  (make-indexer start (interval-ls interval) strides))


(define/contract (safe-make-specialized-array interval
                                              [storage-class generic-storage-class]
                                              [initial-value 'not-present]
                                              [safe?         'not-present])
  (->* (interval?)                     ; mandatory
       (storage-class? any/c boolean?) ; optional
       any)
  (make-specialized-array interval storage-class initial-value safe?))

(define (check-indices who . is)
  (unless (andmap exact-integer? is)
    (raise-argument-error who "all indices must be exact integers" is)))

(define (check-value-is-storable who checker v)
  (when checker
    (unless (checker v)
      (raise-argument-error who "value not compatible with the backing store" v))))


(define (make-specialized-array interval
                                [storage-class generic-storage-class]
                                [initial-value 'not-present]
                                [safe?         'not-present])
  ; this allows the user to pass #f as the initial value
  (when (eq? initial-value 'not-present)
    (set! initial-value (storage-class-default storage-class)))
  
  ; this allows the user to pass #f as safe?
  (when (eq? safe? 'not-present)
    (set! safe? (specialized-array-default-safe?)))
  
  (define d        (interval-dimension interval))
  (define mutable? (specialized-array-default-mutable?))
  
  (define specialized-getter  (storage-class-getter  storage-class))
  (define specialized-setter  (storage-class-setter  storage-class))
  (define specialized-maker   (storage-class-maker   storage-class))
  (define specialized-checker (storage-class-checker storage-class))
  (define getter             (if safe?
                                 (specialize
                                  d (i j k l) _ 
                                  (λ (_)
                                    (check-indices 'specialized-getter _)
                                    (specialized-getter body (indexer _)))
                                  (λ is
                                    (apply check-indices 'specialized-getter is)
                                    (specialized-getter body (apply indexer is))))
                                 (specialize
                                  d (i j k l) _ 
                                  (λ (_) (specialized-getter body (indexer _)))
                                  (λ is  (specialized-getter body (apply indexer is))))))
  (define setter    (and mutable?
                         (if safe?
                             (specialize 
                              d (i j k l) _ 
                              (λ (v _)
                                (check-indices           'specialized-setter _)
                                (check-value-is-storable 'specialized-setter specialized-checker v)
                                (specialized-setter body (indexer _) v))
                              (λ (v . is)
                                (apply check-indices     'specialized-setter is)
                                (check-value-is-storable 'specialized-setter specialized-checker v)
                                (specialized-setter body (apply indexer is) v)))
                             (specialize 
                              d (i j k l) _ 
                              (λ (v _)    (specialized-setter body (indexer _) v))
                              (λ (v . is) (specialized-setter body (apply indexer is) v))))))
                             
  (define body     (specialized-maker (interval-volume interval)
                                      initial-value))
  (define start    0)
  (define ls       (interval-ls interval))
  (define us       (interval-us interval))

  (define strides  (interval-strides interval))
  (define indexer  (make-indexer start ls strides))

  (define elements-in-order? #t)  

  (array interval
         getter
         setter
         storage-class
         body
         indexer
         safe?
         elements-in-order?))


  ;; Elements of extracted arrays of newly created specialized
  ;; arrays are not in order unless
  ;; (1) the differences in the upper and lower bounds of the
  ;;     first dimensions all equal 1 *and*
  ;; (2) the next dimension doesn't matter *and*
  ;; (3) the upper and lower bounds of the latter dimensions
  ;;     of the original and extracted arrays are the same
  ;; Whew!

(define (compute-array-elements-in-order? domain indexer)
  (define d  (interval-dimension domain))
  (define ls (interval-ls domain))
  (define us (interval-us domain))
  (case d
    [(1) (define l0 (vector-ref ls 0))
         (define u0 (vector-ref us 0))
         (or (= 1 (- u0 l0)) ; fast path for single element axis
             ; check that the indexer jumps with 1
             (= 1 (- (indexer (+ l0 1)) (indexer l0))))]
    [(2) (define l0 (vector-ref ls 0))
         (define l1 (vector-ref ls 1))
         (define u0 (vector-ref us 0))
         (define u1 (vector-ref us 1))
         ; last dimension
         (and (or (= 1 (- u1 l1)) ; fast path 
                  (= 1 (- (indexer l0 (+ l1 1))
                          (indexer l0    l1))))
              ; dimension before
              (let ()
                ; the stride, when the elements are order
                (define stride (- u1 l1))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u0 l0))
                    (= stride  (- (indexer (+ l0 1) l1)
                                  (indexer    l0    l1))))))]
    [(3) (define l0 (vector-ref ls 0))
         (define l1 (vector-ref ls 1))
         (define l2 (vector-ref ls 2))
         (define u0 (vector-ref us 0))
         (define u1 (vector-ref us 1))
         (define u2 (vector-ref us 2))
         ; last dimension
         (and (or (= 1 (- u2 l2)) ; fast path 
                  (= 1 (- (indexer l0 l1 (+ l2 1))
                          (indexer l0 l1    l2))))
              ; middle dimension
              (let ()
                ; the stride, when the elements are order
                (define stride (- u2 l2))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u1 l1))
                    (= stride  (- (indexer l0 (+ l1 1) l2)
                                  (indexer l0    l1    l2)))))
              ; first dimension
              (let ()
                ; the stride, when the elements are order
                (define stride (* (- u2 l2) (- u1 l1)))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u1 l1))
                    (= stride  (- (indexer (+ l0 1) l1 l2)
                                  (indexer    l0    l1 l2))))))]
    [(4) (define l0 (vector-ref ls 0))
         (define l1 (vector-ref ls 1))
         (define l2 (vector-ref ls 2))
         (define l3 (vector-ref ls 3))
         (define u0 (vector-ref us 0))
         (define u1 (vector-ref us 1))
         (define u2 (vector-ref us 2))
         (define u3 (vector-ref us 3))
         ; last dimension
         (and (or (= 1 (- u3 l3)) ; fast path 
                  (= 1 (- (indexer l0 l1 l2 (+ l3 1))
                          (indexer l0 l1 l2    l3))))
              ; middle dimension
              (let ()
                ; the stride, when the elements are order
                (define stride (- u3 l3))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u2 l2))
                    (= stride  (- (indexer l0 l1 (+ l2 1) l3)
                                  (indexer l0 l1    l2    l3)))))
              ; middle dimension
              (let ()
                ; the stride, when the elements are order
                (define stride (* (- u3 l3) (- u2 l2)))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u1 l1))
                    (= stride  (- (indexer l0 (+ l1 1) l2 l3)
                                  (indexer l0    l1    l2 l3)))))
              ; first dimension
              (let ()
                ; the stride, when the elements are order
                (define stride (* (- u3 l3) (- u2 l2) (- u1 l1)))
                ; check that the indexer jumps like the stride
                (or (= 1       (- u0 l0))
                    (= stride  (- (indexer (+ l0 1) l1 l2 l3)
                                  (indexer    l0    l1 l2 l3))))))]
    [else
     (define n (vector-length ls))
     (define ls+1 (vector-copy ls))
     (define (increment-axis-index v i)
       (define vi (vector-ref v i))
       (vector-set! v i (+ vi 1))
       v)
     (define (decrement-axis-index v i)
       (define vi (vector-ref v i))
       (vector-set! v i (- vi 1))
       v)     
     (let loop ([i 0] [stride 1])
       (define l (vector-ref ls i))
       (define u (vector-ref us i))
       (define vls (vector->list ls))
       (or (= i n)
           (and (or (= 1 (- u l)) ; fast path 
                    (= stride (- (apply indexer (begin0
                                                    (vector->list (increment-axis-index ls+1 i))
                                                    (decrement-axis-index ls+1 i))) ; add 1 to l_i
                                 (apply indexer vls))))
                (loop (+ i 1) (* stride (- u l))))))]))

(define (safe-compute-array-elements-in-order? array)
  (define cached (array-elements-in-order? array))
  (case (array-elements-in-order? array)
    [(#t) #t]
    [(#f) #f]
    [else
     ; compute the result and cache it
     (define domain  (array-domain  array))
     (define indexer (array-indexer array))
     (define order?  (compute-array-elements-in-order? domain indexer))
     (set-array-elements-in-order?! array order?)
     order?]))

(define/contract (safe-specialized-array-share old-array new-domain new-domain->old-domain)
  (-> array? interval? procedure?
      array?)
  (specialized-array-share old-array new-domain new-domain->old-domain))
  
  
(define (specialized-array-share old-array new-domain new-domain->old-domain)
  ; The new array shares the backing storage with the old array old-array.
  ; Note: new-domain->old-domain is supposed to be affine.

  (define d        (interval-dimension (array-domain old-array)))
  (define new-d    (interval-dimension new-domain))
  (define mutable? (specialized-array-default-mutable?))

  (define storage-class (array-storage-class old-array))

  (define old-getter    (storage-class-getter storage-class))
  (define old-setter    (storage-class-setter storage-class))
  (define old-indexer   (array-indexer old-array)) 

  (define indexer
    (compose-indexers old-indexer new-domain new-domain->old-domain))

  (define gettery  (specialize new-d (i j k l) _
                               (λ (_) (old-getter body (indexer _)))
                               (λ is  (old-getter body (apply indexer is)))))
  (define settery   (and mutable?
                        (specialize new-d (i j k l) _
                                    (λ (v _) (old-setter body (indexer _) v))
                                    (λ (v . is)
                                      #;(when (negative? (apply indexer is))
                                          (displayln (list 'old-array old-array))
                                          (displayln (list 'new-domain new-domain)))
                                      #;(displayln (list 'settery 'is is 'index (apply indexer is)))
                                      (old-setter body (apply indexer is) v)))))
  
  (define old-order?    (array-elements-in-order? old-array))
  (define body          (array-body old-array))
  (define safe?         (specialized-array-default-safe?))

  
  (define elements-in-order? the-unknown-value) ; postpone the computation
  
  (array new-domain
         gettery
         settery
         storage-class
         body
         indexer
         safe?
         elements-in-order?))

(define (array-curry old-array inner-dimension)
  (define d           inner-dimension)
  (define old-dim     (array-dimension old-array))
  (define old-indexer (array-indexer   old-array))
  (define old-domain  (array-domain    old-array))
  (define old-getter  (array-getter    old-array))
  (define old-setter  (array-setter    old-array))
  
  (unless (< 0 inner-dimension old-dim)
    (raise-arguments-error
     'array-curry
     "the inner dimension must be at least 1 and at most one less than the old dimension"
     "old   dimension" old-dim
     "inner dimension" inner-dimension))

  ; domain
  (define-values (outer-domain inner-domain) (interval-projections old-domain d))

  (make-array outer-domain
              ; getter (which returns a sub-array)
              (λ outer-is
                ; (displayln (list (specialized-array? old-array) (mutable-array? old-array)))
                (cond
                  [(specialized-array? old-array)
                   ; the inner arrays are specialized too
                   ; => use specialized-array-share
                   (specialized-array-share
                    old-array inner-domain
                    ; new-domain->old-domain
                    ; todo: specialize this
                    (λ inner-is
                      ; (displayln (list 'curry 'inner inner-is 'outer outer-is))
                      (apply values
                             (append outer-is inner-is))))]
                  ; if the old array is mutable, so are the sub-arrays
                  [(mutable-array? old-array)
                   ; todo: specialize this
                   (make-array inner-domain
                               (λ inner-is
                                 (apply old-getter   (append outer-is inner-is)))
                               (λ (v . inner-is)
                                 ; (displayln (list 'v v 'inner-is inner-is 'outer-is outer-is))
                                 ; (displayln (list 'old-setter old-setter))
                                 (apply old-setter v (append outer-is inner-is))))]
                  ; the old array is immutable
                  [else
                   ; todo: specialize this
                   (make-array inner-domain
                               (λ inner-is
                                 (apply old-getter   (append outer-is inner-is))))]))))

(define/contract (safe-array-extract old-array new-domain)
  (-> array? interval?
      array?)
  (array-extract old-array new-domain))

; array-extract : array interval -> interval
;   Return a "sub array" of array with the same elements as old-array,
;   but restricted to new-domain. The new domain must be a subset
;   of the old domain.
(define (array-extract old-array new-domain)
  (define old-domain (array-domain old-array))
  (define old-getter (array-getter old-array))
  (define old-setter (array-setter old-array))
  
  (unless (and (interval? new-domain)
               (interval-subset? new-domain old-domain))
    (raise-argument-error
     'array-extract
     "the new domain must be a subinterval of the array domain"
     "array domain" old-domain
     "new domain"   new-domain))

  (cond
    [(specialized-array? old-array)
     (specialized-array-share old-array new-domain values)]
    [(mutable-array? old-array)
     (make-array new-domain old-getter old-setter)]
    [else
     (make-array new-domain old-getter)]))

(define/contract (safe-array-tile old-array sizes)
  (-> array? scales-vector?
      array?)
  (array-tile old-array sizes))

; todo: specialize array-tile
(define (array-tile old-array sizes)
  ; Assume that A is an array and S is a vector of positive, exact integers.
  ; The routine array-tile returns a new immutable array T, each entry of
  ; which is a subarray of A whose domain has sidelengths given (mostly)
  ; by the entries of S. These subarrays completely "tile" A, in the sense
  ; that every entry in A is an entry of precisely one entry of the result T.

  (define d           (array-dimension old-array))

  (unless (= (vector-length sizes) d)
    (raise-arguments-error 'array-tile
                           "the vector of sizes must have the same dimension as the array"
                           "array" old-array
                           "sizes" sizes))
  
  (define old-indexer (array-indexer   old-array))
  (define old-domain  (array-domain    old-array))
  (define old-getter  (array-getter    old-array))

  (define old-ls (interval-ls old-domain))
  (define old-us (interval-us old-domain))

  ; the outer domain
  (define ls (make-vector d 0))
  (define us (for/vector #:length d
                 ([l (in-vector old-ls)]
                  [u (in-vector old-us)]
                  [s (in-vector sizes)])
               (inexact->exact (ceiling (/ (- u l) s)))))
  (define new-domain (interval d ls us))
  
  (make-array new-domain
              (λ is
                (unless (apply interval-contains-multi-index? new-domain is)
                  (raise-arguments-error 'tiled-getter "indices is not in the domain"
                                         "domain" new-domain
                                         "indices" is))
                ; we must extract the proper block using array-extract
                (define ls (for/vector #:length d
                               ([l (in-vector old-ls)]
                                [s (in-vector sizes)]
                                [i (in-list is)])
                             (+ l (* i s))))
                (define us (for/vector #:length d
                               ([l (in-vector ls)]
                                [u (in-vector old-us)]
                                [s (in-vector sizes)])
                             (min (+ l s) u)))
                ; this shares elements
                (array-extract old-array (make-interval ls us)))))

(define (array-translate old-array translation)
  (define who 'array-translate)
  (unless (array? old-array)
    (raise-argument-error
     who
     "the first argument must be an array"
     "array" old-array))
  
  (define d (array-dimension old-array))
  
  (unless (and (vector? translation)
               (= d (vector-length translation)))
    (raise-argument-error
     who
     "the translation must be a vector of the same dimension as the array"
     "translation" translation))

  (define old-domain  (array-domain  old-array))  
  (define new-domain  (interval-translate old-domain translation))
  (define ts          (vector->list translation))
  (define new->old    (λ is (map - is ts)))
  (define new->old*   (λ is (apply values (map - is ts))))
  ;; (define old-indexer (array-indexer old-array))
  ;; (define indexer     (compose-indexers old-indexer new-domain new->old))
  
  (cond
    [(specialized-array? old-array)
     (specialized-array-share old-array new-domain new->old*)] ; composes
    [(mutable-array? old-array)
     (define old-getter (array-getter old-array))
     (define old-setter (array-setter old-array))
     (make-array new-domain
                 (λ is         (apply old-getter     (apply new->old is)))
                 (λ (val . is) (apply old-setter val (apply new->old is))))]
    [else
     (define old-getter (array-getter old-array))
     (make-array new-domain
                 (λ is (apply old-getter (apply new->old is))))]))

(define (permutation-inverse permutation)
  (define inverse (make-vector (vector-length permutation) #f))
  (for ([i (in-naturals)]
        [x (in-vector permutation)])
    (vector-set! inverse x i))
  inverse)

(define (vector-permute vector permutation)
  (define d (vector-length vector))
  (for/vector #:length d
      ([i (in-vector permutation)])
    (vector-ref vector i)))


(define (array-permute old-array permutation)
  ; todo: compose index transformation
  (define who 'array-permutation)
  (unless (array? old-array)
    (raise-argument-error
     who
     "the first argument must be an array"
     "array" old-array))
  
  (define d (array-dimension old-array))
  
  (unless (and (vector? permutation)
               (= d (vector-length permutation)))
    (raise-argument-error
     who
     "the permutation must be a vector of the same dimension as the array"
     "permutation" permutation))

  (define old-domain (array-domain old-array))  
  (define new-domain (interval-permute old-domain permutation))
  (define π permutation)

  (define (π-inv is)
    (vector->list
     (vector-permute
      (list->vector is) π)))
  
  (cond
    [(specialized-array? old-array)
     (specialized-array-share old-array
                              (interval-permute old-domain π)
                              (λ is (apply values (π-inv is))))]
    [(mutable-array? old-array)
     (define old-getter (array-getter old-array))
     (define old-setter (array-setter old-array))
     (make-array new-domain
                 (λ is         (apply old-getter     (π-inv is)))
                 (λ (val . is) (apply old-setter val (π-inv is))))]
    [else
     (define old-getter (array-getter old-array))
     (make-array new-domain
                 (λ is         (apply old-getter     (π-inv is))))]))

(define (rotation-permutation d dim)
  ; rotate dim places to the left
  (define π (make-vector d #f))
  (for ([i (in-naturals)]
        [j (in-range dim d)])
    (vector-set! π i j))
  (for ([i (in-range (- d dim) d)]
        [j (in-range 0 d)])
    (vector-set! π i j))
  π)

(define (array-rotate old-array dim)
  (define d (array-dimension old-array))
  (define π (rotation-permutation d dim))
  (array-permute old-array π))

(define/contract (safe-array-reverse old-array [flips #f])
  (->* (array?)                        ; mandatory
       ((or/c #f (vectorof boolean?))) ; optional
       array?)                         ; result
  (array-reverse old-array flips))

(define (array-reverse old-array [flips #f])
  (define who 'array-reverse)
  (define d (array-dimension old-array))
  (when (vector? flips)
    (unless (= (vector-length flips) d)
      (raise-argument-error
       who
       "the vector of flips must have the same length as the array dimension"
       "flips" flips)))
  (unless flips
    (set! flips (make-vector d #t)))

  (define old-domain (array-domain old-array))
  (define ls (interval-ls old-domain))
  (define us (interval-us old-domain))

  (define (flip-indices is)
    ; (displayln (list 'flip-indices 'is is 'flips flips 'ls ls 'us us))
    (for/list ([i (in-list   is)]
               [f (in-vector flips)]
               [l (in-vector ls)]
               [u (in-vector us)])
      (if f
          (- (+ l u -1) i)
          i)))

  (define old-getter (array-getter old-array))
  (define old-setter (array-setter old-array))
  
  (cond
    [(specialized-array? old-array)
     (specialized-array-share
      old-array old-domain
      ; new->old
      (λ is (apply values (flip-indices is))))]
    [(mutable-array? old-array)
     (make-array old-domain
                 (λ is       (apply old-getter   (flip-indices is)))
                 (λ (v . is) (apply old-setter v (flip-indices is))))]
    [else
     (make-array old-domain
                 (λ is       (apply old-getter (flip-indices is))))]))

(define/contract (safe-array-sample old-array vec-scales)
  (-> array? scales-vector?
      array?)
  (array-sample old-array vec-scales))
  
(define (array-sample old-array vec-scales)
  ; all lower bounds are zero
  ; scales is a vector of positive, exact integers
  (define old-domain (array-domain old-array))
  (define old-getter (array-getter old-array))
  (define old-setter (array-setter old-array))
  (define scales (vector->list vec-scales))
  (cond
    [(specialized-array? old-array)
     (define new-domain (interval-scale old-domain vec-scales))
     (specialized-array-share old-array new-domain                              
                              (λ is (apply values (map * is scales))))]
    [(mutable-array? old-array)
     (make-array (interval-scale old-domain vec-scales)
                 (λ is       (apply old-getter   (map * is scales)))
                 (λ (v . is) (apply old-setter v (map * is scales))))]
    [else
     (make-array (interval-scale old-domain vec-scales)
                 (λ is       (apply old-getter   (map * is scales)))
                 (λ (v . is) (apply old-setter v (map * is scales))))]))


(define/contract (safe-array-outer-product op array1 array2)
  (-> procedure? array? array?
      array?)
  (array-outer-product op array1 array2))

; array-outer-product : procedure? array1 array2 -> array
;   Note: See perfomance note in the srfi.
;         In short: If array1 is expensive to compute,
;         use array-copy before calling array-outer-product.
(define (array-outer-product op array1 array2)
  (define dom1 (array-domain array1))
  (define dom2 (array-domain array2))
  (define get1 (array-getter array1))
  (define get2 (array-getter array2))
  (define d1   (array-dimension array1))
  (define d2   (array-dimension array2))
  
  (make-array (interval-cartesian-product dom1 dom2)
              (λ is
                (op (apply get1 (take is d1))
                    (apply get2 (drop is d1))))))


(define/contract (safe-index-rotate n k)
  (-> integer? integer?
      vector)
  (index-rotate n k))
  
; index-rotate : integer integer -> vector
;   (index-rotate 5 0)  = '#(0 1 2 3 4)
;   (index-rotate 5 1)  = '#(1 2 3 4 0)
;   (index-rotate 5 2)  = '#(2 3 4 0 1)
(define (index-rotate n k)
  (unless (<= 0 k (- n 1))
    (raise-arguments-error 'index-rotate "0≤k<n" "n" n "k" k))

  (define v (for/vector #:length n ([x (in-range k n)]) x))
  (for ([x (in-range 0 k)]
        [i (in-naturals (- n k))])
    (vector-set! v i x))
  v)


(define/contract (safe-index-first n k)
  (-> exact-integer? exact-integer?
      vector)
  (index-first n k))

; index-first : integer integer -> vector
;   An n-dimensional index with k first.
;   (index-first 5 0)  =  '#(0 1 2 3 4)
;   (index-first 5 1)  =  '#(1 0 2 3 4)
;   (index-first 5 2)  =  '#(2 0 1 3 4)
;   (index-first 5 3)  =  '#(3 0 1 2 4)
;   (index-first 5 4)  =  '#(4 0 1 2 3)
(define (index-first n k)
  (unless (<= 0 k (- n 1))
    (raise-arguments-error 'index-first "0≤k<n" "n" n "k" k))

  (define v (make-vector n 5))
  (vector-set! v 0 k)
  (for ([i (in-range 1 (+ k 1))])
    (vector-set! v i (- i 1)))
  (for ([i (in-range (+ k 1) n)])
    (vector-set! v i i))
  v)  


(define/contract (safe-index-last n k)
  (-> exact-integer? exact-integer?
      vector)
  (index-last n k))

; (index-last 5 0)  =  '#(1 2 3 4 0)
; (index-last 5 1)  =  '#(0 2 3 4 1)
; (index-last 5 2)  =  '#(0 1 3 4 2)

(define (index-last n k)
  (unless (<= 0 k (- n 1))
    (raise-arguments-error 'index-last "0≤k<n" "n" n "k" k))

  (define v (make-vector n 5))
  (vector-set! v (- n 1) k)
  (for ([i (in-range 0 k)])
    (vector-set! v i i))
  (for ([i (in-range k (- n 1))])
    (vector-set! v i (+ i 1)))
  v)

(define/contract (safe-array-inner-product A f g B)
  (-> array? procedure? procedure? array?
      array?)
  (array-inner-product A f g B))

(define (array-inner-product A f g B)
  ; todo: this is the naive version - implement a more efficent one
  ;       without calling array-outer-product
  (define dom-A (array-domain A))
  (define dom-B (array-domain B))
  (define dA    (array-dimension A))
  (define boundsA (list (vector-ref (interval-ls dom-A) (- dA 1))
                        (vector-ref (interval-us dom-A) (- dA 1))))
  (define boundsB (list (vector-ref (interval-ls dom-B) 0)
                        (vector-ref (interval-us dom-B) 0)))
  (unless (equal? boundsA boundsB)
    (raise-arguments-error 'array-inner-product
                           "the last bounds of the first array must the same as the first bounds of the second array"
                           "last bounds of first array" boundsA
                           "first bounds of last array" boundsB))
  
  (array-outer-product
   (λ (a b) (array-reduce f (array-map g a b)))
   (array-copy (array-curry A 1))
   (array-copy (array-curry (array-permute B (index-rotate (array-dimension B) 1)) 1))))



(define (array-map f array0 . arrays)
  (unless (and (same-domain? (cons array0 arrays))
               (procedure? f))
    (raise-arguments-error 'array-map
                           "all arrays must have the same domain"
                           "arrays" (cons array0 arrays)))
  (define d       (array-dimension array0))
  (define domain  (array-domain array0))
  (case (length arrays)
    [(0)
     (define get0 (array-getter array0))
     (specialize d (i j k l) _
                 (for/array #:domain domain ([(_) (in-interval domain)])
                   (f (get0 _)))
                 (for/array #:domain domain ([is (in-interval/internal-vector domain)])
                   (f (apply get0 (vector->list is)))))]
    [(1)
     (define get0 (array-getter array0))
     (define get1 (array-getter (list-ref arrays 0)))
     (specialize d (i j k l) _
                 (for/array #:domain domain ([(_) (in-interval domain)])
                   (f (get0 _) (get1 _)))
                 (for/array #:domain domain ([is (in-interval/internal-vector domain)])
                   (f (apply get0 (vector->list is))
                      (apply get1 (vector->list is)))))]
    [(2)
     (define get0 (array-getter array0))
     (define get1 (array-getter (list-ref arrays 0)))
     (define get2 (array-getter (list-ref arrays 1)))
     (specialize d (i j k l) _
                 (for/array #:domain domain ([(_) (in-interval domain)])
                   (f (get0 _) (get1 _) (get2 _)))
                 (for/array #:domain domain ([is (in-interval/internal-vector domain)])
                   (f (apply get0 (vector->list is))
                      (apply get1 (vector->list is))
                      (apply get2 (vector->list is)))))]
    [else
     (define getters (map array-getter (cons array0 arrays)))
     (for/array #:domain domain ([is (in-interval/internal-vector domain)])
       (define lis (vector->list is))
       (apply f
              (map (λ (get) (apply get lis))
                   getters)))]))


(define (array-for-each f array0 . arrays)
  (cond
    [(and (same-domain? (cons array0 arrays))
          (procedure? f))
     (define d       (array-dimension array0))
     (define domain  (array-domain array0))
     (case (length arrays)
       [(0)
        (define get0 (array-getter array0))
        (specialize d (i j k l) _
                    (for ([(_) (in-interval domain)])
                      (f (get0 _)))
                    (for ([is (in-interval/internal-vector domain)])
                      (f (apply get0 (vector->list is)))))]
       [(1)
        (define get0 (array-getter array0))
        (define get1 (array-getter (list-ref arrays 0)))
        (specialize d (i j k l) _
                    (for ([(_) (in-interval domain)])
                      (f (get0 _) (get1 _)))
                    (for ([is (in-interval/internal-vector domain)])
                      (f (apply get0 (vector->list is))
                         (apply get1 (vector->list is)))))]
       [(2)
        (define get0 (array-getter array0))
        (define get1 (array-getter (list-ref arrays 0)))
        (define get2 (array-getter (list-ref arrays 1)))
        (specialize d (i j k l) _
                    (for ([(_) (in-interval domain)])
                      (f (get0 _) (get1 _) (get2 _)))
                    (for ([is (in-interval/internal-vector domain)])
                      (f (apply get0 (vector->list is))
                         (apply get1 (vector->list is))
                         (apply get2 (vector->list is)))))]       
       [else
        (define getters (map array-getter (cons array0 arrays)))
        (for ([is (in-interval/internal-vector domain)])
          (apply f
                 (map (λ (get) (apply get (vector->list is)))
                      getters)))])]
    [else
     (raise-argument-error 'array-for-each
                           "all arrays must have the same domain"
                           "arrays" (cons array0 arrays))]))

#;(define (array-fold kons knil array)
  (for/fold ([result knil])
            ([x      (in-array array)])
    (kons x knil)))

(define (array-fold kons knil array)
  (define get (array-getter    array))
  (define dom (array-domain    array))
  (define d   (array-dimension array))
  (specialize d (i j k l) _
              (for/fold ([result knil])
                        ([(_)    (in-interval dom)])
                (kons (get _) result))
              (for/fold ([result knil])
                        ([is     (in-interval/internal-vector dom)])
                (define vis (vector->list is))
                (kons (apply get vis) result))))

(define (array-fold-right kons knil array)
  (array-fold kons knil (array-reverse array)))

; These were added in the follow up.
(define (array-foldl xkons knil array) (array-fold (λ (a d) (xkons d a)) knil array))
(define (array-foldr  kons knil array) (array-fold-right kons knil array))



(define (array-reduce op array)
  (define get (array-getter    array))
  (define dom (array-domain    array))
  (define d   (array-dimension array))

  (define start #t)
  (specialize d (i j k l) _
              (for/fold ([result '()])
                        ([(_)    (in-interval dom)])
                (define x (get _))
                (cond
                  [start (set! start #f) x]
                  [else                  (op result x)]))
              (for/fold ([result '()])
                        ([is      (in-interval/internal-vector dom)])
                (define x (apply get (vector->list is)))
                (cond
                  [start (set! start #f) x]
                  [else                  (op result x)]))))


(define (same-domain? arrays)
  (define array0 (first arrays))
  (define dom0 (array-domain array0))
  (andmap (λ (a) (interval= dom0 (array-domain a))) (rest arrays)))

(define (array-any f array0 . arrays)
  (unless (same-domain? (cons array0 arrays))
    (raise-argument-error 'array-any
                          "the arrays must have the same domain"
                          "arrays" (cons array0 arrays)))
  (unless (procedure? f)
    (raise-argument-error 'array-any
                          "the predicate must be a prodecure"
                          "pred" f))

  (define domain (array-domain array0))
  (define getters (map array-getter (cons array0 arrays)))
  (define d       (array-dimension array0))
  (case (length arrays)
    [(0)
     (define get0 (array-getter array0))
     (specialize d (i j k l) _
                 (for*/or ([(_) (in-interval domain)]
                              [x (in-value (get0 _))])
                   (f x))
                 (for*/or ([is (in-interval/internal-vector domain)]
                              [x (in-value (apply get0 (vector->list is)))])
                   (f x)))]
    [(1)
     (define get0 (array-getter array0))
     (define get1 (array-getter (first arrays)))
     (specialize f (i j k l) _
                 (for*/or ([(_) (in-interval domain)]
                              [x (in-value (get0 _))]
                              [y (in-value (get1 _))])
                   (f x y))
                 (for*/or ([is  (in-interval/internal-vector domain)]
                              [vis (in-value (vector->list is))]
                              [x   (in-value (apply get0 vis))]
                              [y   (in-value (apply get1 vis))])
                   (f x y)))]
    [(2)
     (define get0 (array-getter array0))
     (define get1 (array-getter (first arrays)))
     (define get2 (array-getter (second arrays)))
     (specialize f (i j k l) _
                 (for*/or ([(_) (in-interval domain)]
                              [x (in-value (get0 _))]
                              [y (in-value (get1 _))]
                              [z (in-value (get2 _))])
                   (f x y z))
                 (for*/or ([is  (in-interval/internal-vector domain)]
                              [vis (in-value (vector->list is))]
                              [x   (in-value (apply get0 vis))]
                              [y   (in-value (apply get1 vis))]
                              [z   (in-value (apply get2 vis))])
                   (f x y z)))]
    [else
     (define getters (apply array-getter (cons array0 arrays)))
     (for*/or ([is  (in-interval/internal-vector domain)]
               [vis (vector->list is)])
       (apply f
              (map (λ (get) (apply get vis))
                   getters)))]))

(define (array-every f array0 . arrays)
  (unless (same-domain? (cons array0 arrays))
    (raise-argument-error 'array-every
                          "the arrays must have the same domain"
                          "arrays" (cons array0 arrays)))
  (unless (procedure? f)
    (raise-argument-error 'array-every
                          "the predicate must be a prodecure"
                          "pred" f))


  (define domain  (array-domain array0))
  (define getters (map array-getter (cons array0 arrays)))
  (define d       (interval-dimension domain))
              
  (case (length arrays)
    [(0)
     (define get0 (array-getter array0))
     (specialize d (i j k l) _
                 (for*/and ([(_) (in-interval domain)]
                            [x   (in-value (get0 _))])
                   (f x))
                 (for*/and ([is (in-interval/internal-vector domain)]
                            [x  (in-value (apply get0 (vector->list is)))])
                   (f x)))]
    [(1)
     (define get0 (array-getter array0))
     (define get1 (array-getter (first arrays)))
     (specialize d (i j k l) _
                 (for*/and ([(_) (in-interval domain)]
                            [x   (in-value (get0 _))]
                            [y   (in-value (get1 _))])
                   (f x y))
                 (for*/and ([is  (in-interval/internal-vector domain)]
                            [vis (in-value (vector->list is))]
                            [x   (in-value (apply get0 vis))]
                            [y   (in-value (apply get1 vis))])
                   (f x y)))]
    [(2)
     (define get0 (array-getter array0))
     (define get1 (array-getter (first arrays)))
     (define get2 (array-getter (second arrays)))
     (specialize d (i j k l) _
                 (for*/and ([(_) (in-interval domain)]
                            [x   (in-value (get0 _))]
                            [y   (in-value (get1 _))]
                            [z   (in-value (get2 _))])
                   (f x y z))
                 (for*/and ([is  (in-interval/internal-vector domain)]
                            [vis (in-value (vector->list is))]
                            [x   (in-value (apply get0 vis))]
                            [y   (in-value (apply get1 vis))]
                            [z   (in-value (apply get2 vis))])
                   (f x y z)))]
    [else
     (define getters (apply array-getter (cons array0 arrays)))
     (for*/and ([is  (in-interval/internal-vector domain)]
                [vis (vector->list is)])
       (apply f
              (map (λ (get) (apply get vis))
                   getters)))]))
  
(define array-ref
  (case-lambda
    [(array i)       ((array-getter array) i)]
    [(array i j)     ((array-getter array) i j)]
    [(array i j k)   ((array-getter array) i j k)]
    [(array i j k l) ((array-getter array) i j k l)]
    [(array . is)    (apply (array-getter array) is)]))

(define array-set!
  (case-lambda
    [(array x i)       ((array-setter array) x i)]
    [(array x i j)     ((array-setter array) x i j)]
    [(array x i j k)   ((array-setter array) x i j k)]
    [(array x i j k l) ((array-setter array) x i j k l)]
    [(array x . is)    (apply (array-setter array) x is)]))

(define (array-assign! destination source)
  ; TODO: specialize the moving
  (unless (mutable-array? destination)
    (raise-arguments-error 'array-assign!
                           "the destination must be a mutable array"
                           "destination" destination))
  
  (define src-domain (array-domain source))
  (define dst-domain (array-domain destination))

  ;; The array destination must be compatible with source, in the sense that either
  ;; destination and source have the same domain, or destination is a specialized
  ;; array whose elements are stored adjacently and in order in its body and whose
  ;; domain has the same volume as the domain of source.

  (unless (interval= src-domain dst-domain)
    (raise-arguments-error 'array-assign!
                           "the source and destination domains must be the same"
                           "source     " (array-domain source)
                           "destination" (array-domain destination)))
  
  (define get        (array-getter source))
  (define set        (array-setter destination))

  (for ([is (in-interval/internal-vector src-domain)]
        [js (in-interval/internal-vector dst-domain)])
    (define x (apply get (vector->list is)))
    (apply set x (vector->list js))))

(define (specialized-array-reshape array new-domain [copy-on-failure? #f])
  (error 'specialized-array-reshape "todo: implement it"))
    

; TODO: Benchmark this simple version against the one below.

(define/contract (safe-array->list array)
  (-> array? list?)
  (array->list array))


(define (array->list array)
  (define d   (array-dimension array))
  (define dom (array-domain array))
  (define get (array-getter array))
  (case d
    ;[(1)  (for/list ([i         (in-interval dom)]) (get i))]
    ;[(2)  (for/list ([(i j)     (in-interval dom)]) (get i j))]
    ;[(3)  (for/list ([(i j k)   (in-interval dom)]) (get i j k))]
    ;[(4)  (for/list ([(i j k l) (in-interval dom)]) (get i j k l))]
    [else (for/list ([is (in-interval/internal-vector dom)])
            (apply get (vector->list is)))]))

(define/contract (safe-array->lists array)
  (-> array? list?)
  (array->lists array))

(define (array->lists array)
  (define d   (array-dimension array))
  (define dom (array-domain    array))
  (define get (array-getter    array))
  (define ls  (interval-ls dom))
  (define us  (interval-us dom))
  (case d
    [(1)   (array->list array)]
    [(2)   (for/list ([i (in-range (vector-ref ls 0) (vector-ref us 0))])
             (for/list ([j (in-range (vector-ref ls 1) (vector-ref us 1))])
               (get i j)))]
    [(3)   (for/list ([i (in-range (vector-ref ls 0) (vector-ref us 0))])
             (for/list ([j (in-range (vector-ref ls 1) (vector-ref us 1))])
               (for/list ([k (in-range (vector-ref ls 2) (vector-ref us 2))])
                 (get i j k))))]
    [(4)   (for/list ([i (in-range (vector-ref ls 0) (vector-ref us 0))])
             (for/list ([j (in-range (vector-ref ls 1) (vector-ref us 1))])
               (for/list ([k (in-range (vector-ref ls 2) (vector-ref us 2))])
                 (for/list ([l (in-range (vector-ref ls 3) (vector-ref us 3))])
                   (get i j k l)))))]
    [(5)   (for/list ([i (in-range (vector-ref ls 0) (vector-ref us 0))])
             (for/list ([j (in-range (vector-ref ls 1) (vector-ref us 1))])
               (for/list ([k (in-range (vector-ref ls 2) (vector-ref us 2))])
                 (for/list ([l (in-range (vector-ref ls 3) (vector-ref us 3))])
                   (for/list ([m (in-range (vector-ref ls 4) (vector-ref us 4))])
                   (get i j k l m))))))]
    [else (error 'array->lists "todo: implement the general case")]))


(define/contract (safe-list->array xs domain
                                   [result-storage-class generic-storage-class]
                                   [mutable?             'not-present]
                                   [safe?                'not-present])
  (->* (list? interval?)                  ; mandatory
       (storage-class? boolean? boolean?) ; optional
       array?)
  (list->array xs domain result-storage-class mutable? safe?))

(define (list->array xs domain
                     [result-storage-class generic-storage-class]
                     [mutable?             'not-present]
                     [safe?                'not-present])
  (when (eq? mutable? 'not-present)
    (set! mutable? (specialized-array-default-mutable?)))
  (when (eq? safe? 'not-present)
    (set! safe? (specialized-array-default-safe?)))
  
  (define n (length xs))
  (unless (= n (interval-volume domain))
    (raise-argument-error 'list->array
                          "the length of the list xs must be the same as the volume of the domain"
                          "xs" xs
                          "domain" domain))
  (define initial (storage-class-default result-storage-class))
  (define array   (make-specialized-array domain result-storage-class initial safe?))
  (define body    (array-body array))
  (define set     (storage-class-setter result-storage-class))

  ; Note: storage class could support a list->body 
  
  (for ([x (in-list xs)]
        [i (in-range n)])
    (set body i x))

  (unless mutable?
    (set-array-setter! array #f))
  
  array)

#;(define (vectors->array vs
                        [result-storage-class generic-storage-class]
                        [mutable?             (specialized-array-default-mutable?)]
                        [safe?                (specialized-array-default-safe?)])
  (define (depth vs) (if (vector? vs) (+ 1 (depth (vector-ref vs 0))) 0))
  (define dim (depth vs))
  (define ls (make-vector dim 0))
  (define us (let ([us (make-vector dim 0)])
               (let loop ([vs vs] [i 0])
                 (define v0 (vector-ref vs 0))
                 (when (vector? v0)
                   (vector-set! us i (vector-length v0))
                   (loop v0 (+ i 1))))
               us))
  (define dom (make-interval ls us))
  (for/array #:domain dom
      ([x (in-vectors vs)])
    x))

(define/contract (safe-array-copy
                  old-array
                  [result-storage-class generic-storage-class]
                  ; [new-domain           #f] ; or interval of same volume
                  [mutable?             'not-present]
                  [safe?                'not-present])
  (->* (array?)                                               ; mandatory
       (storage-class? #;(or/c #f interval?) boolean? boolean?) ; optional
       any)  
  (array-copy old-array result-storage-class #;new-domain mutable? safe?))


(define (array-copy old-array
                    [result-storage-class generic-storage-class]
                    ; [new-domain           #f] ; or interval of same volume
                    [mutable?             'not-present]
                    [safe?                'not-present])

  (when (eq? mutable? 'not-present)
    (set! mutable? (if (specialized-array? old-array)
                       (mutable-array? old-array)
                       (specialized-array-default-mutable?))))

  (when (eq? safe?    'not-present)
    (set! safe? (if (specialized-array? old-array)
                    (array-safe? old-array)
                    (specialized-array-default-safe?))))

  
  ; (displayln (list 'array-copy "remove todox"))
  ; Note: new-domain is present in the srfi spec, but in the latest implementation
  ;       array-copy no longer supports new-domain. That functionality has been
  ;       moved to array-reshape.

  ; Note: The copy will always be a specialized array.
  ;       A common use of `array-copy` is to convert a generalized array
  ;       returned by `array-map` into a specialized one.
  
  (define old-domain        (array-domain             old-array))
  (define old-storage-class (array-storage-class      old-array))
  (define old-body          (array-body               old-array))
  (define old-indexer       (array-indexer            old-array))
  (define old-in-order?     (array-elements-in-order? old-array))
  (define old-safe?         (array-safe?              old-array))
  (define old-get           (array-getter             old-array))

  (define new-domain old-domain)

  ; (displayln (list 'old-domain old-domain))
  ; (displayln (list 'new-domain new-domain))
  
  #;(unless (or (eq? new-domain #f)
              (= (interval-volume new-domain)
                 (interval-volume old-domain)))
    (raise-argument-error 'array-copy
                          "the new domain passed does not have the same volume"
                          "new-domain" new-domain))
  #;(unless new-domain
    (set! new-domain old-domain))

  ;; (struct array (domain getter setter
  ;;              storage-class body indexer
  ;;              safe? elements-in-order?))

  ;; We have two cases.
  ;;   1. Specialized arrays: the backing store is linear
  ;;   2. The general case.
  #;(displayln "HERE1")
  (cond
    [(specialized-array? old-array)
     #;(displayln "HERE2")
     ; (struct storage-class (getter setter checker maker copier length default) #:transparent)
     ; Note: It's assumed that the result-storage-class can handle all elements
     ;       in the old storage class.
     
     (define old-copier  (storage-class-copier  old-storage-class))
     (define old-maker   (storage-class-maker   old-storage-class))
     (define old-get     (storage-class-getter  old-storage-class))
     (define old-length  (storage-class-length  old-storage-class))
     (define old-default (storage-class-default old-storage-class))

     (define n (old-length old-body))
     
     ; copy backing store
     (define new-maker    (storage-class-maker   result-storage-class))
     (define new-default  (storage-class-default result-storage-class))
     (define new-body     (new-maker n new-default))
     (define new-spec-get (storage-class-getter  result-storage-class))
     (define new-spec-set (storage-class-setter  result-storage-class))
     ; copy to new backing store
     (cond
       ; if possible, we want to use `copier`
       ;        TODOX
       #; [(and old-copier old-in-order?
             (eq? result-storage-class old-storage-class))
        (old-copier new-body 0 old-body 0 n)]
       ; if there is no `copier`, then if the elements are in order,
       ; we can copy between backing stores directly without
       ; having to use the indexer.

      ;        TODOX
      #;[old-in-order?        
        (for ([i (in-range (interval-volume new-domain))])
          (new-spec-set new-body i (old-get old-body i)))]
       ; turns out we need to a naive brute force copy
      [else
       (define get (array-getter old-array))

       (define d   (interval-dimension old-domain))
       (define vol (interval-volume old-domain))
       (specialize d (i j k l) _
                   (for/list ([t   (in-range vol)]
                              [(_) (in-interval old-domain)])
                     (new-spec-set new-body t (get _)))
                   (for/list ([t  (in-range vol)]
                              [is (in-interval/internal-vector old-domain)])
                     (new-spec-set new-body t (apply get (vector->list is)))))])
     ; done
     #;(define new-indexer
       (make-indexer 0 (interval-ls new-domain) (interval-strides new-domain)))
     (define new-indexer old-indexer)
     (define new-getter   (λ is (new-spec-get new-body (apply new-indexer is))))
     (define new-setter   (and mutable?
                               (λ (val . is)
                                 (new-spec-set new-body (apply new-indexer is) val))))
     (define elements-in-order? #t)
     (array new-domain new-getter new-setter
            result-storage-class new-body new-indexer
            safe? elements-in-order?)]
    ; The case: old-array is not specialized
    [else
     #;(displayln "HERE3")
     ; The copy is always specialized.
     (define n (interval-volume new-domain))
     (define new-maker    (storage-class-maker   result-storage-class))
     (define new-spec-get (storage-class-getter  result-storage-class))
     (define new-spec-set (storage-class-setter  result-storage-class))
     (define new-default  (storage-class-default result-storage-class))
     ; make new backing store
     (define new-body     (new-maker n new-default))
     ; copy from old array to the new backing store
     ; todo: specialize according to dimension of old array
     (for ([j  (in-range n)]
           [is (in-interval/internal-vector old-domain)])
       (new-spec-set new-body j (apply old-get (vector->list is))))
     ; remaining methods
     ; TODO: specialize according to dimension of new array.
     (define new-indexer
       (make-indexer 0 (interval-ls new-domain) (interval-strides new-domain)))
     (define new-getter   (λ is (new-spec-get new-body (apply new-indexer is))))
     (define new-setter   (and mutable?
                               (λ (val . is)
                                 (new-spec-set new-body (apply new-indexer is) val))))
     (define elements-in-order? #t)
     #;(displayln (list 'new-body new-body))
     ; done
     (array new-domain new-getter new-setter
            result-storage-class new-body new-indexer
            safe? elements-in-order?)]))


(define/contract (safe-array-append k first-array . remaining-arrays)
  (-> integer? array? array? ...
      any)
  (apply array-append k first-array remaining-arrays))

(define (array-append k first-array . remaining-arrays)
  (define arrays (cons first-array remaining-arrays))

  ; todo: check domains match except on the kth axis

  ; compute new size of kth axis
  (define-values (dividers kth-size)
    (for/fold ([dividers '(0)]
               [sum      0]
               #:result (values (reverse dividers) sum))
              ([a (in-list arrays)])
      (define dom     (array-domain a))
      (define ls      (interval-ls dom))
      (define us      (interval-us dom))
      (define size    (- (vector-ref us k) (vector-ref ls k)))
      (define new-sum (+ sum size))
      (values (cons new-sum dividers) new-sum)))
  ; (displayln (list dividers kth-size))

  ; The new domain:
  ;   The interval of the result array is the same as the common
  ;   interval of the sub-arrays except on the kth axis, which
  ;   we set to [0;kth-size[.
  (define dom0 (array-domain first-array))
  (define ls   (interval-lower-bounds->vector dom0)) ; copy
  (define us   (interval-upper-bounds->vector dom0)) ; copy
  (vector-set! ls k 0)
  (vector-set! us k kth-size)
  (define dom  (make-interval ls us))
  (define d    (interval-dimension dom))

  ;(displayln (list 'dom dom))
  ;(displayln (list 'd d))
  
  ; The result:
  (define result (make-specialized-array dom))
  (define trans  (make-vector d 0))

  ;(displayln (list 'xx))

  (define (kth-lower-bound array)
    (vector-ref (interval-ls (array-domain array)) k))
  
  (for ([array (in-list arrays)]
        [start (in-list dividers)]
        [end   (in-list (rest dividers))])
    ; extract kth area of the result
    (vector-set! ls k start)
    (vector-set! us k end)

    ;(displayln (list 'translation trans))
    
    (define dst (array-extract result (make-interval ls us)))
    ;(displayln (list 'dst (array-domain dst)))
    ; make sure the source has the same domain as the destination
    (vector-set! trans k (- start (kth-lower-bound array)))
    ;(displayln (list 'array (array-domain array)))
    (define src (array-translate array trans))
    ;(displayln (list 'src (array-domain src)))
    ; copy
    (array-assign! dst src))

  (vector-set! ls k 0)
  (vector-set! us k kth-size)
  result)

(define (list-insert xs i x)
  (append (take xs i)
          (list x)
          (drop xs i)))


(define/contract (safe-array-stack k first-array . remaining-arrays)
  (-> integer? array? array? ...
      any)
  (apply array-stack k first-array remaining-arrays))

(define (array-stack k first-array . remaining-arrays)
  (define arrays (cons first-array remaining-arrays))
  ; todo: check domains match except on the kth axis
  ; size of kth axis
  (define n (length arrays))
  ; The new domain:
  ;   The new domain is the same as the old, except we are adding a new kth axis,
  ;   which contains the numbers 0, 1, ..., n-1
  (define old-dom (array-domain first-array))
  (define old-d   (interval-dimension old-dom))
  (define d       (+ old-d 1))

  ; new lower and upper bounds
  (define old-ls  (interval-ls old-dom))
  (define old-us  (interval-us old-dom))
  (define new-ls  (make-vector d 0))
  (define new-us  (make-vector d 0))
  (for ([i (in-range k)])
    (vector-set! new-ls i (vector-ref old-ls i))
    (vector-set! new-us i (vector-ref old-us i)))
  (for ([i (in-range (+ k 1) d)])
    (vector-set! new-ls i (vector-ref old-ls (- i 1)))
    (vector-set! new-us i (vector-ref old-us (- i 1))))
  (vector-set! new-ls k 0)
  (vector-set! new-us k n)
  ; new domain
  (define new-dom (make-interval new-ls new-us))
    
  ; The result:
  (define result (make-specialized-array new-dom))

  (define dsts (array-curry (array-permute result (index-first d k)) old-d))
  (define get  (array-getter dsts))
  (for ([i   (in-range n)]
        [src (in-list arrays)])
    (define dst (get i))
    (array-assign! dst src))

  result)

