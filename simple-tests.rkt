#lang racket
(require "arrays.rkt")

(define an-interval      (make-interval #(1 2 3) #(11 22 33)))
(define another-interval (make-interval #(1 3 4) #(11 20 27)))
(interval-lower-bound an-interval 0)
(interval-upper-bound an-interval 0)
(interval-lower-bounds->vector an-interval)
(interval-upper-bounds->vector an-interval)
(interval-volume an-interval)       ; 6000
(interval= an-interval an-interval) ; #t
(interval-subset? an-interval an-interval) ; #t
(interval-projections an-interval 2)
; (interval-for-each (λ xs (displayln xs)) (make-interval #(2 3) #(4 6)))  
(interval-dilate an-interval #(1 1 1) #(2 2 2))

(interval-intersect an-interval another-interval)
(interval-translate an-interval #(100 200 300))
(interval-permute an-interval #(2 0 1))
(interval-rotate an-interval 1)
(interval-scale (make-interval #(0 0 0) #(10 10 10)) #(2 3 4)) ; (interval 3 '#(0 0 0) '#(5 4 3))
(interval-cartesian-product (make-interval #(0 0 0)    #(2 3 4))
                            (make-interval #(10 20 30) #(100 200 300)))

#;(let ()
  (define A  (make-specialized-array (make-interval #(10 20) #(15 25))))
  (define A_ (array-getter A))
  (define A! (array-setter A))
  (define B  (specialized-array-share A
                                      (make-interval #(0) #(5))
                                      (λ (i) (values (+ i 10) 20))))
  (define B_ (array-getter B))
  (define B! (array-setter B))
  B)

#;(let ()
  (define A  (make-specialized-array (make-interval #(10 20) #(15 25))))
  (define A_ (array-getter A))
  (define A! (array-setter A))
  (define B  (specialized-array-share A
                                      (make-interval #(0) #(5))
                                      (λ (i) (values (+ i 10) 20))))
  (define B_ (array-getter B))
  (define B! (array-setter B))
  (array->list B))

(let () ; 1-dimensional array with index from 10 to 20
  (define A  (make-specialized-array (make-interval #(10) #(20))))
  (define A_ (array-getter A))
  (define A! (array-setter A))
  (for ([i (in-range 10 20)]) (A! i i))
  (for/list ([i (in-range 10 20)]) (A_ i)))

(let () ; 2-dimensional array with index from [10,14[ x [30;44[
  (define A  (make-specialized-array (make-interval #(10 30) #(14 34))))
  (define A_ (array-getter A))
  (define A! (array-setter A))
  (for* ([i (in-range 10 14)] [j (in-range 30 34)]) (A! (list i j) i j))
  (displayln A)
  (for*/list ([i (in-range 10 14)] [j (in-range 30 34)]) (A_ i j)))

(let () ; array-copy on 1-dimensional specialized array
  (define A (make-specialized-array (make-interval #(10) #(20)) u32-storage-class))
  (define A! (array-setter A))
  (for ([x (in-range 10 20)])
    (A! (- x 10) x))
  (define B (array-copy A))
  (array->list B))

(let () ; array-copy on 2-dimensional specialized array
  (define A (make-specialized-array (make-interval #(10 15) #(15 20)) u32-storage-class))
  (define A_ (array-getter A))
  (define A! (array-setter A))
  (for* ([x (in-range 10 15)]
         [y (in-range 15 20)])
    (A! (+ (* 10 (- x 10)) (- y 15)) x y))
  ; (displayln (array-body A))
  (define B (array-copy A))
  (define B_ (array-getter B))
  ; (displayln (array-body B))
  (array->list B))

(let () ; array-copy on 1-dimensional general array
  (define A (make-array (make-interval #(10) #(20)) (λ (x) x)))
  (define A! (array-setter A))
  (define B (array-copy A))
  (array->list B))

(let () ; array-copy on 2-dimensional general array
  (define A (make-array (make-interval #(10 15) #(15 20)) list))
  (define A! (array-setter A))
  (define B (array-copy A))
  (array->list B))

(let () ; array-curry on 2-dimensional
  (define A (make-array (make-interval #(10 15) #(15 20)) list))
  (define B (array-curry A 1))
  (map array->list (array->list B)))

(let () ; array-extract on 1-dimensional
  (define A (make-array (make-interval #(10) #(20)) values))
  (define B (array-extract A (make-interval #(12) #(14))))
  (array->list B))

(let () ; array-extract on 1-dimensional
  (define A (make-array (make-interval #(10) #(20)) values))
  (define B (array-extract (array-copy A) (make-interval #(12) #(14))))
  (array->list B))

(let () ; array-extract on 1-dimensional
  (define A (make-array (make-interval #(10) #(20)) values))
  (define B (array-tile A #(2)))
  (map array->list (array->list B)))

(let () ; array-tile on 2-dimensional
  (define A (make-array (make-interval #(10 18) #(20 26)) list))
  (define B (array-tile A (vector 3 2)))
  (displayln (array-body (array-copy B)))
  (map array->list (array->list B)))

(let () ; array-translate on 1-dimensional
  (define A (make-array (make-interval #(10) #(20)) values))
  (define B (array-translate A #(2)))
  (array->list B)
  (array-domain B))

(let () ; array-tile on 2-dimensional
  (define A (make-array (make-interval #(4 5)) list))
  (define B (array-rotate A 1))
  (array->list B))



(define V (list->array '(0 1 2 3) (make-interval #(2 2))))

(time (for ([n (in-range 100000)])
        (array-ref V 0 0)))

(define V_ (array-getter V))

(time (for ([n (in-range 100000)])
        (V_ 0 0)))




