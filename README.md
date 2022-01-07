# SRFI 179 - Nonempty Intervals and Generalized Arrays

This is an implementation of the multi-dimensional arrays described in srfi-179.

The original srfi is here:

    https://srfi.schemers.org/srfi-179/srfi-179.html#array-curry

This implemention follows the followup:

    https://htmlpreview.github.io/?https://raw.githubusercontent.com/gambiteer/srfi-179-followup/master/srfi-179.html

Both Bradley J. Lucier's specification and test suite helped a great deal.


## How to install this srfi

The implementation is still a work in progress, so I haven't submitted it to the Racket package server yet.

To run the test suite simply clone the repo.
Open "arrays-test-suite.rkt" in DrRacket or in Emacs with racket-mode and run it.

## Benchmarking

For benchmarking make a new file:

   #lang racket
   (require "arrays.rkt")
   benchmarks here

And run the benchmark in the terminal.


## Comments and Feedback

Send comments and feedback to  jensaxel@soegaard.net




