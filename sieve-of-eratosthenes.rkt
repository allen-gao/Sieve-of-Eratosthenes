#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This module contains a prime number finder which uses the 
;; Sieve of Eratosthenes algorithm, coded recursively.  

;; Information about the Sieve of Eratosthenes can be found here:
;; http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

;; sieve: Int -> (listof Int)
;; sieve consumes an integer n and produces a list of all the 
;; prime numbers between 1 and n.

;; Example: (sieve 50) -> '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

;; The implementation is below.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; filter-mult: (listof Int) Int Int -> (listof Int)

;;      lst: a sorted list that does not contain any duplicates.
;;      num: an Int whose multiples will be filtered from lst.
;;      max: an Int that is greater than/equal to the largest Int in lst. 
;;      num-mult: an Int accumulator that is a multiple of num.

;; filter-mult will produce a list equivalent to "lst" but with all the 
;; multiples of num filtered from it.

;; This function has O(n) runtime.
(define (filter-mult lst num max num-mult)
  (cond
    [(empty? lst) empty]
    [(= num-mult (first lst))
     (filter-mult (rest lst) num max (+ num-mult num))]
    [(> num-mult (first lst))
     (cons (first lst)
           (filter-mult (rest lst) num max num-mult))]
    [(< num-mult (first lst))
     (filter-mult lst num max (+ num-mult num))]
    [(> num-mult max) empty]))

;; ---------------------------

;; sieve-acc: (listof Int) (listof Int) Int Int -> (listof Int)

;;      lst: initially a list of incrementing Ints
;;      prime-list: a list of known prime numbers so far
;;      prime: most recently discovered prime
;;      n: the initial length of lst

;; This function has O(n) runtime.
(define (sieve-acc lst prime-list prime n)
  (define filtered-list (filter-mult lst prime n prime))
  (cond
    [(empty? filtered-list) (append prime-list 
                                    (cons prime empty))]
    [else (sieve-acc filtered-list
                     (append prime-list (cons prime empty))
                     (first filtered-list)
                     n)]))

;; sieve is the wrapper function for sieve-acc. 
;; see interface for purpose/example.
(define (sieve n)
  (sieve-acc (rest (build-list n add1)) '() 2 n))

