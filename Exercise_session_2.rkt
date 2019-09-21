#lang racket
;Exercise 2.2. A tail recursive replication function.
; In this exercise, we will create a function that replicates a list to a given length n.
; The first function thus takes two parameters, the list to replicate, and a number n.
; It then calls a helper function to recursively replicate the list.

(define (replicate-to-lenght lst n)
  (replicate-help lst '() n))

; This helper function takes three parameters. The original list to replicate,
; a result list where the answer (the replicated list), is saved, and a number n.
; When the function is called, the result list is an empty list to begin with.
; In order to explain how the helper function works, we can call it with the original list abc, result list as '(),
; and n = 5.
; The recursive calls of replicate-help:
; abc '() 5
; bc a 4
; c ab 3
; () abc 2
; bc abca 1
; c abcab 0
; Return result.

; The function is tail recursive, because we do not make any computation on the result from replicate-help in each recursion.

(define (replicate-help lst result n)
  (if (= n 0)
      result
      (if (null? lst)
          (replicate-help (cdr result) (append result (list (car result))) (- n 1))
          (replicate-help (cdr lst) (append result (list (car lst))) (- n 1)))))

; Exercise 2.11. Make a C-style compare function.
; If x<y, return -1. If x>y, return 1. If x = y, return 0.

(define (lt x y) (< x y))
(define (gt x y) (> x y))

(define (make-comparator f)
  (lambda (x y) (cond ((f x y) "-1")
                      ((f y x) "1")
                      (else "0"))
    )
  )
