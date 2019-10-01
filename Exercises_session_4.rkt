#lang racket
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())


(define (stream-section n stream)
  (cond ((= n 0) '())
        (else (cons (head stream)
              (stream-section 
                (- n 1)
               (tail stream))))))

(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream 
    (+ h1 h2)
    (add-streams (tail s1) (tail s2)))))


(define ones (cons-stream 1 ones))

(define nat-nums 
 (cons-stream 1 
  (add-streams ones nat-nums)))


(define (add a b)
  (+ a b))

; Exercise 4.2
(define (mul-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream 
    (* h1 h2)
    (mul-streams (tail s1) (tail s2)))))

(define fac
  (cons-stream 1
    (mul-streams (tail nat-nums) fac)))


;Exercise 4.4
(define (improve-sqrt-guess guess x)
 (/ (+ guess (/ x guess)) 2))



(define (map-stream f stream)
  (cond ((empty-stream? stream) the-empty-stream)
        (else (cons-stream (f (head stream)) (map-stream f (tail stream))))))

(define (sqrt-stream x)
  (cons-stream 1
    (map-stream (lambda (g) (improve-sqrt-guess g x))(sqrt-stream x))))

