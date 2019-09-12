; Exercise 1.3 A proper List Predicate
; http://people.cs.aau.dk/~normark/pp/fp-intro-scheme-proper-lists-slide-exercise-1.html
; Function that checks if a list is a porper-list or pair-list

(define (proper-list1? list)
  (if(null? list) #t
     (if(pair? list)
        (proper-list1? (cdr list)) #f
     )   
  )
)

; Without if statement 
(define (proper-list2? list)
    (and 
        (pair? list) 
        (or 
            (null? (cdr list)) 
            (proper-list2? (cdr list))
        )
    )
)

(proper-list1? (list 1 2 3 4))
(proper-list1? (list 1 2 . 3))
(proper-list2? (list 1 2 3 4))
(proper-list2? (list 1 2 . 3))