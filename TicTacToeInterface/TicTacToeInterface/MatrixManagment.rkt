#lang racket

(define (make_matrix m n)
 (make-matrix_aux m n '()) 
)

(define (make-matrix_aux m n result)
  (cond
    ((zero? m)
     result
    )     
    (else
     (make-matrix_aux (- m 1) n (append result (list (make-list n 0))))
    ) 
  )  
)

(define (replace_matrix matrix i j element)
  (cond
    ((zero? i)     
     (cons (replace_list (car matrix) j element) (cdr matrix))
    )
    (else
     ( cons (car matrix) (replace_matrix (cdr matrix) (- i 1) j element ))
    )
  )
)

(define (replace_list list j element)
  (cond
    ((zero? j)
     (cons element (cdr list))
    )
    (else
     (cons(car list) (replace_list (cdr list) (- j 1) element))
    )
 )
)

(define (get_element matrix i j)
  (cond
    ((zero? i)
     (get_aux (car matrix) j)
    )
    (else
     (get_element (cdr matrix) (- i 1) j  )
    )
  )
)

(define (get_aux list j)
  (cond
    ((zero? j)
     (car list)
    )
    (else
     (get_aux (cdr list) (- j 1) )
    )
  )
)

(define (matrix-to-list matrix)
  (to-list-aux matrix '())
)

(define (to-list-aux matrix result)
  (cond
    ((empty? matrix)
     result
    )
    ((empty? (car matrix))
     (to-list-aux (cdr matrix) result)
    )
    (else
     (to-list-aux (append (list (cdar matrix)) (cdr matrix)) (append result (list (caar matrix))))
    ) 
  )  
)

(provide (all-defined-out))