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

(define (modify-matrix matrix x y element)  
  (cond
    ((zero? y)
     (append (list (modify_aux x element (car matrix))) (cdr matrix)) 
    )
    (else
     (append (list (car matrix)) (modify-matrix (cdr matrix) x (- y 1) element))
    ) 
  )  
)

(define (modify_aux x element row)
  (cond
    ((zero? x)
     (append (list element) (cdr row))
    )
    (else
     (append (list (car row)) (modify_aux (- x 1) element (cdr row)))
    )
  )
)

(define (find-element matrix i j)  
  (cond
    ((null? matrix)
     #false
    )
    ((zero? i)
     (find_aux (car matrix) j)
    )
    (else
     (find-element (cdr matrix) (- i 1) j)
    ) 
  )  
)

(define (find_aux row j)
  (cond
    ((null? row)
     #false
    )
    ((zero? j)
     (car row)
    )
    (else
     (find_aux (cdr row) (- j 1))
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