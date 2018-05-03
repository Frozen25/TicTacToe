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

(define (modify-matrix matrix i j)
  (modify_aux '() i j matrix)
)

(define (modify_aux result i j matrix)
  (cond
    ((empty? matrix)
     (display result)
     (append (list result) (car matrix))
    )
    ((zero? i)
     (modify_aux_2 result j (car matrix))     
    )
    (else
     (modify_aux (append result (list (car matrix))) (- i 1) j (cdr matrix))
    ) 
  )  
)

(define (modify_aux_2 result j row)
  (cond
    ((empty? row)
     result
    )
    ((zero? j)
     ;;(modify_aux_2 (car matrix) (- j 1) (list 1))
     (display result)
     (append result (cdr row))
    )
    (else
     (modify_aux_2 (append result (list (car row))) (- j 1) (cdr row))
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

(define matrix
  '((0 2 3)
    (4 5 6)
    (7 8 1009))
) 

(provide (all-defined-out))















