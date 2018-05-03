#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require htdp/matrix)
(require lang/posn)
(require "MatrixManagment.rkt")

(define WIDTH 300)
(define HEIGHT 300)
(define ROW 0)
(define COLUMN 0)

(define CANVAS (empty-scene WIDTH HEIGHT))

(define (get-x-position x m)
  (quotient x (/ WIDTH m))
)

(define (get-y-position y n)
  (quotient y (/ HEIGHT n))
)

(define square-size-x
  (/ WIDTH 3)
)

(define square-size-y
  (/ HEIGHT 3)
)

(define (create-image-list datalist)
  (create-image-list_aux datalist '())
)  

(define (create-image-list_aux datalist result)
  (cond
    ((empty? datalist)
     result
    )
    (else
     (cond
       ((zero? (car datalist))
        (create-image-list (cdr datalist) (append result (list (circle 10 "solid" "blue"))))
       )
       (else
        (create-image-list (cdr datalist) result)
       ) 
     )  
    ) 
  )  
)

(define (create-image-position datamatrix)
  (create-image-position_aux datamatrix 0 0 '())
)

(define (create-image-position_aux datamatrix i j result)
  (cond
    ((empty? datamatrix)
     result
    )
    ((empty? (car datamatrix))
     (create-image-position_aux (cdr datamatrix) (+ i 1) 0 result)
    )
    (else
     (create-image-position_aux (append (list (cdar datamatrix)) (cdr datamatrix)) i (+ 1 j)
           (append result (list (make-posn (* j square-size-x) (* i square-size-y)))))
    ) 
  ) 
)  

(define (draw-grid n)  
  (place-image (square 100 "outline" "blue") 100 100 CANVAS)
  (place-image (circle 100 "outline" "blue") 200 200 CANVAS)
  (place-image (square 100 "outline" "blue") 300 300 CANVAS)

  #|
(place-images
   (create-image-list n)
   (create-image-position n)
   CANVAS))
   |#  
) 

(define (process-player-action w x y me)
  (cond
    ((mouse=? me "button-down")
     (cond
       ((zero? (find-element w (get-y-position y ROW) (get-x-position x COLUMN)))
        (matrix-set w (get-y-position y ROW) (get-x-position x COLUMN)  1)
       )
       (else
        w
       ) 
     )  
    )
    (else
     w
    )
  )  
)

(define (TTT m n)
  (set! ROW m)
  (set! COLUMN n)
  
  (big-bang
      (make_matrix m n)
      (name "Tic-Tac-Toe")
      (to-draw draw-grid)
      (on-mouse process-player-action) 
  )    
)