#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require htdp/matrix)
(require lang/posn)
(require "MatrixManagment.rkt")

(define WIDTH 400)
(define HEIGHT 400)
(define ROW 1)
(define COLUMN 1)
(define TURN 0)

(define CANVAS (empty-scene WIDTH HEIGHT))
(define (get-x-position x m) (quotient x (floor (/ WIDTH m))))
(define (get-y-position y n) (quotient y (floor (/ HEIGHT n))))
(define square-size-x (/ WIDTH COLUMN))
(define square-size-y (/ HEIGHT ROW))

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
        (create-image-list_aux (cdr datalist) (append result (list (rectangle square-size-x square-size-y "outline" "blue"))))
       )
       ((equal? (car datalist) 1)
        (create-image-list_aux (cdr datalist) (append result (list (circle 10 "outline" "blue"))))
       )
       (else
        (create-image-list_aux (cdr datalist) result)
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
     (create-image-position_aux (cdr datamatrix) (+ i square-size-y) 0 result)
    )
    (else
     (create-image-position_aux (append (list (cdar datamatrix)) (cdr datamatrix)) i (+ square-size-x j)
           (append result (list (make-posn j i))))
    ) 
  ) 
)  

(define (draw-grid n)
  (place-images/align (create-image-list (matrix-to-list n)) (create-image-position n) "left" "top" CANVAS)   
  ;;(place-image/align (circle 40 "outline" "blue") 150 150 "left" "top" CANVAS)  
)
  
(define (process-player-action w x y me)
  (cond
    ((mouse=? me "button-down")
     (cond
       ((zero? (find-element w (get-y-position y ROW) (get-x-position x COLUMN)))
        (set! TURN (+ TURN 1))
        ;;(modify-matrix w (get-y-position y ROW) (get-x-position x COLUMN)  1)
        (modify-matrix w (get-x-position x COLUMN) (get-y-position y ROW) 1)
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
  (set! square-size-x (/ WIDTH COLUMN))
  (set! square-size-y (/ HEIGHT ROW))
  
  (big-bang
      (make_matrix ROW COLUMN)
      (name "Tic-Tac-Toe")
      (to-draw draw-grid)
      (on-mouse process-player-action) 
  )    
)