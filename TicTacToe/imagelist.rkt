#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(define (adios lista result)
  (cond
    ((empty? lista)
     result
    )
    (else
     (cond
       ((zero? (car lista))
        (adios (cdr lista) (append result (list (circle 10 "solid" "blue"))))
       )
       (else
        (adios (cdr lista) result)
       ) 
     )  
    ) 
  )  
)

(define
  (lista n)
  (list (circle 10 "solid" "blue")
         (circle 10 "solid" "red")
         (circle 10 "solid" "green")
         (circle 10 "solid" "yellow")))

(define (hola n)
(place-images
   (adios n '())
   (list (make-posn 180 200)
         (make-posn 0 60)
         (make-posn 140 20)
         (make-posn 80 140))
   (empty-scene 500 500)))

(big-bang (make-list 4 0) (to-draw hola))