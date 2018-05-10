#lang racket

;; Importa todas las librerías y archivos externos necesarios para el funcionamenito de este módulo.
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require "MatrixManagment.rkt")
(require "Game.rkt")
(require "GreedyAlgorithm.rkt")

;; Define las constantes del programa, el ancho y el alto de la pantalla. También se obtiene el centro de la pantalla.
;; Se define el lienzo donde se van a dibujar todos los elementos en pantalla.
(define WIDTH 400)
(define HEIGHT 400)
(define MSG_POS (make-posn (- (/ WIDTH 2) 200) (/ HEIGHT 2)))
(define CANVAS (empty-scene WIDTH HEIGHT))

;; Define funciones cuyo valor será cambiado en algún momento. La cantidad de filas y columnas de la matriz. La variable
;; utilizada para tener control de los turnos y la variable utiliza para saber si el juego ya terminó.
(define ROW 1)
(define COLUMN 1)
(define TURN 0)
(define RT_STOP (list 0))

;; Retorna el tamaño que deben tener cada uno de los rectángulos que se dibujarán en pantalla.
(define square-size-x (/ WIDTH COLUMN))
(define square-size-y (/ HEIGHT ROW))

;; Define la función utilizada para calcular la posición del mouse en la matriz general.
(define (get-x-position x m) (quotient x (floor (/ WIDTH m))))
(define (get-y-position y n) (quotient y (floor (/ HEIGHT n))))

;; Retorna un rectángulo vacío del tamaño que se le pase por parámetro.
(define (empty-rectangle width height)
  (rectangle width height "outline" "blue")
)

;; Retorna un rectángulo con una O del tamaño que se le pase por parámetro.
(define (o-rectangle width height)
  (overlay (rectangle width height "outline" "blue") (circle 10 "solid" "red"))
)

;; Retorna un rectángulo con un X del tamaño que se le pase por parámetro.
(define (x-rectangle width height)
  (overlay (rectangle width height "outline" "blue") (overlay (line -30 -40 "blue") (line -30 40 "blue")))
)

;; Retorna un mensaje dependiendo del valor que se le pase por parámetro. Si la cantidad de turnos es mayor que el
;; tamaño de la matriz es empate, si es impar ganó el jugador y si en par ganó la PC.
(define (win-msg player)
  (cond    
    ((equal? player (* ROW COLUMN))
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Bien jugado \nHemos empatado" 24 "blue")        
    )
    ((equal? (remainder player 2) 1)
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Felicidades \nMe has ganado la partida!" 24 "blue")
    )
    (else
     (set! TURN 0)
     (set! RT_STOP (list 0))
     (text "Perdiste \nNo te preocupes, no es facíl ganarme" 24 "blue")
    )
  )
)

;; Retorna una lista de imagenes dependiendo de la lista que se pasa por parámetro.
(define (create-image-list datalist)
  (create-image-list_aux datalist '())
)  

;; Si el elemento en la lista es un 0 agrega a la lista un rectángulo vacío, si es un 1 agrega un rectángulo con una O, 
;; si es un 2 agrega un rectángulo con una X
(define (create-image-list_aux datalist result)
  (cond
    ((empty? datalist)
     result
    )
    (else
     (cond
       ((zero? (car datalist))
        (create-image-list_aux (cdr datalist) (append result (list (empty-rectangle square-size-x square-size-y))))
       )
       ((equal? (car datalist) 1)
        (create-image-list_aux (cdr datalist) (append result (list (o-rectangle square-size-x square-size-y))))
       )
       ((equal? (car datalist) 2)
        (create-image-list_aux (cdr datalist) (append result (list (x-rectangle square-size-x square-size-y))))
       )
       (else
        (create-image-list_aux (cdr datalist) result)
       ) 
     )  
    ) 
  )  
)

;; Retorna una lista de posiciones para dibujar los rectángulos en pantalla.
(define (create-image-position datamatrix)
  (create-image-position_aux datamatrix 0 0 '())
)

;; Recorre la matriz y va agregando posiciones dependiendo de cual lugar ocupa el elemento en la matriz.
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

;; Función que se encarga de dibujar todos los elementos en pantalla. La malla de rectángulos y los mensajes cuando
;; termina la partida. Dibuja estos mensajes cuando la variable RT_STOP es verdadera.
(define (draw-grid n)
  (cond
    ((zero? (car RT_STOP))     
     (place-images/align (create-image-list (matrix-to-list n)) (create-image-position n) "left" "top" CANVAS)
    )
    (else     
     (place-images/align (append (create-image-list (matrix-to-list n)) (list (win-msg TURN)))
                         (append (create-image-position n) (list MSG_POS)) "left" "top" CANVAS)
    )
  )  
)

;; Toma la posición del mouse y agrega un 1 en la posición de la matriz que corresponde.
;; Cambia la variable de turno para que la PC sepa que es su turno.
(define (process-player-action w x y me)
  (cond
    ((mouse=? me "button-down")
     (cond
       ((zero? (get_element w (get-y-position y ROW) (get-x-position x COLUMN)))
        (set! TURN (+ TURN 1))
        (replace_matrix w (get-y-position y ROW) (get-x-position x COLUMN) 1)
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

;; Resetea la matriz a ceros y por lo tanto, se limpia la pantalla.
(define (restart w ke)
  (cond
    ((key=? ke "up")
     (make_matrix ROW COLUMN)
    )
    (else
     w
    )
  )  
)

;; Utiliza la función que verifica si la PC o el jugador ganaron. Cambia el valor de la variable que utiliza la función de
;; dibujar para saber si tiene que mostrar el mensaje final.
(define (should-win? matrix)
  (cond
    ((or (win? matrix 1) (win? matrix 2) (equal? TURN (* ROW COLUMN)))
     (set! RT_STOP (list 1))
     #t
    )
    (else
     #f
    )
  )
)

;; Función que llama al algoritmo voraz para realiar el movimiento de la PC. Sólo realiza esta función si la cantidad de turnos
;; es impar.
(define (pc-move matrix)
  (cond
    ((equal? (remainder TURN 2) 1)
     (set! TURN (+ TURN 1))
     (greedyAlgorithmn matrix)
    )
    (else
     matrix
    );; Retorna un rect;; Retorna un rectángulo vacío del tamaño que se le pase por parámetro.ángulo vacío del tamaño que se le pase por parámetro.
  )
)

;; Función que realiza el bucle principal del juego, modifica la variables de la cantidad de filas y columnas que el usuario
;; ingresa. La matriz es recibida por cada una de la funciones que se llaman en el bucle y estas a su vez, devuelven la matriz
;; con los cambios que se le hayan hecho.
(define (TTT m n)
  (set! ROW m)
  (set! COLUMN n)
  (set! square-size-x (/ WIDTH COLUMN))
  (set! square-size-y (/ HEIGHT ROW))
  
  (big-bang
      (make_matrix ROW COLUMN)
      (name "Tic-Tac-Toe")      
      (on-mouse process-player-action)      
      (on-key restart)
      (on-tick pc-move)
      (to-draw draw-grid)
      (stop-when should-win? draw-grid)      
  )    
)