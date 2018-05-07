;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |replace-get matrix|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;replace - replace in list
(define (replace_list list j element)
  (cond((zero? j) (cons element (cdr list)))
       (else(cons(car list) (replace_list (cdr list) (- j 1) element)))))

;replace - replace fila
(define (replace_matrix matrix i j element)
  (cond((zero? i) (cons (replace_list (car matrix) j element) (cdr matrix)))
       (else( cons (car matrix) (replace_matrix (cdr matrix) (- i 1) j element )))))

;get element in fila
(define (get_aux list j)
  (cond((zero? j) (car list))
       (else(get_aux (cdr list) (- j 1) ))))

;get fila
(define (get_element matrix i j)
  (cond((zero? i)(get_aux (car matrix) j))
       (else(get_element (cdr matrix) (- i 1) j  ))))