#lang eopl

;; Hernan David Cisneros Vargas 2178192
;; John Freddy Belalcar Rojas 2182464
;; Julián David Rendon Cardona 2177387

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (3)
;; list-set:
;; Proposito:
;; Lista x Int x Element -> Lista : Reemplaza el elemento n de la lista L por x.
;;
;; <Lista> := ()
;;         := (<valor-de-scheme> <Lista>)

(define list-set
   (lambda (L n x)
      (if (zero? n)
         (cons x (cdr L))
         (cons (car L) (list-set (cdr L) (- n 1) x)))))

;; Pruebas

(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (6)
;; swapper
;; Proposito:
;; Element x Element x Lista -> Lista : Remplaza todas las coincidencias de E1 por E2
;; y viceversa en la Lista L.
;;
;; <Lista> := ()
;;         := (<valor-de-scheme> <Lista>)

(define swapper
   (lambda (E1 E2 L)
      (if (null? L)
         L
         (cond
            [(equal? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
            [(equal? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
            [else (cons (car L) (swapper E1 E2 (cdr L)))]))))

;; Pruebas

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
