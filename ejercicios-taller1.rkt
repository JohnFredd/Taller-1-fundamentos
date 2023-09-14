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
