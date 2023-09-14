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

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (9)
;; inversions
;; Proposito:
;; Lista -> Int : Retorna el número de inversiones de la lista L, sea L una lista
;; (a1 a2 ... an) de n numeros diferentes, una inversion de L es una pareja (i j)
;; tal que i < j y ai > aj.
;;
;; <Lista> := ()
;;         := (<Int> <Lista>)

; ************************* Funciones auxiliares *****************
;; aux-inversions
;; Proposito:
;; Int x Lista -> Int : Compara x con los elementos de la lista y devuelve
;; el numero de veces que x fue mayor que algun elemento de la lista.
;;
;; <Lista> := ()
;;         := (<Int> <Lista>)

(define aux-inversions
   (lambda (x L)
      (if (null? L)
          0
          (if (> x (car L))
              (+ 1 (aux-inversions x (cdr L)))
              (aux-inversions x (cdr L))))))

;; Pruebas

(aux-inversions 2 '(3 8 6 1))
(aux-inversions 1 '(2 3 4))
(aux-inversions 3 '(2 1))

; ************************* Funcion principal ********************

(define inversions
   (lambda (L)
      (if (null? L)
         0
         (+ (aux-inversions (car L) (cdr L)) (inversions (cdr L))))))

;; Pruebas

(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))
