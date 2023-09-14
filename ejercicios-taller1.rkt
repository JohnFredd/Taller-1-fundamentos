;;Hernan David Cisneros Vargas 2178192
;;John Freddy Belalcar Rojas 2182464
;;Julián David Rendon Cardona 2177387

#lang eopl

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;; (1)
;; invert:
;; Proposito:
;; Lista-tuplas -> Lista-tuplas-invertida: Procedimiento que toma una lista de pares (tuplas)
;; y devuelve una nueva lista, donde cada par ha intercambiado el orden de sus elementos.
;;
;; <list>  := ()
;;         := ((<schemeVauluw> <acheyasby>) <list>)


(define invert
  (lambda (lst)
    (if (null? lst)
        '() 
        (cons (list (cadr (car lst)) (car (car lst))) 
              (invert (cdr lst)))))) 

;;pruebas *********************************************************************

(invert '((1 A) (2 B) (3 C)))  ; Debería imprimir '((A 1) (B 2) (C 3))
(invert '((como hola) (? estas)))  ; Debería imprimir '((hola como) (estas ?))
(invert '())  ; Debería imprimir '()

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;; (4)
;; filter-in:
;; Proposito:
;; Predicado x Lista -> Lista-cumple-predicado : Procedimiento que toma un predicado P y una lista L de elementos,
;; y devuelve una nueva lista L' con solo los elementos de L que satisfacen el predicado P.
;;
;; 
;;

(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()] 
      [(P (car L))   
       (cons (car L) (filter-in P (cdr L)))]
      [else            
       (filter-in P (cdr L))] 
    )))

;; Pruebas ***************************************************************************

(filter-in even? '(1 2 3 4 5 6 7 8 9 10)) ; Debería imprimir '(2 4 6 8 10)
(filter-in (lambda (x) (equal? x 'a)) '(a b c d e f g h i j)) ; Debería imprimir '(a)
(filter-in (lambda (x) (< x 30)) '(10 20 30 40 50)) ; Debería imprimir '(10 20)

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;; (7)
;; cartesian-product:
;; Proposito:
;; Lista x Lista -> Lista-producto-cartesiano : Procedimiento que toma los elementos de dos listas
;; y realiza su producto cartesiano y luego lo devuelve en una lista de tuplas.
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)

(define cartesian-product
  (lambda (L1 L2)
    (cond
      [(null? L1) '()]
      [else
       (mi_append (mi_map (lambda (x) (list (car L1) x)) L2)
               (cartesian-product (cdr L1) L2))]
    )))



; ******************* Funciones auxiliares *******************************

;; mi_append:
;; Proposito:
;; Lista x Lista -> Lista : Procedimiento que toma dos listas, y devuelve una nueva lista
;; que contiene todos los elementos de La lista 1 seguidos por todos los elementos de La lista 2 en el mismo orden.
;;

(define mi_append
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (mi_append (cdr L1) L2)))))

;; mi_map:
;; Proposito:
;; (A -> B) x [A] -> [B] : Procedimiento que aplica la función func a cada elemento de
;; la lista lst y devuelve una nueva lista con los resultados de las aplicaciones.
;;

(define mi_map
  (lambda (func lst)
    (if (null? lst)
        '() 
        (cons (func (car lst)) 
              (mi_map func (cdr lst))))))

; ************************************************************************

;; Pruebas ***************************************************************

(cartesian-product '(1 2 3)'(a b c))
(cartesian-product '(azul rojo) '(manzana banano))
(cartesian-product '() '(a b c)) ; Deberia imprimir '()
(cartesian-product '(1) '(a b c)) ; Debería imprimir '((1 a) (1 b) (1 c))

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;; (10)
;; up:
;; Proposito:
;; L -> L' : Procedimiento que toma una lista L y devuelve una nueva lista L'
;; que es una versión plana de L, eliminando la anidación de sublistas.
;;

(define up
  (lambda (lst)
    (cond
      [(null? lst) '()] 
      [(pair? (car lst)) 
       (append (car lst) (up (cdr lst)))] 
      [else
       (cons (car lst) (up (cdr lst)))]  
    )))

;; Pruebas **************************************************************

(up '(1 2 3 4)) ; Debería imprimir '(1 2 3 4)
(up '(a (b c) d)) ; Debería imprimir '(a b c d)
(up '((x (y)) z)) ; Debería imprimir '(x (y) z)
(up '(((a) (b c)) (d (e (f g))))) ; Debería imprimir '((a) (b c) (d) (e (f g)))

;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;; (13)
;; operate:
;; Proposito:
;; [Lista de Funciones Binarias] [Lista de Números] -> Número : Procedimiento que aplica
;; sucesivamente una lista de operadores binarios a una lista de valores
;; y devuelve el resultado final de la operacion.
;
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)

(define operate
  (lambda (lrators lrands)
    (cond
      [(null? lrators)
       (car lrands)]
      [else (operate (cdr lrators)
                     (cons ((car lrators) (car lrands) (cadr lrands))
                           (cddr lrands)))]
  )))


;; Pruebas ***************************************************************

(operate (list + * - *) '(10 2 4 1 2)) ; Deberia imprimir 94
(operate (list * / + -) '(6 4 8 10 1)) ; Deberia imprimir 12
(operate (list - / - *) '(100 5 2 10 2)) ; Deberia imprimir 75

;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;; (16)
;; Operar-binarias:
;; Proposito:
;; S x L -> N : Calcula el resultado de una expresión binaria representada como una lista anidada,
;; donde S es una expresión válida que consiste en números enteros, operadores ('suma', 'resta' o 'multiplica')
;; y listas anidadas de expresiones, y L es una lista que representa la expresión binaria, 
;; y luego devuelve un número N que es el resultado de evaluar la expresión binaria.
;;
;;<OperacionB>::= <int>
;;            ::= (<OperacionB> ’suma <OperacionB>)
;;            ::= (<OperacionB> ’resta <OperacionB>)
;;            ::= (<OperacionB> ’multiplica <OperacionB>)


(define Operar-binarias
  (lambda (operacionB)
    (cond
      [(number? operacionB) operacionB]
      [(list? operacionB)
       (let
           ((op (cadr operacionB))
            (left (car operacionB))
            (right (caddr operacionB)))
         (cond
           ((equal? op 'suma) (+ (Operar-binarias left) (Operar-binarias right)))
           ((equal? op 'resta) (- (Operar-binarias left) (Operar-binarias right)))
           ((equal? op 'multiplica) (* (Operar-binarias left) (Operar-binarias right)))
           [else '()]
         ))]
      [else '()]
    ))) 

;; Pruebas *************************************************************

(Operar-binarias 4) ; Deberia imprimir 4
(Operar-binarias '(2 suma 9)) ; Deberia imprimir 11
(Operar-binarias '(2 resta 9)) ; Deberia imprimir -7
(Operar-binarias '((2 multiplica 3) suma (5 resta 1)))  ; Deberia imprimir 10
(Operar-binarias '((2 multiplica (4 suma 1))
                           multiplica
                           ((2 multiplica 4) resta 1))) ; Deberia imprimir 70

