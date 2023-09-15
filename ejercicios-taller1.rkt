#lang eopl

;; Hernan David Cisneros Vargas 2178192
;; John Freddy Belalcar Rojas 2182464
;; Julián David Rendon Cardona 2177387

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;; (1)
;; invert:
;; Proposito:
;; L -> L': Procedimiento que toma una lista de pares (tuplas) L
;; y devuelve una nueva lista L', donde cada par ha intercambiado el orden de sus elementos.
;;
;; <invert> := (invert <list>)
;;
;; <list>  := ()
;;         := ((<schemeValue> <schemeValue>) <list>)


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
;; P x L -> L' : Procedimiento que toma un predicado P y una lista L de elementos,
;; y devuelve una nueva lista L' con solo los elementos de L que satisfacen el predicado P.
;;
;; <filter-in> := (filter-in <predicate> <list>)
;;
;; <predicate> := <procedure>
;; <list>  := ()
;;         := ((<schemeValue> <schemeValue>) <list>)
;; <schemeValue> := <atom> | <list>


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
; ;<cartesian-product> := (cartesian-product <list> <list>)
;;
;; <list> := ()
;;        := (<valor-de-scheme> <list>)
;; <schemeValue> := <atom> | <list>

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
;; <mi_append> := (mi_append <list> <list>)
;;
;; <list> := ()
;;        := (<schemeValue> <list>)
;; <schemeValue> := <atom> | <list>

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
;; <mi_map> := (mi_map <function> <list>)
;;
;; <function> := <Procedure>
;; <list> ::= '() | (<schemeValue> <list>)
;; <schemeValue> := <atom> | <list>


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
;; <up> := (up <list>)
;;
;; <list> := ()
;;        := (<schemeValue> <list>)
;; <schemeValue> := <atom> | <list>

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
;;
;; <operate> := (operate <binaryFuncionList> <numbersList>)
;;
;; <binaryFuncionList> := ()
;;                    := (<function> <binaryFuncionList>)
;; <numbersList> := ()
;;               := (<int> <numbersList>)


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

(define ls1 (list-set '(a b c d) 2 '(1 2)))
(define ls2 (list-set '(a b c d) 3 '(1 5 10)))

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

(define sw1 (swapper 'a 'd '(a b c d)))
(define sw2 (swapper 'a 'd '(a d () c d)))
(define sw3 (swapper 'x 'y '(y y x y x y x x y)))

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

(define ain1 (aux-inversions 2 '(3 8 6 1)))
(define ain2 (aux-inversions 1 '(2 3 4)))
(define ain3 (aux-inversions 3 '(2 1)))

; ************************* Funcion principal ********************

(define inversions
   (lambda (L)
      (if (null? L)
         0
         (+ (aux-inversions (car L) (cdr L)) (inversions (cdr L))))))

;; Pruebas
(define in1 (inversions '(2 3 8 6 1)))
(define in2 (inversions '(1 2 3 4)))
(define in3 (inversions '(3 2 1)))

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (12)
;; filter-acum
;; Proposito:
;; Int x Int x Function x Int x Function -> Int : Aplica la funcion
;; binaria a todos los elementos entre a y b que cumplen el predicado
;; de la funcion unaria y lo guarda en acum, posteriormente retorna acum.

(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
       acum
       (if (filter a)
          (filter-acum (+ a 1) b F (F a acum) filter)
          (filter-acum (+ a 1) b F acum filter)))))

;; Pruebas

(define fa1 (filter-acum 1 10 + 0 odd?))
(define fa2 (filter-acum 1 10 + 0 even?))

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (15)
;; count-odd-and-even
;; Proposito:
;; Arbol -> Int x Int : Retorna una lista con dos elementos correspondientes
;; a la cantidad de pares e impares en el arbol.
;;
;; <Arbol> := ()
;;         := (<Int> <Arbol> <Arbol>)

; ************************* Funciones auxiliares *****************
;; count-even
;; Proposito:
;; Arbol -> Int : Retorna la cantidad de pares en el arbol.
;;
;; <Arbol> := ()
;;         := (<Int> <Arbol> <Arbol>)

(define count-even
   (lambda (arbol)
      (if (null? arbol)
         0
         (+
            (if (even? (car arbol))
               1
               0)
            (count-even (cadr arbol))
            (count-even (caddr arbol))))))

;; Pruebas

(define ce1 (count-even '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))))

(define ce2 (count-even
   '(5 (3 (2 (1 () ()) (4 () ())) ())
       (8 (6 () ()) (9 () ())))))

;; count-odd
;; Proposito:
;; Arbol -> Int : Retorna la cantidad de impares en el arbol.
;;
;; <Arbol> := ()
;;         := (<Int> <Arbol> <Arbol>)

(define count-odd
   (lambda (arbol)
      (if (null? arbol)
         0
         (+
            (if (odd? (car arbol))
               1
               0)
            (count-odd (cadr arbol))
            (count-odd (caddr arbol))))))

;; Pruebas

(define co1 (count-odd '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))))

(define co2 (count-odd
   '(5 (3 (2 (1 () ()) (4 () ())) ())
       (8 (6 () ()) (9 () ())))))

; ************************* Funcion principal ********************

(define count-odd-and-even
   (lambda (arbol)
      (cons (count-even arbol) (cons (count-odd arbol) empty))))

;; Pruebas

(define coae1 (count-odd-and-even '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ())))))

(define coae2 (count-odd-and-even
   '(5 (3 (2 (1 () ()) (4 () ())) ())
       (8 (6 () ()) (9 () ())))))

;--------------------------------------------------------------
;--------------------------------------------------------------
;; (18)
;; pascal
;; Proposito:
;; Int -> Lista : Retorna la N-esima fila del triangulo de pascal.
;;
;; <Pascal> := (<Lista>)+
;; <Lista>  := (<Int>)*

; ************************* Funciones auxiliares *****************
;; sumar-listas
;; Proposito:
;; Lista x Lista -> Lista : Retorna una lista correspondiente a la suma de
;; cada elemento de la primera lista con el elemento de la misma posicion
;; en la segunda lista.
;;
;; <Lista> := ()
;;         := (<Int> <Lista>)

(define sumar-listas
   (lambda (L1 L2)
      (if (null? L1)
         '()
         (cons (+ (car L1) (car L2)) (sumar-listas (cdr L1) (cdr L2))))))

;; Pruebas

(define sl1 (sumar-listas '(0 1 3 3 1) '(1 3 3 1 0)))
(define sl2 (sumar-listas '(0 1 4 6 4 1) '(1 4 6 4 1 0)))

;; append
;; Proposito:
;; Lista x Lista -> Lista : Retorna la lista resultante de concatenar las listas
;; de entrada.
;;
;; <Lista> := ()
;;         := (<Int> <Lista>)

(define append
   (lambda (L1 L2)
      (if (null? L1)
         L2
         (cons (car L1) (append (cdr L1) L2)))))

;; Pruebas

(define ap1 (append '(0 1 3 3 1) '(1 3 3 1 0)))
(define ap2 (append '(0 1 4 6 4 1) '(1 4 6 4 1 0)))

; ************************* Funcion principal ********************

(define pascal
   (lambda (N)
     (if (equal? N 1)
        '(1)
        (sumar-listas (cons 0 (pascal (- N 1))) (append (pascal (- N 1)) '(0))))))

;; Pruebas

(define pas1 (pascal 4))
(define pas2 (pascal 5))
(define pas3 (pascal 6))