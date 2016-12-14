#lang eopl
(require data/gvector) 

;******************************************************************************************
; INTEGRANTES

;Mauricio Castillo 1226715
;Nathalia Bedoya 1226305
;Paola Medina 1329233

;;;;; Interpretador Simple

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;  <primitive>     ::= + | - | * | / | % |
;;  <bool-primitive>::= < | > | <= | >= | ==
;;  <bool-oper>     ::= not | and | or
;-----------------------------------------------------------------------------------------
;        Variables Globales declaradas para Muchas cosas
;-----------------------------------------------------------------------------------------
;;Variable para determinar el fin de un cadane o lista
(define terminal 0)
(define return-var '<nada>)

;; ---------------------------------------------------------------------------------------
;; ESPECIFICACION LEXICA
;; ---------------------------------------------------------------------------------------

(define scanner-spec-simple-interpreter
  '((white-sp
   (whitespace) skip)
    (-
     ("#" (arbno (not #\newline))) skip)
; Definicion de identificadores.
    (identifier(letter (arbno (or letter digit "?"))) symbol)
    
; Definicion de numeros enteros
    (number(digit (arbno digit)) number)
    (number("-" digit (arbno digit)) number)
    
; 2.1 Definicion de Flotantes
    (number(digit (arbno digit)"."digit (arbno digit)) number)
    (number("-" digit (arbno digit)"."digit (arbno digit)) number)
  
  #|Definición de char|#
    (cht-def( "\'" letter "\'" ) symbol)
    
  #|Definición de String|#
    (str-def( "\""(or letter whitespace digit) (arbno (or whitespace letter digit)) "\"") string)
  
  )
 )
;; ---------------------------------------------------------------------------------------
;; ESPECIFICACION SINTACTICA
;; ---------------------------------------------------------------------------------------

(define grammar-simple-interpreter
  '((program (expression) a-program)
    
    ;Expresiones char
    (expression (cht-def) cht-exp)
    
    ;Expresiones String
    (expression (str-def) str-exp)
    
    ;Expresiones Numericas
    (expression (number) lit-exp)
    
    ;Expreciones identificador
    (expression (identifier) var-exp)
    ;Exprecion Begin
    (expression ("begin" expression (arbno ";" expression) "end")begin-exp)

    
    ;2.1.1 Expresiones Booleanas
    ;<booleano>:= True | False
    ;proposito: representar los booleanos
    ;Explicación: Apartir de la gramatica dada, se hace la representación de los booleanos True y False, llamando la expresion
    ;para True  bool-expT y para False bool-expF
    ; ejemplos: True 
               ;False
    (expression ("True") bool-expT)
    (expression ("False") bool-expF)

     ; 2.2 Definicion de Operadores Matematicos(primitivas numericas)
    ; <primitiva> := " + "suma-prim
                     ;:= " - "resta-prim
                     ;:= " * "mult-prim
                     ;:= " / " div-prim
                     ;:= " mod "mod-prim
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)
    
    ;2.3 Definicion de Operadores Booleanos(primitivas relacionales)
    ; <primitiva> := "<" menor-prim
                 ;:= "<=" menor-o-igual-prim
                 ;:= ">" mayor-prim
                 ;:= ">=" mayor-o-igual-prim
                 ;:= "==" igual-igual-prim
                 ;:= "!=" diferente
                 ;:= "and" and-prim
                 ;:= "or" or-prim
                 ;:= "pnot" not-prim
    
    (primitive ("<") menor-prim)
    (primitive ("<=") menor-o-igual-prim)
    (primitive (">") mayor-prim)
    (primitive (">=") mayor-o-igual-prim)
    (primitive ("==") igual-igual-prim)
    (primitive ("!=") diferente)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("pnot") not-prim)

    ;Definicion de Gramatica de una funcion
    ;Esta es la especificacion gramatical de evaluate
    (expression( "evaluate" expression primitive expression ) primapp-expO)

    ;2.4 Expresion print
    (expression("print" "("(arbno  expression)")" )primaPrint-exp)
    
    ;Definicion primitiva de print
    (primitive ("pprint") print-prim)
        
    ;2.5 Definicion de condicionales
    (expression ("if" expression ":" expression (arbno expression)
                     (arbno "elif" expression ":" (arbno expression) "end")
                     "else" ":" (arbno expression) "end") if-exp)

    ; Expresion not
    (expression("not" "(" (arbno  expression) ")" )primaNot-exp)

    ;;2.7 Definicion una funcion 
    (expression("def" identifier "(" (separated-list identifier "," ) ")" ":" (arbno expression) "return" expression "end")funtion)
    (expression("execute" identifier "(" (separated-list expression ",") ")")funtionExecute)
    ;Funcion para saber si arlgo es un retorno
    (expression
               ("retornar" expression ) return-exp)
    ;-----------------------------------------------------------------------------------------------------
    ;                                          4 Proyecto Ciclos For and While
    ;-----------------------------------------------------------------------------------------------------
    ;; 4.1: Proyecto Definicion de For 
    (expression("for" identifier "in" "range" "(" expression "," expression")" "do" expression (arbno expression) "end") for-exp)

    ; 4.2: Proyecto Definicion de While
    (expression("while" "(" expression ")" "do" expression (arbno expression) "end" ) while-exp)
    ;-----------------------------------------------------------------------------------------------------
    ;                                          4 Asignacion de Variables
    ;-----------------------------------------------------------------------------------------------------
    
    ; 3 Global <expression> := global <identificador> = <expresion>
    (expression ("global" identifier "=" expression)global-exp)

    ;2.6 Definicion de ambientes para la variale Var
    (expression ("var" identifier "=" expression)set-exp)
    
    
    )
  )
;*****************************************************************
;                   Construidos automáticamente de DataType:
;*****************************************************************

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body init-env)))))
;***********************************************************************************************
;   AMBIENTE INICIAL
;***********************************************************************************************

;# Este Ambiente esta definido con el uso de Vectores
(define init-env 2)
(define gvectorLength (lambda (arreglo)
                         (vector-length (gvector->vector arreglo
                                                         ))
                         )
  )

;# Definición de datatype ambiente
; environment := <empty-env>
;             := <extended-env-record> (syms vals extend-from)
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms  gvector?)
                       (vals  gvector?)
                       (extend-from integer?)))
; Funcion que valida si una ruta n dada es valida en el ambiente_complemento
; positive-integer->bool
(define VerificacionRuta_complemento (lambda (n)
                                       (VerificacionRuta ambiente_complemento n)
                                           ))

; # Definicion de ambiente_complemento
; ambiente_complemento := <empty-env> <extended-env-record> <extended-env-record>*
;                  ambiente_complemento (ambiente-actual ambiente-vacio ambiente-global ambientes-locales)
(define ambiente_complemento ( gvector init-env (empty-env-record)
                                       (extended-env-record (gvector ) (gvector) 1)
                                       ))
; Funcion que valida si una ruta n dada es valida en un ambiente 
; environmetn positive-integer -> bool
(define VerificacionRuta
  (lambda (env n)
    (if  ( > n (- (gvectorLength env) 1)) #f #t)
    ) )
; Funcion que devuelve el ambiente numero n del ambiente_complemento inicial
; si se se ingresa un n mayor a la longitud de macroamb da una exepcion
; ambiente_complemento -> ambiente
(define get-env-n (lambda (n)
                (if (> n (- (gvectorLength ambiente_complemento) 1))
                    (eopl:error "El ambiente solicitado (No encontrado...)")
                    (gvector-ref ambiente_complemento n)
                    )))

; Funcion que recibe un gvector y un simbolo, si este existe retorna su ruta en el gvector
; si no existe retorna 9999 (esto limita a un ambiente a tener menos de 9999 variables)
(define search-symbol-gvector (lambda(gv symb pos)
                                (cond
                                  ((not (VerificacionRuta gv pos)) 9999)
                                  ((eq? (gvector-ref gv pos) symb) pos)
                                  (else (search-symbol-gvector gv symb (+ pos 1)))
                                  )))
; Funcion que recibe un ambiente, busca si existe una variable dada en este y retorna su pocición, si no la encuentra
; retorna -1
; env->ruta
(define search-var (lambda (env var)
  (cases  environment env
    (empty-env-record () (eopl:error "La variable No Se Encuentra Definida"))
    (extended-env-record (syms vals extend-from)
                         (search-symbol-gvector syms var 0)
                         )
    )
                     )
  )

; Funcion que recibe la ruta del ambiente en ambiente_complemento y un identificador de variable y retorna el valor que corresponde a esta, buscando tanto en el ambiente
; ingresado como en los ambientes de los que este extienda
; ambiente y simbolo -> valor
(define apply-env (lambda (env-n sym)

                    (cases  environment (get-env-n env-n)
                      (empty-env-record () (eopl:error "Variable no definida en el ambiente de ejecucion"))
                      (extended-env-record (syms vals extend-from)
                                           (cond
                                             ((= (search-symbol-gvector syms sym 0) 9999) (apply-env  extend-from sym))
                                             (else (gvector-ref vals (search-symbol-gvector syms sym 0)))
                                           )
                                           )
                      )
                    )
  
  )

; Verifica si una variable (id) esta definida en el ambiente.
; env id -> bool
(define exist-var? (lambda (env sym)
                    (cases  environment env
                      (empty-env-record () #f)
                      (extended-env-record (syms vals extend-from)
                                           (cond
                                             ((= (search-symbol-gvector syms sym 0) 9999) #f)
                                             (else #t)
                                           )
                                           )
                      )
                    )
  )
  


; Retorna el ambiente el cual pertenece las variables globales.

(define get-global-env (lambda()
                         (gvector-ref ambiente_complemento init-env)
                         )
  )
; Variable que define la ruta del ambiente global en el ambiente_complemento


; Adiciona una variable a un ambiente, basado en la ruta
; en el vector de ambientes (ambiente_complemento)
(define add-var (lambda (pos-env id val)
                  (if (VerificacionRuta_complemento pos-env)
                      (cases environment (gvector-ref ambiente_complemento pos-env)
                        (empty-env-record () (eopl:error "Error al agregar variable"))
                        (extended-env-record (syms vals extend-from)
                                             (begin
                                               (gvector-add! vals   val)
                                               (gvector-add! syms id)
                                               )
                                             )
                        )
                      (eopl:error "Usted esta ingresando a un ambiente no definido")
                      )
                  )
  )
; Adiciona una variable a un ambiente que se encuentra en una ruta dada (pos-env)
; Si la variable existe modifica su valor sino la crea.
(define set-var (lambda (pos-env id val)
                  (cases environment (get-env-n  pos-env)
                    (empty-env-record () (eopl:error "Falla del Ambiente al insertar variable"))
                    (extended-env-record (syms vals extend-from)
                                         (if (exist-var? (get-env-n  pos-env) id)
                                             (gvector-set! vals (search-var (get-env-n   pos-env) id) val)
                                             (add-var pos-env id val)
                                             )
                                         )))
  )
; Funcion que adiciona una variable global, si la variable denotada por el id ingresado ya existe
; se modifica esta por el valor de val
(define addGlobalVar (lambda (id val)
                         (set-var init-env id val)
                         ))
(define set-current-env (lambda (n)
                          (gvector-set! ambiente_complemento 0 n)
                          ))

; Retorna el ambiente que referencia la ejecucion de instrucciones

(define get-current-env(lambda ()
                         (gvector-ref ambiente_complemento 0)
                         )
  )
;Funcion que adiciona un nuevo ambiente a ambiente_complemento
(define add-env (lambda (env)
                  (gvector-add! ambiente_complemento env)
                  )
  )
(define setEjecucionenv (lambda (n)
                          (gvector-set! ambiente_complemento 0 n)
                          )
  )
;**********************************************************************************************
;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
;**********************************************************************************************
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (bool-expF () #f)
      (bool-expT () #t)
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (cht-exp (chr) chr)
      (str-exp (cd)
               (let ((end (- (string-length cd) 1))) ;#|Quitamos el ultimo caracter de la cadena|#
                 (substring cd 1 end)))
     ;Ejemplo While expretion
      ;var a = 3
      ;while (evaluate a < 6) do
      ;  var a = evaluate a + 1
      ;  print ( a )
      ;end
      
    (while-exp (parada first-exp-body rest-exps-body)
               (begin
                 (set-var (get-current-env) 'while (parse-while-to-recursive-closure parada (cons first-exp-body rest-exps-body) env))
                 (eval-expression (funtionExecute 'while '()) env)))
      

      ;Inserto if-exp

      ;if expresion : expresion+
      ;<elif expresion : expresion* end>*
      ;else : expresion*
      ;end
      ;Proposito: poder realizar condicionales de tipo if elif else dentro del lenguaje que se esta creando
      ;Explicación: al if-exp le ingresa las siguientes expresiones:
      ;if-test-exp, que es la expresion que se debe evaluar en el if
      ;if-true-first-exp,if-true-rest-exps, todas las expresiones+ que se da en caso de que al evaluar la expresion if-test-exp esta sea verdadera
      ;elif-test-exps elif-true-exps que indican respectivamente la lista de expresiones a evaluar de los elif que se ingresen y la lista de expresiones
      ;que se realizan en caso que algunas de las expresiones anteriores de verdaderas.
      ;y por ultimo else-exps que es la lista de expresiones que se van a arealizar en caso de que no se cumpla ninguna de las sentencias anteriores.

      ;lo que se hace es verificar si la expresion del if da correcta pues se llama a la función eval-rands2 la cual evalua todas las expresiones
      ;if-true-first-exp if-true-rest-exps pero solo retorna el resultado de la ultima
      ; y en caso de que esta no se cumpla se llama a eval-elif la cual internamente evaluara los elif y mira si se cumle alguno y si no realizara el
      ; else-exps
      ;Pruebas:
      ;if evaluate 5 < 3 :
      ;evaluate 4 + 3
      ;evaluate 4 < 5
      ;else :
      ;evaluate 8 + 9
      ;evaluate 7 < 3
      ;end
      ;R=#f

      ;if evaluate 5 < 3 :
      ;evaluate 4 + 3
      ;evaluate 4 < 5
      ;elif True :
      ;evaluate 4 + 9
      ;evaluate 6 + 8
      ;end
      ;else :
      ;evaluate 8 + 9
      ;evaluate 7 < 3
      ;end
      ;R=14 
      (if-exp (if-test-exp if-true-first-exp if-true-rest-exps elif-test-exps elif-true-exps else-exps)
              (if  (eval-expression if-test-exp env)
                               (eval-rands2 (cons if-true-first-exp if-true-rest-exps) env)
                               (eval-elif elif-test-exps elif-true-exps else-exps env)))
      

            (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      
; Procedimietos para not
      ;se define la variable args con el rand(expresion) que se ingresa evaluado en el ambiente y se manda al
      ;apply-primitive  la primitiva not-prim y el rand evaluado, para que realize el not de scheme a la expresion
      ;pruebas:
      ;not (evaluate True and False)
      ;R=#t
      ;not (True)
      ;#f
      
      (primaNot-exp (rands)
                   (let 
                       ((args (eval-rands rands env)))
                     (apply-primitive (not-prim) args)))

      
      
; Procedimientos para Print
      ;proposito: lograr que se imprima una expresion
      ;explicación: Se crea la lista de los args con el rands ingresado evaluedo en el ambiente
      ;y se llama a apply-primitive con la primitiva print-prim y los args, ya que esta retorna la evaluación de rands.
      ;pruebas:
      ;print ("hola")
      ;R= "hola"
      ;print (evaluate 2+5)
      ;R= 7
      (primaPrint-exp (rands)
                   (let 
                       ((args (eval-rands rands env)))
                     (apply-primitive(print-prim) args)))

      
      
;Reconoce las expresiones Evaluate
      ;<primitiva> := " + "suma-prim
                     ;:= " - "resta-prim
                     ;:= " * "mult-prim
                     ;:= " / " div-prim
                     ;:= " % "mod-prim
      ;<expresion> := evaluate expresion primitiva expresion evaluate-exp( exp1 exp2 )
      ;proposito: evaluar una  primitiva sobre dos numeros, en notación infija
      ;Explicación: se crea una lista llamada args con los dos rands (numeros) ingresados, evaluados en el ambiente de forma recursiva con
      ;eval-expression y despues esta lista con la primitiva que se se recibe se le envia al apply-primitive para que se realize la operación
      ;que pide la primitiva en concreto.
      ;pruebas:
      ;evaluate 5+7
       ;R=12
      ;evaluate 5 + evaluate 7+1
      ;R=13
      ;evaluate evaluate 2*3+1
      ;R=7
      ;evaluate evaluate 2%2*7
      ;R=0
      ;evaluate 1 + evaluate 2 + evaluate 3 + evaluate 4+5
      ;R=15
      ;evaluate 5 < 3
      ;R=#f
      ;evaluate 5 > 3
      ;R=#t
      ;evaluate 5 == 5
      ;R=#t
      ;evaluate
      ;evaluate True and False
      ;R=#f
      ;evaluate False or evaluate True or False
      ;#t
      
      (primapp-expO ( rands prim rands2)                   
                    (let ((args
                           (list (eval-expression rands env) (eval-expression rands2 env) )

                                ))
                    (apply-primitive prim args)))

      
; # Inserta una varible en el ambiente si no existe previemente
      ;proposito: que en el lenguaje que estamos definiendo, se pueda definir variables con alcance estatico
      ;Explicaciòn: se espera la entrada de el id de la variable con la cual se identificará y la asignaciòn que se le hara
      ;un set-var el cual se el manda el ambiente que referencia la ejecucion de instrucciones con (get-current-env)el id de la variable y
      ; la expresión de que se aigna a la variable pero evaluada mediante eval-expression para que se asigne y se guarde en un ambiente dicha expresion ingresada al id
      ;Prueba:
      ;var x = 9
      ;print ( x )
      ;var x = 4
      ;print ( x )
     
      (set-exp (id rhs-exp)
                  (set-var (get-current-env) id (eval-expression rhs-exp env))
                  )
;# 2.6 Definicion de una funcion
      ;expresion := defidentificador (identifcador * ' , ' ) : expresion * return expresion end
      ;proposito: permitir que en el lenguaje que se esta definiendo, se permita crear funciones y en lenguaje las reconozca
      ;Explicación: se recibe el id de la funcion a definir, los argumentos que va a recibir dicha función, las expresiones
      ; del cuerpo de la función y la expresiones que tendra el return de la función para esto creo una copia del ambiente en ejecucion el cual
      ;get-current-env y almaceno junto con sus parametros y expreciones (id args exps-body return-exp ) como si estos fueran una variable definida en el ambiente global
      ;en forma de lista
      ;por medio de la funcion addGlobalVar
      ;
      ;Prueba:
      ;def funcion(a,b,c):
      ; evaluate x < a
      ; return evaluate b + c
      ;end
      
            (funtion (id args exps-body return-exp  )
               (addGlobalVar id (list args  exps-body   return-exp  (get-current-env)))
               )
;# 2.7 Definicion de ejecucion de una funcion
      ;proposito: permitir que el lenguaje sea capas de ejecutar funciones definidos previmente en el ambiente.
      ;Explicación recibe un identificador que es el nombre de una funcion previamente defeinida y los argumentos de
      ;que recive la funcion para operar.
      ;busco el nombre  del id en la ambiente global con la funcion apply-env la cual me restorna una lista con los parametro que espera la funcion mas los procedimientos y evaluaciones que realiza
      ;y realizo la ejecucion del cuerpo de la funciono dentro de un ambiente extendido usando la funcion setEjecucionenv y realizo llamados de fomarma recursiba hata
      ;que finaliza todas las evaluaciones
      ;Prueba:
      ;execute funcion (4,5,6)
      
     (funtionExecute (id argumentos)
             (let  ((funcion (apply-env env id)))
                   (eval-closure funcion argumentos)
      ))
      
    ; 4.1 For
    ;
    (for-exp (id-var exp-val-start exp-val-end first-exp-body exps-body )
               (begin 
               (set-var (get-current-env) 'fore (parse-for-to-recursive-closure id-var exp-val-start exp-val-end (cons first-exp-body exps-body) env)  )
               (eval-expression (funtionExecute 'fore (list exp-val-start exp-val-end)) env)
               ))
      ; Global
      (global-exp (id exp) (addGlobalVar id (eval-expression exp env)))
      ; Reotrnos Permite saber si una Funcion llego al punto final de RETORNO
      (return-exp (exp)
                  (begin
                    (set! terminal 1)
                    (set! return-var (eval-expression exp env))
                    (eval-expression exp env)
                    )
                  )
      
      )))


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
     (map (lambda (x) (eval-rand x env)) rands)))


;;funcion que realiza el la evaluacion de los rands que recibe, pero solo retorna la ultima posicion de esa lista que retorna el map
(define eval-rands2
  (lambda (list-exps env)
    (if(null? list-exps) return-var
       (if (eq? terminal 0)
           (cond
             ((or (return-exp? (car list-exps)) (eq? (length list-exps ) 1)) (eval-expression (car list-exps)  env) )
             (else
              (begin
                (eval-expression (car list-exps)  env)
                (eval-rands2 (cdr list-exps) env)
                )))
           return-var))))
  ;(lambda (rands env)
  ;  (display rands)
  ;  (list-ref  (map (lambda (x) (eval-rand x env)) rands) (-(length rands)1

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;funcion aux basada en la funcion iota para construir una lista para la iteracion
; pero agregando el incremento para evaluar en la condicion del tamaño
(define aux
  (lambda (fin inc)
    (let loop ((next 0))
      (if (>= next fin) '()
        (cons next (loop (+ inc next)))))))
;; Funcion que construye un closure recursivo al interior de el interpretador
;; que representa a la estructura for
;; id-var exp-val-start exp-val-end exps-body -> recursive-closure
(define parse-for-to-recursive-closure(lambda (id-var exp-val-start exp-val-end exps-body env)
  (closure (list id-var 'valend)
           (list (if-exp
            (primapp-expO (var-exp id-var) (menor-prim) (var-exp 'valend)); test-exp
            (car exps-body);true-fist-exp
            (if
             (null? (cdr exps-body))
             (list (funtionExecute  'fore (list (primapp-expO (var-exp id-var) (add-prim) (lit-exp 1)) (var-exp'valend))));true-rest-exp
             (append  (cdr exps-body) (list (funtionExecute  'fore (list (primapp-expO (var-exp id-var) (add-prim) (lit-exp 1)) (var-exp'valend)))));true-rest-exp
             )
            '()
            '()
            '()
            )
                 )
           env
           )
  )
  )
 ;; Funcion que construye un closure recursivo al interior de el interpretador
;; que representa a la estructura while
;; id-var exp-val-start exp-val-end exps-body -> recursive-closure test-exp first-exp-body rest-exps-body
(define parse-while-to-recursive-closure(lambda ( test-exp exps-body env)
  (closure '()
           (list (if-exp
            test-exp
            (car exps-body);true-fist-exp
            (if
             (null? (cdr exps-body))
             (list (funtionExecute  'while '()))  ;true-rest-exp
             (append  (cdr exps-body) (list (funtionExecute  'while '())))
             )
            '()
            '()
            '()
            )
                 )
           env
           )
  )
  )


;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (mod-prim () (modulo (car args) (cadr args)))
      (add-prim () (+ (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))  
      (substract-prim () (- (car args) (cadr args)))
      (menor-prim () (< (car args) (cadr args)))
      (mayor-prim () (> (car args) (cadr args)))
      (mayor-o-igual-prim () (>= (car args) (cadr args)))
      (menor-o-igual-prim () (<= (car args) (cadr args)))
      (igual-igual-prim () (= (car args) (cadr args)))
      (diferente () (not(= (car args) (cadr args))))
      (and-prim () (and (car args) (cadr args)))
      (or-prim () (or (car args) (cadr args)))
      (not-prim () (not (car args)))
      (print-prim () (car args))

      )))

;*******************************************************************************************
;Ambientes



(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;;Codigo tomado del campus
(define-datatype proc proc?
  (closure
   (rands (list-of symbol?))
   (exps (list-of expression?))
   (env integer?)
   )
  )




;****************************************************************************************
;Funciones Auxiliares
;----------------------------------------------------------------------------------------
;         Evalucion de PROC
;----------------------------------------------------------------------------------------
(define return-exp? (lambda (x)
                      (if (expression? x)
                          (cases expression x
                            (return-exp (exp) #t)
                            (else #f) )#f
                 )))

(define eval-closure (lambda (closur args)
                        (cases proc closur
                           (closure (rands body extend-from)
                                    (begin
                                      (add-env (extended-env-record (list->gvector rands) (list->gvector (map (lambda (exp) (eval-expression  exp (get-current-env)  )) args)) extend-from))
                                      (set-current-env (+ (get-current-env) 1))
                                      
                                      (let ((resultado (eval-list-exp-with-return body (get-current-env))))
                                        (begin
                                          (gvector-remove-last! ambiente_complemento)
                                          (set! terminal 0)
                                          (set! return-var '<nada>)
                                          (set-current-env (- (get-current-env) 1))
                                          resultado)
                                        )
                                      )
                       )))
  )
;; Funcion que evalua una lista de expresiones una por una, si encuentra un return para la evaluación y devuelve el valor de
;; el retorno
;; list-of-expressions -> valor-retorno (bien puede ser void)
(define eval-list-exp-with-return (lambda (list-exps env)
                                    (if
                                      (null? list-exps) return-var
                                      (if (eq? terminal 0)
                                          (cond
                                            ((or (return-exp? (car list-exps)) (eq? (length list-exps ) 1)) (eval-expression (car list-exps)  env) )
                                            (else
                                          (begin
                                            (eval-expression (car list-exps)  env) 
                                            (eval-list-exp-with-return (cdr list-exps) env)
                                            )
                                          )
                                            )
                                          return-var                                                                                  
                                      )
                                      )
                                    )
  )
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
;-------------------------------------------------------------------------------
;              Funciones Creadas especificamentes para el elif
;-------------------------------------------------------------------------------
(define eval-elif
  (lambda (list-test list-true list-else env)
    (cond
      [(null? list-test)(eval-rands2 list-else env)]
      [(eval-rand(car list-test)env)(eval-rands2(car list-true)env)]
      [else (eval-elif (cdr list-test) (cdr list-true) list-else env)])))
;-------------------------------------------------------------------------------
;      Agrego Variables al Ambiente inicial debido a que siempre esta en blanco
;-------------------------------------------------------------------------------
(addGlobalVar 'a 9)
(addGlobalVar 'b 5)
(addGlobalVar 'c 3)
(addGlobalVar 'd 8)
(addGlobalVar 'e 2)
(addGlobalVar 'x 4)


(interpretador)
;******************************************************************************************
;Pruebas este codigo se pone despues de iniciar la funcion interpretador
;;--> if evaluate 5 > 10 : evaluate 4+8 else: 5 end
;;--> if evaluate 5 > 10 : evaluate 4+8 else: print("hola") end
;;--> if evaluate 5 > 10 : evaluate 4+8 else: evaluate 4<8 end
;;--> var y = 56 : y
;;--> var mode = 45 : mode
;; => not(False)
;; => print(evaluate 45 +2;)
;;--> print("Hola mundo")
;;--> execute hola ("Hola mundo")
;;--> execute funcion(4,3)

