;; Autores:
;;  - Jean Carlos Lerma Rojas - 2259305
;;  - Juan Camilo García Saenz - 2259416

#lang eopl

; Gramatica BNF

; <circuito> := circ-simple ({cable}*)
; 			      ({cable}*)
; 		         <chip>
; 		simple-circuit (int out chip)
; 		:= circ_comp <circuito>
;;       {<circuito>}*
; 			input {cable}*inputs
; 			output {cable}*
; 		complex-circuit (circ lcircs in out)

; <chip> := <chip_prim>
;          prim-chip (chip-prim)
;          := chip (-> {(port)})
;                (<- {(port)})
;                <circuito>
;          comp-chip(in,out,circ)

; <chip prim> := prim_or
;                chip-or()
;             := prim_and
;                chip-and()
;             := prim_not
;                chip-not()
;             := prim_xor
;                chip-xor()inputs
;             := prim_nand
;                chip-nand()
;             := prim_nor
;                chip-nor()
;             := prim_xnor
;                chip-xnor()



(define list-chip-or '(chip-or))
(define list-chip-and '(chip-and))
(define list-chip-not '(chip-not))
(define list-chip-xor '(chip-xor))
(define list-chip-nand '(chip-nand))
(define list-chip-nor '(chip-nor))
(define list-chip-xnor '(chip-xnor))

(define list-prim-chip 
   (lambda 
      (chip-prim)
      (list 'prim-chip chip-prim)
   )
)

(define list-comp-chip 
   (lambda 
      (inputs outputs circuit)
      (list 'comp-chip inputs outputs circuit)
   )
)

(define list-simple-circuit 
   (lambda
      (inputs outputs chip)
      (list 'simple-circuit inputs outputs chip)
   )
)

(define list-complex-circuit 
   (lambda 
      (circuit circuits inputs outputs)
      (list 'complex-circuit circuit circuits inputs outputs)
   )
)

;; Observadores

;; Predicados

(define list-chip-or?
   (lambda (chip)
      (eq? chip list-chip-or)
   )
)

(define list-chip-and?
   (lambda (chip)
      (eq? chip list-chip-and)
   )
)

(define list-chip-not?
   (lambda (chip)
      (eq? chip list-chip-not)
   )
)

(define list-chip-xor?
   (lambda (chip)
      (eq? chip list-chip-xor)
   )
)

(define list-chip-nand?
   (lambda (chip)
      (eq? chip list-chip-nand)
   )
)

(define list-chip-nor?
   (lambda (chip)
      (eq? chip list-chip-nor)
   )
)

(define list-chip-xnor?
   (lambda (chip)
      (eq? chip list-chip-xnor)
   )
)

(define list-prim-chip?
   (lambda (chip)
      (eq? (car chip) 'prim-chip)
   )
)

(define list-comp-chip?
   (lambda (chip)
      (eq? (car chip) 'comp-chip)
   )
)

(define list-simple-circuit?
   (lambda (circuit)
      (eq? (car circuit) 'simple-circuit)
   )
)

(define list-complex-circuit?
   (lambda (circuit)
      (eq? (car circuit) 'complex-circuit)
   )
)

;; Extractores

(define list-chip-prim
   (lambda (chip)
      (cadr chip)
   )
)

(define list-comp-chip-inputs
   (lambda (chip)
      (cadr chip)
   )
)

(define list-comp-chip-outputs
   (lambda (chip)
      (caddr chip)
   )
)

(define list-comp-chip-circ
   (lambda (chip)
      (cadddr chip)
   )
)

(define list-simple-circuit-inputs
   (lambda (circuit)
      (cadr circuit)
   )
)

(define list-simple-circuit-outputs
   (lambda (circuit)
      (caddr circuit)
   )
)

(define list-simple-circuit-chip
   (lambda (circuit)
      (cadddr circuit)
   )
)

(define list-complex-circuit-circuit
   (lambda (circuit)
      (cadr circuit)
   )
)

(define list-complex-circuit-circuits
   (lambda (circuit)
      (caddr circuit)
   )
)

(define list-complex-circuit-inputs
   (lambda (circuit)
      (cadddr circuit)
   )
)

(define list-complex-circuit-outputs
   (lambda (circuit)
      (cadddr (cdr circuit))
   )
)

;; Pruebas

(define list-circuit1 
   (list-comp-chip
      '(INA INB INC IND)
      '(OUTA)
      (list-complex-circuit
         (list-simple-circuit '(a b) '(e) (list-prim-chip list-chip-and))
         '(
            (list-simple-circuit '(c d) '(f) (list-prim-chip list-chip-and))
            (list-simple-circuit '(e f) '(g) (list-prim-chip list-chip-or))
         )
         '(a b c d)
         '(g)
      )
   )
)

(define list-curcuit2
   (list-complex-circuit
      (list-simple-circuit 
         '(m n o p) 
         '(e f)
         (list-comp-chip
            '(INA INB INC IND)
            '(OUTA OUTF)
            (list-complex-circuit
               (list-simple-circuit '(a b) '(e) (list-prim-chip list-chip-and))
               (list (list-simple-circuit '(c d) '(f) (list-prim-chip list-chip-and)))
               '(a b c d)
               '(e f)
            )
         )
      )
      (list
         (list-simple-circuit 
            '(e f) 
            '(z) 
            (list-comp-chip
               '(INE INF)
               '(OUTA)
               (list-simple-circuit '(e f) '(g) (list-prim-chip list-chip-or))
            )
         )
      )
      '(m n o p)
      '(z)
   )
)

(provide (all-defined-out))