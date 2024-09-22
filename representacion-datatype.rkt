;; Autores:
;;  - Jean Carlos Lerma Rojas - 2259305
;;  - Juan Camilo García Saenz - 2259416

#lang eopl

; Gramatica BNF

; <circuito> := circ-simple ({cable}*)
; 			      ({cable}*)
; 		         <chip>
; 		simple-circuit (int out chip)
; 		:= circ_comp <circuito>{<circuito>}*
; 			input {cable}*
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
;                chip-xor()
;             := prim_nand
;                chip-nand()
;             := prim_nor
;                chip-nor()
;             := prim_xnor
;                chip-xnor()

(define-datatype circuito circuito?
   (simple-circuit
      (inputs (list-of symbol?))
      (outputs (list-of symbol?))
      (chip chip?)
   )
   (complex-circuit
      (circuit circuito?)
      (circuits (list-of circuito?))
      (inputs (list-of symbol?))
      (outputs (list-of symbol?))
   )
)

(define-datatype chip chip?
   (prim-chip
      (prim chip-prim?)
   )
   (comp-chip
      (inputs (list-of symbol?))
      (outputs (list-of symbol?))
      (circ circuito?)
   )
)

(define-datatype chip-prim chip-prim?
   (chip-or)
   (chip-and)
   (chip-not)
   (chip-xor)
   (chip-nand)
   (chip-nor)
   (chip-xnor)
)


;; Pruebas

(define example1 (comp-chip
   '(INA INB INC IND)
   '(OUTA)
   (complex-circuit
      (simple-circuit '(a b) '(e)
         (prim-chip (chip-and))
      )
      (list
         (simple-circuit 
            '(c d) 
            '(f)
            (prim-chip (chip-and))
         )
         (simple-circuit 
            '(e f) 
            '(g)
            (prim-chip (chip-or))
         )
      )
      '(a b c d)
      '(g)
   )
))

(define example2 (complex-circuit
      (simple-circuit 
         '(m n o p)
         '(e f)
         (comp-chip
            '(INA INB INC IND)
            '(OUTE OUTF)
            (complex-circuit
               (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
               (list
                  (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
               )
               '(a b c d)
               '(e f)
            )
         )
      )
      (list
         (simple-circuit
            '(e f)
            '(z)
            (comp-chip
               '(INE INF)
               '(OUTA)
               (simple-circuit '(e f) '(g) (prim-chip (chip-or)))
            )
         )
      )
      '(m n o p)
      '(z)
   )
)

(define example3 (complex-circuit
   (simple-circuit 
      '(m n o p)
      '(e f)
      (comp-chip
         '(INA INB INC IND)
         '(OUTE OUTF)
         (complex-circuit
            (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
            (list
               (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
            )
            '(a b c d)
            '(e f)
         )
      )
   )
   (list
      (simple-circuit
         '(e f)
         '(z)
         (comp-chip
            '(INE INF)
            '(OUTA)
            (simple-circuit '(e f) '(g) (prim-chip (chip-or)))
         )
      )
   )
   '(m n o p)
   '(z)
))

;;ejemplo circuito simple
(define circuit-simple-datatype (simple-circuit '(a b) '(c) (prim-chip (chip-and))))
;;ejemplo circuito complejo
(define circuit-complex-datatype (complex-circuit circuit-simple-datatype (list circuit-simple-datatype) '(a b) '(c)))


(display "Example 1")
(newline)
(newline)
(display example1)
(newline)
(newline)
(display "Example 2")
(newline)
(newline)
(display example2)
(newline)
(newline)
(display "Example 3")
(newline)
(newline)
(display example3)
(newline)
(newline)
(display "Circuit simple")
(newline)
(newline)
(display circuit-simple-datatype)
(newline)
(newline)
(display "Circuit complex")
(newline)
(newline)
(display circuit-complex-datatype)
(newline)



(provide (all-defined-out))