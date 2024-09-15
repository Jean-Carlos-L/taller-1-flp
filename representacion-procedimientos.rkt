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

;; Representacion de datos por procedimientos

;; Contructores

(define chip-or
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-or]
            [else (eopl:error 'chip-or "No existe tal selector")]
         )
      )
   )
)

(define chip-and
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-and]
            [else (eopl:error 'chip-and "No existe tal selector")]
         )
      )
   )
)

(define chip-not
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-not]
            [else (eopl:error 'chip-not "No existe tal selector")]
         )
      )
   )
)

(define chip-xor
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-xor]
            [else (eopl:error 'chip-xor "No existe tal selector")]
         )
      )
   )
)

(define chip-nand
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-nand]
            [else (eopl:error 'chip-nand "No existe tal selector")]
         )
      )
   )
)

(define chip-nor
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-nor]
            [else (eopl:error 'chip-nor "No existe tal selector")]
         )
      )
   )
)

(define chip-xnor
   (lambda ()
      (lambda (s)
         (cond
            [(= s 0) 'chip-xnor]
            [else (eopl:error 'chip-xnor "No existe tal selector")]
         )
      )
   )
)

(define prim-chip
   (lambda (chip-prim)
      (lambda (s)
         (cond
            [(= s 0) 'prim-chip]
            [(= s 1) chip-prim]
            [else (eopl:error 'prim-chip "No existe tal selector")]
         )
      )
   )
)

(define comp-chip
   (lambda (inputs outputs circ)
      (lambda (s)
         (cond
            [(= s 0) 'comp-chip]
            [(= s 1) inputs]
            [(= s 2) outputs]
            [(= s 3) circ]
            [else (eopl:error 'comp-chip "No existe tal selector")]
         )
      )
   )
)

(define simple-circuit
   (lambda (inputs outputs chip)
      (lambda (s)
         (cond
            [(= s 0) 'simple-circuit]
            [(= s 1) inputs]
            [(= s 2) outputs]
            [(= s 3) chip]
            [else (eopl:error 'simple-circuit "No existe tal selector")]
         )
      )
   )
)

(define complex-circuit
   (lambda (circuit circuits inputs outputs)
      (lambda (s)
         (cond
            [(= s 0) 'complex-circuit]
            [(= s 1) circuit]
            [(= s 2) circuits]
            [(= s 3) inputs]
            [(= s 4) outputs]
            [else (eopl:error 'complex-circuit "No existe tal selector")]
         )
      )
   )
)

;; Observadores

;; Predicados

(define chip-or?
   (lambda (chip)
      (eq? (chip 0) 'chip-or)
   )
)

(define chip-and?
   (lambda (chip)
      (eq? (chip 0) 'chip-and)
   )
)

(define chip-not?
   (lambda (chip)
      (eq? (chip 0) 'chip-not)
   )
)

(define chip-xor?
   (lambda (chip)
      (eq? (chip 0) 'chip-xor)
   )
)

(define chip-nand?
   (lambda (chip)
      (eq? (chip 0) 'chip-nand)
   )
)

(define chip-nor?
   (lambda (chip)
      (eq? (chip 0) 'chip-nor)
   )
)

(define chip-xnor?
   (lambda (chip)
      (eq? (chip 0) 'chip-xnor)
   )
)

;; Extractores

(define chip-prim
   (lambda (prim-chip)
      (prim-chip 1)
   )
)

(define comp-chip-inputs
   (lambda (comp-chip)
      (comp-chip 1)
   )
)

(define comp-chip-outputs
   (lambda (comp-chip)
      (comp-chip 2)
   )
)

(define comp-chip-circ
   (lambda (comp-chip)
      (comp-chip 3)
   )
)

(define simple-circuit-inputs
   (lambda (simple-circuit)
      (simple-circuit 1)
   )
)

(define simple-circuit-outputs
   (lambda (simple-circuit)
      (simple-circuit 2)
   )
)

(define simple-circuit-chip
   (lambda (simple-circuit)
      (simple-circuit 3)
   )
)

(define complex-circuit-circuit
   (lambda (complex-circuit)
      (complex-circuit 1)
   )
)

(define complex-circuit-circuits
   (lambda (complex-circuit)
      (complex-circuit 2)
   )
)

(define complex-circuit-inputs
   (lambda (complex-circuit)
      (complex-circuit 3)
   )
)

(define complex-circuit-outputs
   (lambda (complex-circuit)
      (complex-circuit 4)
   )
)

;; Pruebas

(comp-chip
   '(INA INB INC IND)
   '(OUTA)
   (complex-circuit
      '(simple-circuit '(a b) '(e))
      '(
         (simple-circuit '(c d) '(f) (prim-chip chip-and))
         (simple-circuit '(e f) '(g) (prim-chip chip-or))
      )
      '(a b c d)
      '(g)
   )
)

(complex-circuit
   '(simple-circuit 
      '(m n o p) 
      '(e f)
      (comp-chip
         '(INA INB INC IND)
         '(OUTA OUTF)
         (complex-circuit
            '(simple-circuit '(a b) '(e) (prim-chip chip-and))
            '(
               (simple-circuit '(c d) '(f) (prim-chip chip-and))
            )
            '(a b c d)
            '(e f)
         )
      )
   )
   '(
      (simple-circuit 
         '(e f) 
         '(z) 
         (comp-chip
            '(INE INF)
            '(OUTA)
            (simple-circuit '(e f) '(g) (prim-chip chip-or))
         )
      )
   )
   '(m n o p)
   '(z)
)