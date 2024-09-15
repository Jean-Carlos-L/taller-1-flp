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



(define chip-or '(chip-or))
(define chip-and '(chip-and))
(define chip-not '(chip-not))
(define chip-xor '(chip-xor))
(define chip-nand '(chip-nand))
(define chip-nor '(chip-nor))
(define chip-xnor '(chip-xnor))

(define prim-chip '(prim-chip chip-prim))
(define comp-chip 
   (lambda 
      (inputs outputs circuit)
      `(comp-chip ,inputs ,outputs ,circuit)
   )
)

(define simple-circuit 
   (lambda
      (inputs outputs chip)
      `(simple-circuit ,inputs ,outputs ,chip)
   )
)

(define complex-circuit 
   (lambda 
      (circuit circuits inputs outputs)
      `(complex-circuit ,circuit ,circuits ,inputs ,outputs)
   )
)

;; Observadores

;; Predicados

(define chip-or?
   (lambda (chip)
      (eq? chip chip-or)
   )
)

(define chip-and?
   (lambda (chip)
      (eq? chip chip-and)
   )
)

(define chip-not?
   (lambda (chip)
      (eq? chip chip-not)
   )
)

(define chip-xor?
   (lambda (chip)
      (eq? chip chip-xor)
   )
)

(define chip-nand?
   (lambda (chip)
      (eq? chip chip-nand)
   )
)

(define chip-nor?
   (lambda (chip)
      (eq? chip chip-nor)
   )
)

(define chip-xnor?
   (lambda (chip)
      (eq? chip chip-xnor)
   )
)

(define prim-chip?
   (lambda (chip)
      (eq? (car chip) 'prim-chip)
   )
)

(define comp-chip?
   (lambda (chip)
      (eq? (car chip) 'comp-chip)
   )
)

(define simple-circuit?
   (lambda (circuit)
      (eq? (car circuit) 'simple-circuit)
   )
)

(define complex-circuit?
   (lambda (circuit)
      (eq? (car circuit) 'complex-circuit)
   )
)

;; Extractores

(define chip-prim
   (lambda (chip)
      (cadr chip)
   )
)

(define comp-chip-inputs
   (lambda (chip)
      (cadr chip)
   )
)

(define comp-chip-outputs
   (lambda (chip)
      (caddr chip)
   )
)

(define comp-chip-circ
   (lambda (chip)
      (cadddr chip)
   )
)

(define simple-circuit-inputs
   (lambda (circuit)
      (cadr circuit)
   )
)

(define simple-circuit-outputs
   (lambda (circuit)
      (caddr circuit)
   )
)

(define simple-circuit-chip
   (lambda (circuit)
      (cadddr circuit)
   )
)

(define complex-circuit-circuit
   (lambda (circuit)
      (cadr circuit)
   )
)

(define complex-circuit-circuits
   (lambda (circuit)
      (caddr circuit)
   )
)

(define complex-circuit-inputs
   (lambda (circuit)
      (cadddr circuit)
   )
)

(define complex-circuit-outputs
   (lambda (circuit)
      (cadr (cadddr circuit))
   )
)

;; Pruebas

(define circuit1 
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
)

(define curcuit2
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
)

(define parser
   (lambda (circuit)
      (cond
         [(simple-circuit? circuit) 
            (simple-circuit 
               (circuit-inputs circuit) 
               (circuit-outputs circuit) 
               (parser (circuit-chip circuit))
            )
         ]
         [(complex-circuit? circuit) (complex-circuit (parser (circuit-circuit circuit)) (map parser (circuit-circuits circuit)) (circuit-inputs circuit) (circuit-outputs circuit))]
         [(prim-chip? circuit) (prim-chip (parser (chip-prim circuit)))]
         [(comp-chip? circuit) (comp-chip (circuit-inputs circuit) (circuit-outputs circuit) (parser (circuit-circuit circuit)))]
         [else (eopl:error 'parser "Circuito no válido")]
      )
   )
)

(parser circuit1)

