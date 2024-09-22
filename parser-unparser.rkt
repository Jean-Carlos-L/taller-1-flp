;; Autores:
;;  - Jean Carlos Lerma Rojas - 2259305
;;  - Juan Camilo García Saenz - 2259416

#lang racket

(require (only-in eopl cases define-datatype))
(require  "representacion-listas.rkt")
(require  "representacion-datatype.rkt")

; (define compuerta-or list-chip-or)
; (define compuerta-and list-chip-and)

; (define pruebita (list-prim-chip compuerta-or))
; (define pruebita2 (list-prim-chip compuerta-and))

; (define test-simple-circuit (list-simple-circuit '(a b) '(c) pruebita))

; (define test-chip-comp (list-comp-chip '(a b) '(c) test-simple-circuit))

; (define test-complex-circuit (list-complex-circuit test-simple-circuit (list test-simple-circuit) '(a b) '(c)))

;; Pruebas para datatype

(define compuerta-and (chip-and))
(define chipsito (prim-chip compuerta-and))
(define simple-circuito (simple-circuit '(a b) '(c) chipsito))
(define chipsito-jodido (comp-chip '(t j) '(g) simple-circuito))
(define complex-circuito (complex-circuit simple-circuito (list simple-circuito) '(m x) '(p)))

(define parser
   (lambda (circuit)
      (cond
         [(list-chip-or? circuit) (chip-or)]
         [(list-chip-and? circuit) (chip-and)]
         [(list-chip-not? circuit) (chip-not)]
         [(list-chip-xor? circuit) (chip-xor)]
         [(list-chip-nand? circuit) (chip-nand)]
         [(list-chip-nor? circuit) (chip-nor)]
         [(list-chip-xnor? circuit) (chip-xnor)]
         [(list-prim-chip? circuit) (prim-chip (parser (list-chip-prim circuit)))]
         [(list-comp-chip? circuit) (comp-chip (list-comp-chip-inputs circuit) (list-comp-chip-outputs circuit) (parser (list-comp-chip-circ circuit)))]
         [(list-simple-circuit? circuit) (simple-circuit (list-simple-circuit-inputs circuit) (list-simple-circuit-outputs circuit) (parser (list-simple-circuit-chip circuit)))]
         [(list-complex-circuit? circuit) (complex-circuit (parser (list-complex-circuit-circuit circuit)) (map parser (list-complex-circuit-circuits circuit)) (list-complex-circuit-inputs circuit) (list-complex-circuit-outputs circuit))]
      )
   )
)

(define unparser
   (lambda (circuit)
      (cond 
         [(chip-prim? circuit) (cases chip-prim circuit
               (chip-or () list-chip-or)
               (chip-and () list-chip-and)
               (chip-not () list-chip-not)
               (chip-xor () list-chip-xor)
               (chip-nand () list-chip-nand)
               (chip-nor () list-chip-nor)
               (chip-xnor () list-chip-xnor)
            )
         ]
         [(chip? circuit) (cases chip circuit
            (prim-chip (prim) (list-prim-chip (unparser prim)))
            (comp-chip (inputs outputs circ) (list-comp-chip inputs outputs (unparser circ)))
         )]
         [(circuito? circuit) (cases circuito circuit
            (simple-circuit (inputs outputs chip) (list-simple-circuit inputs outputs (unparser chip)))
            (complex-circuit (circuit circuits inputs outputs) (list-complex-circuit (unparser circuit) (map unparser circuits) inputs outputs))
         )]
      )
   )
)
