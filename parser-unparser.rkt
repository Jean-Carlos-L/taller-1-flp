;; Autores:
;;  - Jean Carlos Lerma Rojas - 2259305
;;  - Juan Camilo García Saenz - 2259416

#lang oepl

(define parser
   (lambda (circuit)
      (cond
         [(simple-circuit? circuit) (simple-circuit (circuit-inputs circuit) (circuit-outputs circuit) (parser (circuit-chip circuit)))]
         [(complex-circuit? circuit) (complex-circuit (parser (circuit-circuit circuit)) (map parser (circuit-circuits circuit)) (circuit-inputs circuit) (circuit-outputs circuit))]
         [(prim-chip? circuit) (prim-chip (parser (chip-prim circuit)))]
         [(comp-chip? circuit) (comp-chip (circuit-inputs circuit) (circuit-outputs circuit) (parser (circuit-circuit circuit)))]
         [else (eopl:error 'parser "Circuito no válido")]
      )
   )
)

(define unparser
   (lambda (circuit)
      (cases
         circuito circuit
         (simple-circuit (inputs outputs chip) `(simple-circuit ,inputs ,outputs ,chip))
         (complex-circuit (circuit circuits inputs outputs) `(complex-circuit ,circuit ,(map unparser circuits) ,inputs ,outputs))
         (prim-chip (prim) `(prim-chip ,prim))
         (comp-chip (inputs outputs circuit) `(comp-chip ,inputs ,outputs ,circuit))
      )
   )
)

; (provide (all-defined-out))