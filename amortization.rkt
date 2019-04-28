#lang racket

(require racket/gui)

; Author: Karina Reyes

; Contract: pmt : number number number -> number
; Purpose: to calculate the payment per month
; Definition: 
(define (pmt loan months ir)
  (* (/ (* (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) (/ (/ ir 100.0) 12.0))
        (- (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) 1.0)) loan))

; test
;(pmt 1000000 12 25)


(define (interest capital_to_be_paid i)
  (* capital_to_be_paid (/ (/ i 100.0) 12.0)))

; test
;(interest 1000000 25)

(define (amortization pmt interest)
  (- pmt interest))

; test
;(amortization 95044.20 20833.33)

(define (capital_to_be_paid last amortization)
  (- last amortization))

; test
;(capital_to_be_paid 1000000 74210.87)

(define (capital last amortization)
  (+ last amortization))

; test
;(capital 0 74210.87)


(define (amortization_table loan months ir)
  (amort 12 0 (pmt loan months ir) 0 0 loan 0 ir))

(define (amort months count pmt int amt cap_tb_paid cap_paid ir)
  (writeln cap_tb_paid)
  (cond
    [(>= count months) #t]
    [else (amort months (+ count 1) pmt
                 (interest cap_tb_paid ir)
                 (amortization pmt (interest cap_tb_paid ir))
                 (capital_to_be_paid cap_tb_paid (amortization pmt (interest cap_tb_paid ir)))
                 (capital cap_paid (amortization pmt (interest cap_tb_paid ir))) ir)]))


(define (main button event)
  (amortization_table
   (string->number (send loan_field get-value))
   (string->number (send months_field get-value))
   (string->number (send ir_field get-value))))
  
; test
;(amortization_table 1000000 12 25)


(define frame (new frame% [label "Amortization"]))
 
(define msg (new message% [parent frame]
                 [label "This program helps you to calculate the total amount of interest you pay for a loan"]))

(define loan_field (new text-field% [parent frame]
                        [label "Amount of total loan: "]))

(define months_field (new text-field% [parent frame]
                        [label "Number of months: "]))

(define ir_field (new text-field% [parent frame]
                        [label "Interest rate per year: "]))
                        
(new button% [parent frame]
             [label "Table"]
             [callback main])

(new button% [parent frame]
             [label "Graph"]
             [callback main])

(send frame show #t)