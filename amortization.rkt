#lang racket

(require racket/gui)
(require plot)
(plot-new-window? #t)


; Author: Karina Reyes

; Contract: pmt : number number number -> number
; Purpose: to calculate the payment per month
; Definition: 
(define (payment loan months ir)
  (* (/ (* (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) (/ (/ ir 100.0) 12.0))
        (- (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) 1.0)) loan))

; test
;(pmt 1000000 12 25)


(define (interest capital_to_be_paid ir)
  (* capital_to_be_paid (/ (/ ir 100.0) 12.0)))

; test
;(interest 1000000 25)

(define (amortization payment interest)
  (- payment interest))

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

(define (total_payment loan months ir)
  (* (payment loan months ir) months))

; test
;(total_payment 1000000 12 25)

(define lst_months '())
(define lst_cap_to_be_paid_y '()) 

(define (amortization_table loan months ir)
  (amortization_aux 12 0 (payment loan months ir) 0 0 0 loan 0 ir))

(define (amortization_aux months months_count pmt actual_interest acum_interest amt cap_tb_paid cap_paid ir)
  (writeln acum_interest)
  (set! lst_months (append lst_months (list months_count)))
  (set! lst_cap_to_be_paid_y (append lst_cap_to_be_paid_y (list cap_tb_paid)))
  (cond
    [(>= months_count months) #t]
    [else (amortization_aux months (+ months_count 1) pmt
                 (interest cap_tb_paid ir) (+ (interest cap_tb_paid ir) acum_interest)
                 (amortization pmt (interest cap_tb_paid ir))
                 (capital_to_be_paid cap_tb_paid (amortization pmt (interest cap_tb_paid ir)))
                 (capital cap_paid (amortization pmt (interest cap_tb_paid ir))) ir)]))

; test
;(amortization_table 1000000 12 25)


(define (amortization_plot)
  (plot (points (map vector lst_months lst_cap_to_be_paid_y) #:color 'red)))


  
(define frame (new frame% [label "Amortization"]))
 
(define msg (new message% [parent frame]
                 [label "This program helps you to calculate the total amount of interest you pay for a loan"]))

(define loan_field (new text-field% [parent frame]
                        [label "Amount of total loan: "]))

(define months_field (new text-field% [parent frame]
                        [label "Number of months: "]))

(define ir_field (new text-field% [parent frame]
                        [label "Interest rate per year: "]))

(define (main button event)
  (amortization_table
   (string->number (send loan_field get-value))
   (string->number (send months_field get-value))
   (string->number (send ir_field get-value)))
  (new button% [parent frame]
             [label "Graph"]
             [callback main_graph]))

(define (main_graph button event)
  (amortization_plot))

(new button% [parent frame]
             [label "Show results"]
             [callback main])

(send frame show #t)