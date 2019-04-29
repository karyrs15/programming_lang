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

(define (total_interest total_pmt loan)
  (- total_pmt loan))

(define lst_months '())
(define lst_amortization_interest '())

(define (amortization_table loan months ir)
  (amortization_aux months 0 (payment loan months ir) 0 0 loan 0 ir)
  (new button% [parent frame]
             [label "Graph"]
             [callback show_graph]))

(define (amortization_aux months months_count pmt actual_interest amt cap_tb_paid cap_paid ir)
  (writeln months_count)
  (set! lst_months (append lst_months (list months_count)))
  (set! lst_amortization_interest (append lst_amortization_interest (list (list months_count (list amt actual_interest)))))
  (cond
    [(>= months_count months) #t]
    [else (amortization_aux months (+ months_count 1) pmt
                 (interest cap_tb_paid ir)
                 (amortization pmt (interest cap_tb_paid ir))
                 (capital_to_be_paid cap_tb_paid (amortization pmt (interest cap_tb_paid ir)))
                 (capital cap_paid (amortization pmt (interest cap_tb_paid ir))) ir)]))

; test
;(amortization_table 1000000 12 25)


(define (amortization_plot)
  (plot (stacked-histogram lst_amortization_interest
                         #:labels '("Amortization" "Interest")
                         #:colors '(2 1)
                         #:line-colors '(2 1))
      #:legend-anchor 'top-right))
  
(define frame (new frame% [label "Amortization"]
                   [width 400] [height 400]))

(define loan_field (new text-field% [parent frame]
                        [label "Amount of total loan: "]))

(define months_field (new text-field% [parent frame]
                        [label "Number of months: "]))

(define ir_field (new text-field% [parent frame]
                        [label "Interest rate per year: "]))

(define (show_results button event)
  (amortization_table
   (string->number (send loan_field get-value))
   (string->number (send months_field get-value))
   (string->number (send ir_field get-value))))

(define (show_graph button event)
  (amortization_plot))

(new button% [parent frame]
             [label "Show results"]
             [callback show_results])

(define msg_results (new message% [parent frame]
                         [label "Results display here: "]))

(send frame show #t)