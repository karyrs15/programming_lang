#lang racket

(require racket/gui)
(require plot)
(plot-new-window? #t)


; Author: Karina Reyes

; Based on code from https://dev.to/goober99/learn-racket-by-example-gui-programming-3epm
(define number-field%
  (class text-field%
    ; Add init variables to define allowed range
    (init min-value max-value)
    (define min-allowed min-value)
    (define max-allowed max-value)
    (super-new)
    (define/override (on-focus on?)
      (unless on?
        (define current-value (string->number (send this get-value)))
        (unless (and current-value
                     (>= current-value min-allowed)
                     (<= current-value max-allowed))
          (send this set-value (~a min-allowed)))))))

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
  (send result_monthly_pmt set-label (string-append "Monthly payment: " (~r (payment loan months ir))))
  (send result_interest_pmt set-label (string-append "Total interest payment: " (~r (total_interest (total_payment loan months ir) loan))))
  (send result_total_pmt set-label (string-append "Total payment: " (~r (total_payment loan months ir)))))

(define (amortization_aux months months_count pmt actual_interest amt cap_tb_paid cap_paid ir)
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
  (cond
    [(empty? lst_amortization_interest) (send msg_errors set-label "*** ERROR: Introduce some data ***")]
    [else (plot (stacked-histogram lst_amortization_interest
                         #:labels '("Principal" "Interest")
                         #:colors '(2 1)
                         #:line-colors '(2 1))
      #:legend-anchor 'top-right
      #:title "Loan payments"
      #:x-label "Months"
      #:y-label "Amount"
      #:width 500
      #:height 500)]))
  
(define frame (new frame% [label "Amortization"]
                   [width 450] [height 300]))

(define loan_field (new number-field% [parent frame]
                        [label "Amount of total loan (0-100M): "]
                        [min-value 0]
                        [max-value 100000000]))

(define months_field (new number-field% [parent frame]
                        [label "Number of months (1-360): "]
                        [min-value 1]
                        [max-value 360]))

(define ir_field (new number-field% [parent frame]
                        [label "Interest rate per year (0-100): "]
                        [min-value 0]
                        [max-value 100]))

(define (show_results button event)
  (cond
    [(and (non-empty-string? (send loan_field get-value))
          (non-empty-string? (send months_field get-value))
          (non-empty-string? (send ir_field get-value)))
     (amortization_table
      (string->number (send loan_field get-value))
      (string->number (send months_field get-value))
      (string->number (send ir_field get-value)))]
    [else (send msg_errors set-label "*** ERROR: Introduce some data ***")]))

(define (show_graph button event)
  (amortization_plot))

(new button% [parent frame]
             [label "Show results"]
             [callback show_results])

(define panel (new vertical-panel% [parent frame]
                   [alignment (list 'left 'top)]
                   [min-width 400]))

(define msg_results (new message% [parent panel]
                         [label "RESULTS: "]))

(define result_monthly_pmt (new message% [parent panel]
                                [label "Monthly payment: "]
                                [min-width 400]))

(define result_interest_pmt (new message% [parent panel]
                                [label "Total interest payment: "]
                                [min-width 400]))

(define result_total_pmt (new message% [parent panel]
                                [label "Total payment: "]
                                [min-width 400]))

(define graph_btn (new button% [parent panel]
             [label "Show graph"]
             [callback show_graph]))

(define msg_errors (new message% [parent frame]
                         [label " "]
                         [min-width 400]))

(send frame show #t)