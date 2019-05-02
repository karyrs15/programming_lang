#lang racket

(require racket/gui)
(require plot)
(plot-new-window? #t) ; open a new window for every plot


;; Author: Karina Reyes
;; Purpose of program: Deploy a UI to help user to compute amortization results and plot them.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to compute results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Contract: payment : number number number -> number
; Purpose: to calculate the payment per month
; Definition: 
(define (payment loan months ir)
  (* (/ (* (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) (/ (/ ir 100.0) 12.0))
        (- (expt (+ 1.0 (/ (/ ir 100.0) 12.0)) months) 1.0)) loan))
; test:
; (payment 1000000 12 25)
; expected value 95044.20


; Contract: interest : number number -> number
; Purpose: to calculate the interest that still need to pay
; Definition:
(define (interest capital_to_be_paid ir)
  (* capital_to_be_paid (/ (/ ir 100.0) 12.0)))
; test:
; (interest 1000000 25)
; expected value 20833.33


; Contract: amortization : number number -> number
; Purpose: to calculate the amortization or principal payment
; Definition: 
(define (amortization payment interest)
  (- payment interest))


; Contract: capital_to_be_paid : number number -> number
; Purpose: to calculate the capital that stills needs to be paid
; Definition: 
(define (capital_to_be_paid last amortization)
  (- last amortization))


; Contract: capital : number number -> number
; Purpose: to calculate the cumulative capital that has already been paid
; Definition:
(define (capital last amortization)
  (+ last amortization))


; Contract: total_payment : number number number -> number
; Purpose: to calculate the sum of all the payments (including interest and principal)
; Definition:
(define (total_payment loan months ir)
  (* (payment loan months ir) months))


; Contract: total_interest : number number -> number
; Purpose: to calculate the total interest sum paid for the loan
; Definition:
(define (total_interest total_pmt loan)
  (- total_pmt loan))


; Define lists used to plot the stacked histogram
(define lst_months '())
(define lst_amortization_interest '())


; Contract: amortization_table : number number number -> number number number
; Purpose: to call amortization_aux function with the default and input values
; Definition:
(define (amortization_table loan months ir)
  (amortization_aux months 0 (payment loan months ir) 0 0 loan 0 ir)
  ; send results to show them in the UI results panel
  (send result_monthly_pmt set-label (string-append "Monthly payment: " (~r (payment loan months ir))))
  (send result_interest_pmt set-label (string-append "Total interest payment: " (~r (total_interest (total_payment loan months ir) loan))))
  (send result_total_pmt set-label (string-append "Total payment: " (~r (total_payment loan months ir)))))


; Contract: amortization_aux : number number number number number number number number
; Purpose: to calculate all the values used for the amortization computation
; Definition:
(define (amortization_aux months months_count pmt actual_interest amt cap_tb_paid cap_paid ir)
  (set! lst_months (append lst_months (list months_count))) ; add months count to list
  (set! lst_amortization_interest (append lst_amortization_interest (list (list months_count (list amt actual_interest))))) ; add interests and principal values to list
  (cond
    [(>= months_count months) #t]
    [else (amortization_aux months (+ months_count 1) pmt
                 (interest cap_tb_paid ir)
                 (amortization pmt (interest cap_tb_paid ir))
                 (capital_to_be_paid cap_tb_paid (amortization pmt (interest cap_tb_paid ir)))
                 (capital cap_paid (amortization pmt (interest cap_tb_paid ir))) ir)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to plot results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Plot stacked histogram using results lists
; If input fields are empty show error
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to deploy the UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Redifine text-field to validate min and max values on UI inputs
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

; Create a new frame window to show the whole UI
(define frame (new frame% [label "Amortization"]
                   [width 450] [height 300]))

; Create loan input field
(define loan_field (new number-field% [parent frame]
                        [label "Amount of total loan (0-100M): "]
                        [min-value 0]
                        [max-value 100000000]))

; Create months input field
(define months_field (new number-field% [parent frame]
                        [label "Number of months (1-360): "]
                        [min-value 1]
                        [max-value 360]))

; Create interest rate input field
(define ir_field (new number-field% [parent frame]
                        [label "Interest rate per year (0-100): "]
                        [min-value 0]
                        [max-value 100]))

; Call amortization_table function to compute results
; using the input values
; if button for results is clicked
; if input fields are empty, show error
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

; Call plotting funtion if button for graph is clicked
(define (show_graph button event)
  (amortization_plot))

; Make button to show results
(define showresults_btn (new button% [parent frame]
             [label "Show results"]
             [callback show_results]))

; Make panel for all the results
(define panel (new vertical-panel% [parent frame]
                   [alignment (list 'left 'top)]
                   [min-width 400]))

; Make message to show results title
(define msg_results (new message% [parent panel]
                         [label "RESULTS: "]))

; Make message to show monthly payment result
(define result_monthly_pmt (new message% [parent panel]
                                [label "Monthly payment: "]
                                [min-width 400]))

; Make message to show interest payment result
(define result_interest_pmt (new message% [parent panel]
                                [label "Total interest payment: "]
                                [min-width 400]))

; Make message to show total payment result
(define result_total_pmt (new message% [parent panel]
                                [label "Total payment: "]
                                [min-width 400]))

; Make button to show graph
(define graph_btn (new button% [parent panel]
             [label "Show graph"]
             [callback show_graph]))

; Make message field to show errors
(define msg_errors (new message% [parent frame]
                         [label " "]
                         [min-width 400]))

; Show the frame
(send frame show #t)