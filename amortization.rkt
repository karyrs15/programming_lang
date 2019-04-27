#lang racket

; Author: Karina Reyes

; Contract: pmt : number number number -> number
; Purpose: to calculate the payment per month
; Definition: 
(define (pmt p n i)
  (* (/ (* (expt (+ 1.0 (/ (/ i 100.0) 12.0)) n) (/ (/ i 100.0) 12.0))
        (- (expt (+ 1.0 (/ (/ i 100.0) 12.0)) n) 1.0)) p))

; test
(pmt 1000000 12 25)


(define (interest capital_to_be_paid i)
  (* capital_to_be_paid (/ i 12.0)))

; test
(interest 1000000 25)

(define (amortization pmt interest)
  (- pmt interest))

; test
(amortization 95044.20 20833.33)

(define (capital_to_be_paid last amortization)
  (- last amortization))

; test
(capital_to_be_paid 1000000 74210.87)

(define (capital last amortization)
  (+ last amortization))

; test
(capital 0 74210.87)

