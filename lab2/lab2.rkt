;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CS101 Lab 2
; Name: Matthew Imiolek
 
; Exercise 1:
; Define the function letter-grade that consumes a number between 
; 0 and 100, and produces a letter grade ("A", "B", "C", "D", "F") 
; using the usual 90, 80, 70, 60 cutoffs.

; letter-grade : num -> string
; converts a grade as a percent into a letter grade
(define (letter-grade x)
  (cond
    [(>= x 90) "A"]
    [(>= x 80) "B"]
    [(>= x 70) "C"]
    [(>= x 60) "D"]
    [else "F"]
    ))
(check-expect (letter-grade 90) "A")
(check-expect (letter-grade 80) "B")
(check-expect (letter-grade 85) "B")
(check-expect (letter-grade 70) "C")
(check-expect (letter-grade 60) "D")
(check-expect (letter-grade 63) "D")
(check-expect (letter-grade 50) "F")

  
 
; Exercise 2:
; Define the function pass? that determines whether the given number
; grade is a passing grade (60 or higher).

; pass :  num -> boolean
; Determines whether the given number grade is a passing grade
(define (pass y)
  (cond
    [(>= y 60) true]
    [else false]))
(check-expect (pass 60) true)
(check-expect (pass 44) false)
 
; Exercise 3:
; Define the function pass-fail that consumes a number grade and 
; determines either "pass" or "fail". Use the pass? predicate function
; from Exercise 2.

;pass-fail : num -> string
;Consumes a number grade and determines either "pass" or "fail".
(define (pass-fail z)
  (cond
    [(pass z) "pass"]
    [else "fail"]))

(check-expect(pass-fail 60) "pass")
(check-expect(pass-fail 44) "fail")
 
; Exercise 4:
; Define the function NRO-grade, which consumes a number grade and a 
; minimum numeric grade between 0 and 100, and produces either a 
; letter grade, "PA", or "F", according to Vassar's NRO rules. You
; are encouraged to use one or more functions from Exercises 1-3 as
; helper functions.

; NRO-grade : num -> string
;consumes a number grade and a minimum numeric grade between
;0 and 100, and produces either a letter grade, "PA", or "F",
;according to Vassar's NRO rules.
(define (NRO-grade min act)
  (cond
    [(>= act min) (letter-grade act)]
    [(pass act) "PA"]
    [else "F"]))

(check-expect(NRO-grade 80 60) "PA")
(check-expect(NRO-grade 80 90) "A")
(check-expect(NRO-grade 80 50) "F")