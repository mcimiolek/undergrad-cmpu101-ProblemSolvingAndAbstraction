;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;------------------------------------------------------------------------
; CMPU-101 Assignment 3 
; Fall 2016
; Matthew Imiolek
; Description: Permutations
;   Find all the permutations of a given list of numbers
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; Data Definitions
;------------------------------------------------------------------------

; A list-of-num (LON) is either
; - '()
; - (cons num LON)

; A list-of-LON (LOLON) is either
; - '()
; - (cons LON LOLON)



; permutations: LON -> LOLON
; to create a list of all permutations of the numbers in lon
(define (permutations lon)
  (cond
    [(empty? lon) (list '())]
    [(cons? lon) (insert-all-lons (first lon)
                                                (permutations (rest lon)))]))

(check-expect (permutations '()) (list '()))
(check-expect (permutations '(1)) (list '(1)))
(check-expect (permutations '(1 2)) (list '(1 2) '(2 1)))
(check-expect (permutations '(1 2 3)) (list '(1 2 3) '(2 1 3) '(2 3 1)
                                            '(1 3 2) '(3 1 2) '(3 2 1)))


; You continue designing/implementing/testing the following functions,
; in order. You should not need any additional functions.


; insert-all-lons: num LOLON -> LOLON
; Inserts given number at the beginning, between all numbers, and at the end 
; of all LONs in the given list of lists of numbers.
(define (insert-all-lons n lolon)
  (cond
    [(empty? lolon) '()]
    [(cons? lolon) (append (insert-one-lon n (first lolon))(insert-all-lons n (rest lolon)))]))

(check-expect (insert-all-lons 1 (list '())) (list '(1)))
(check-expect (insert-all-lons 2 (list '(1))) (list '(2 1) '(1 2)))
(check-expect (insert-all-lons 3 (list '(1 2))) (list '(3 1 2) '(1 3 2) '(1 2 3)))
(check-expect (insert-all-lons 3 (list '(1 2) '(2 1))) (list '(3 1 2) '(1 3 2) '(1 2 3) '(3 2 1) '(2 3 1) '(2 1 3)))

; insert-one-lon: num LON -> LOLON
; Returns a list of numbers with the given number inserted in front of, 
; in between, and at the end of each number in the given list of numbers.
(define (insert-one-lon n lon)
  (cond
    [(empty? lon) (list (list n))]
    [(cons? lon) (append (list (cons n lon)) (insert-beginning-all-lons (first lon) (insert-one-lon n (rest lon))))]))

(check-expect (insert-one-lon 1 '()) (list '(1)))
(check-expect (insert-one-lon 2 '(1)) (list '(2 1) '(1 2)))
(check-expect (insert-one-lon 3 '(1 2)) (list '(3 1 2) '(1 3 2) '(1 2 3)))

; insert-beginning-all-lons: num LOLON -> LOLON
; Inserts the given number at the beginning of each LON in given list of LONs.
(define (insert-beginning-all-lons n lolon)
  (cond
    [(empty? lolon) '()]
    [(cons? lolon) (cons (cons n (first lolon)) (insert-beginning-all-lons n (rest lolon)))]))

(check-expect (insert-beginning-all-lons 1 (list '())) (list '(1)))
(check-expect (insert-beginning-all-lons 2 (list '(1))) (list '(2 1)))
(check-expect (insert-beginning-all-lons 3 (list '(1 2))) (list '(3 1 2)))
(check-expect (insert-beginning-all-lons 3 (list '(1 2) '(2 1))) (list '(3 1 2) '(3 2 1)))
