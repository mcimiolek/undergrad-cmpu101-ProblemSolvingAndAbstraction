;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Matthew Imiolek
;Lab 9
;This program will help understand higher order functions with lambda expressions


; build-list1 : NN -> list-of-NN
; creates the list '(0 ... (- n 1)) for any NN n
(define (build-list1 n)
  (build-list n (lambda (m) m)))

(check-expect (build-list1 1) '(0))
(check-expect (build-list1 5) '(0 1 2 3 4))


; build-list2 : NN -> list-of-NN
; creates the list '(1 2... n) for any NN n
(define (build-list2 n)
  (build-list n (lambda (m) (+ m 1))))

(check-expect (build-list2 1) '(1))
(check-expect (build-list2 5) '(1 2 3 4 5))


; build-list3 : NN -> list-of-NN
; creates the list '(1/2 1/3 ... 1/n) for any NN n
(define (build-list3 n)
  (build-list n (lambda (m) (/ 1 (+ 1 m)))))

(check-expect (build-list3 1) '(1))
(check-expect (build-list3 5) '(1 1/2 1/3 1/4 1/5))


; build-list4 : NN -> list-of-NN
; creates the list '(0 2 4... 2n) for any NN n
(define (build-list4 n)
  (build-list n (lambda (m) (* m 2))))

(check-expect (build-list4 1) '(0))
(check-expect (build-list4 5) '(0 2 4 6 8))


; append-from-fold : list list -> list
; appends each list in the list-of-lists
(define (append-from-fold lon1 lon2)
  (foldr cons lon2 lon1))

(check-expect (append-from-fold (list 1) (list 2)) (list 1 2))


; map-via-fold : list -> list
; simulates map using flodr
(define (map-via-fold f lon1)
  (foldr (lambda (x y) (cons (f x) y)) '() lon1))

(check-expect (map-via-fold add1 (list 1 2 3 4 5)) (list 2 3 4 5 6))