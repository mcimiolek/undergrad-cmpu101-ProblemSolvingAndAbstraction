;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Matthew Imiolek
;Lab 8
;Helps understand abstraction in programming



(define-struct toast (kind level))
; A toast is a
;   (make-toast string number[0 to 10])
; where where a toast’s kind can be any string, including (but not limited to) “white” or “wheat”.

; A list-of-toast (lot) is either
; - '()
; - (cons toast lot)

(define toast-list
  (list (make-toast "white" 0)
        (make-toast "wheat" 0)
        (make-toast "white" 4)
        (make-toast "wheat" 4)
        (make-toast "white" 6)
        (make-toast "wheat" 6)
        (make-toast "white" 10)
        (make-toast "wheat" 10)))

;count-bread : pred -> num
;sees which values meet a requirment, and returns the total number that do
(define (count-bread pred)
     (cond
       [pred (+ 1 (count-bread))]
       [else (count-bread pred)]))

;count-toast : lot string -> num
;counts the number of pieces of specific kind of bread
(define (count-toast lot typ)
  (cond
    [(empty? lot) 0]
    [else
     (cond
       [(string=? (toast-kind (first lot)) typ) (+ 1 (count-toast (rest lot) typ))]
       [else ( + 0 (count-toast (rest lot) typ))])]))
  
(check-expect (count-toast toast-list "wheat") 4)
(check-expect (count-toast toast-list "white") 4)
(check-expect (count-toast '() "white") 0)


;count-untoasted : lot -> num
;counts the number of untoasted pieces of bread
(define (count-untoasted lot)
  (count-bread (local [(define (un piece) (= (toast-level piece)  0))]
                 (cond
                   [(empty? lot) 0]
                   [else
                 (count-untoasted (rest lot))]))))

(check-expect (count-untoasted toast-list) 2)


;count-white : lot -> num
;counts the number of pieces of white bread
(define (count-white lot)
  (count-toast lot "white"))

(check-expect (count-white toast-list) 4)


;count-wheat : lot -> num
;counts the number of pieces of wheat bread
(define (count-wheat lot)
  (count-toast lot "wheat"))

(check-expect (count-wheat toast-list) 4)


;count-yummy : lot -> num
;counts the number of pieces of toast between 4 and 8 in level
(define (count-yummy lot)
  (count-bread (local [(define (yum piece) (and (> (toast-level piece)  4)(< (toast-level piece) 8)))]
                 (cond
                   [(empty? lot) 0]
                   [else
                 (count-untoasted (rest lot))]))))

(check-expect (count-yummy toast-list) 4)