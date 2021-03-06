;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
 
; CS101 Lab 1
; Name: Matthew Imiolek
; Description: evaluating different types of data expressions 
 
; Numbers are a type of data
; --the following expression adds numbers
(+ 40 1 1)
 
; Strings are a type of data (strings of characters)
; --the following expression appends strings
(string-append "Computer " "Science")
 
; Booleans are a type of data (true or false)
; --the following expression compares two numbers 
(> 42 9)
 
; Images are a type of data
; --shapes are one type of image; 
(circle 10 "solid" "red");
(overlay(triangle/sss 200 80 140 "solid" "lightgreen")
        (triangle/sss 250 100 175 "solid" "green"));
 
; Images may be combined
 
; --the following expression overlays a circle on a square
; --the result is another image
(overlay (rectangle 20 20 "solid" "blue")
         (circle 5 "outline" "red"))
 
; Scenes are a type of data
; --the following expression creates an empty scene
(empty-scene 100 100)
 
; Images may be placed within scenes
; --the following expression places a circle in an empty scene
; --the result is another scene
(place-image (circle 10 "solid" "red")
             25 25
             (empty-scene 100 100))