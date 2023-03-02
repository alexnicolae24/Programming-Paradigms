#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (cond
    ((null? X) 0);daca X este null se returneaza 0
    ((null? Y) 0);daca Y este null se returneaza 0
    ((+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y))))));daca cele doua argumente nu sunt null,se calculeaza produsul scalar recursiv


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (multiply-helper M V '()))
  
(define (multiply-helper M V acm)
  (if (= 0 (length M))
      (reverse acm) ;daca lungimea lui M este zero,se retine lista vida in acm
      ;(cons (dot-product (car M) V) (multiply-helper (cdr M) V acm))));altfel,se construieste rezultatul inmultirii in mod recursiv
      (multiply-helper (cdr M) V (cons (dot-product (car M) V) acm))))

; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (get-transformations n)
  (get-transformations-helper n '()))

(define (get-transformations-helper n acm)
  (cond
    ((= 1 n) acm);daca n este 1,se retine in acm lista vida
    ((= 1 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 1 acm)));se observa ca in functie de rezultatul lui (n-1) modulo 3,se retine in acm 1,2 sau 3,
    ((= 2 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 2 acm)));iar apoi se urca la parintele nodului,ce se calculeaza ca fiind rezultatul lui (n+1) div 3
    ((= 0 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 3 acm)))))
       
      
      


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă
(define (apply-matrix-transformations Ts ppt)
  (apply-matrix-transformations-helper Ts ppt '()))

(define (apply-matrix-transformations-helper Ts ppt acm)
  (cond
    ((null? Ts) acm);daca lista de transformari Ts este nula se retine in acm lista vida
    ((= (car Ts) 1) (apply-matrix-transformations-helper (cdr Ts) (multiply T1 ppt) (multiply T1 ppt)));daca elementul din Ts este 1 se aplica T1,la fel si pentru 2 si 3
    ((= (car Ts) 2) (apply-matrix-transformations-helper (cdr Ts) (multiply T2 ppt) (multiply T2 ppt)))
    ((= (car Ts) 3) (apply-matrix-transformations-helper (cdr Ts) (multiply T3 ppt) (multiply T3 ppt)))))
    


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (if(= n 1)
     '(3 4 5);daca n este 1,se returneaza lista '(3 4 5)
     (apply-matrix-transformations (get-transformations n) '(3 4 5))));;altfel,se aplica functiile de la exercitiile 3 si 4
