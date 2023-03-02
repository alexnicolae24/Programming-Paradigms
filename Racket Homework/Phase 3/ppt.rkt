#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;;Am preluat din checker aceste functii ce aplica functiile Q1,Q2,Q3 pe un set de valori
(define (FQ1 V) (apply Q1 V))
(define (FQ2 V) (apply Q2 V))
(define (FQ3 V) (apply Q3 V))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (apply + (map * X Y)))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|

(define (dot-product-by Q);am implementat o functie auxiliara ce ajutala efectuarea produsului scalar cu un vector dat 
  (lambda (X)
    (dot-product X Q)))

(define (multiply M V)
    (map (dot-product-by V) M))

(define (curry-multiply)
  (lambda(M)
    (lambda(V)
      (multiply M V))))





; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
(define (get-transformations n)
  (get-transformations-helper n '()))

(define (get-transformations-helper n acm)
  (cond
    ((= 1 n) acm);daca n este 1,se retine in acm lista vida
    ((= 1 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 1 acm)));se observa ca in functie de rezultatul lui (n-1) modulo 3,se retine in acm 1,2 sau 3,
    ((= 2 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 2 acm)));iar apoi se urca la parintele nodului,ce se calculeaza ca fiind rezultatul lui (n+1) div 3
    ((= 0 (modulo (- n 1) 3)) (get-transformations-helper (quotient (+ n 1) 3) (cons 3 acm)))))


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (help-func f acm) ;functie ajutatoare ce ajuta la aplicarea ulterioara a lui foldl
  (lambda(f acm) (f acm)))

(define (apply-functional-transformations Fs tuple)
   (foldl (help-func Fs tuple) tuple Fs))
           

; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
(define (get-nth-tuple tuple) ;am dat parametrul tuple,deoarece plecand de la un tuple de baza se vor aplica functiile corespunzatoare                      
  (lambda (func)
      (apply-functional-transformations func tuple)))



; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (func-T L1 lst acm);functie ce returneaza o lista formata din functiile T ce vor fi aplicate pe tripletul initial pentru a se forma tripletul cerut
   (cond
     ((null? L1) (reverse acm))
     ((= (car L1) 1) (func-T (cdr L1) lst (cons ((lambda(M) ((curry-multiply) M)) (car lst)) acm)))
     ((= (car L1) 2) (func-T (cdr L1) lst (cons ((lambda(M) ((curry-multiply) M)) (cadr lst)) acm)))
     ((= (car L1) 3) (func-T (cdr L1) lst (cons ((lambda(M) ((curry-multiply) M)) (caddr lst)) acm)))))
       
(define (get-nth-ppt-from-matrix-transformations n)
  ((get-nth-tuple '(3 4 5)) (func-T (get-transformations n) (list T1 T2 T3) '())))
   

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (func-Q L2 lis acm);functie ce returneaza o lista formata din functiile T ce vor fi aplicate pe cvartetul initial pentru a se forma cvartetul cerut
   (cond
     ((null? L2) (reverse acm))
     ((= (car L2) 1) (func-Q (cdr L2) lis (cons (car lis) acm)))
     ((= (car L2) 2) (func-Q (cdr L2) lis (cons (cadr lis) acm)))
     ((= (car L2) 3) (func-Q (cdr L2) lis (cons (caddr lis) acm)))))
       
(define (get-nth-quadruple n)
  ((get-nth-tuple '(1 1 2 3)) (func-Q (get-transformations n) (list FQ1 FQ2 FQ3) '())))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
;a = gh,   b = 2ef,   c = e^2 + f^2
;(define (list-func L) 
(define (get-nth-ppt-from-GH-quadruples n);am aplicat formulele de mai sus pe elementele cvartetului returnat de get-nth-quadruple
  (list (* (car (get-nth-quadruple n)) (cadddr (get-nth-quadruple n)))
           (* 2 (cadr (get-nth-quadruple n)) (caddr (get-nth-quadruple n)))
                (+ (expt (cadr (get-nth-quadruple n)) 2) (expt (caddr (get-nth-quadruple n)) 2))))
