;;;
;;; Oblig 3b av henriktk
;;;
;;; OBS: alle endringer som gjøres i evaluator.scm oppsummeres nederst i filen

;;;
;;; Setup
;;;

(load "evaluator.scm")

(set! the-global-environment (setup-environment))

(mc-eval '(+ 1 2) the-global-environment)

;;;
;;; Oppgave 1 a)
;;;

(mc-eval '(define (foo cond else)
            (cond ((= cond 2) 0)
                  (else (else cond))))
         the-global-environment)
(mc-eval '(define cond 3) the-global-environment)

(mc-eval '(define (else x) (/ x 2)) the-global-environment)

(mc-eval '(define (square x) (* x x)) the-global-environment)

(mc-eval '(foo 2 square) the-global-environment)
(mc-eval '(foo 4 square) the-global-environment)
(mc-eval '(cond ((= cond 2) 0)
                (else (else 4))) the-global-environment)

;;Første utrykket evaluerer til 0. Dette er hardkodet i prosedyren
;;(define (foo cond else): hver gang 2 blir sendt som cond-argumentet
;;så returneres 0.
;;
;;Andre utrykk evaluerer til 16. Dette skjer ved at testen i foo feiles, ettersom
;;cond-argumentet er 4 og ikke 2. Følgene av dette er at else-argumentet evalueres
;;med cond-argumentet som input, altså (square 4).
;;
;;I det siste utrykket så vil testen i cond-utrykket sammenligne den definerte variablen
;;cond (som er satt til 3), mot den primitive integer 2. Denne testen vil alltid bli usann,
;;så da evalueres else. Ved å omskrive ser vi at (else (else 4)) => (else (/ 4 2)) => 2.
;;
;;Hele dette kan virke forvirrende dersom man ikke vet hvordan evaluatoren fungerer.
;;Når nye variabler (eller prosedyrer) blir definert, så lagres navnene som quotes/strings som bindes til en verdi.
;;Disse vil lagres i tagged-list, hvor man kan sjekke dem opp.
;;Et eksempel her er at i (define (square x) (* x x)) vil tilsvare at 'square blir bundet til (lambda (x)(* x x)).
;;I oppgaven vil cond og else evalueres utifra om de er special-forms eller quotes.


;;;
;;; Oppgave 2 b
;;;
;;; Velger å løse denne før a, ettersom denne kan brukes til å løse den.

;;; Ganske enkel prosedyre som legger til nye elementer til
;;; primitive-procedures ved bruk av set! og append.
;;; Viktig å huske å legge primitiven til som en variabel, slik at evluatoren
;;; behandler dem rett (noe som gjør at vi slipper "ERROR: Unbound variable"
(define (instal-primitive! id exp)
  (let ((exp2 (list 'primitive exp)))
    (set! primitive-procedures
          (append primitive-procedures
                  (list (list id exp))))
    (define-variable! id exp2 the-global-environment)))

;;;
;;; Tester
;;;

(instal-primitive! 'cube (lambda (x) (* x x x)))
(mc-eval '(cube 5) the-global-environment) ;125, riktig

;;;
;;; Oppgave 2 a
;;;

(instal-primitive! '1+ (lambda (x) (+ x 1)))
(instal-primitive! '1- (lambda (x) (- x 1)))

;;; Tester
(mc-eval '(1+ 521) the-global-environment) ;522, riktig
(mc-eval '(1- 42) the-global-environment) ;41, riktig

;;;
;;; Oppgave 3 a
;;;
;;; Prosedyren er definert nederst i evaluator.scm

(mc-eval '(and ) the-global-environment) ; #f, riktig
(mc-eval '(and 'hei 'dette 'er #f) the-global-environment) ; #f, riktig
(mc-eval '(and 1 23 'hei 23) the-global-environment) ; #t, riktig

(mc-eval '(or ) the-global-environment) ; #f, riktig
(mc-eval '(or #f #f #f #t) the-global-environment) ; #t, riktig
(mc-eval '(or 1 23 'hei 23) the-global-environment) ; #t, riktig

;;;
;;; Oppgave 3 b
;;;

;;; TODO

;;;
;;; Oppgave 3 c
;;;

(mc-eval '(let ((a 20)
                (b 5)
                (c 2)
                (d 4))
            (* (/ a b c) d))
         the-global-environment) ;; 8, riktig


;;;
;;; Oppgave 3 e
;;;

;;; TODO

