;;INF2810 - Innlevering 1a
;;Brukernavn: henriktk

;;Oppgave 1


;;Deloppgave a
(* (+ 4 2) 5)
;;Evaluerer til 30.

;;Deloppgave b
;;(* (+ 4 2) (5))
;;Syntaxfeil, forventer en prosedyre inne i parantesen hvor det står 5.
;;Riktig syntax er som i deloppgave a

;;Deloppgave c
;;(* (4 + 2) 5)
;;Syntaxfeil, riktig syntax er som i deloppgave a

;;Deloppgave d
(define bar (/ 42 2))
bar
;;Evaluerer til 21

;;Deloppgave e
(/ (* bar 3 4 1) bar)
;;Evaluerer til 12, kan forenkles til (* 3 4).


;;Oppgave 2

;;Deloppgave a-1
;;Skriver ut "piff!" grunnet det er den første som er true.
;;Dette er en spesial case ettersom prosedyren "or" stopper etter første true
;;argument (dersom et slik finnes), og evaluerer ikke resten av argumentene.

;;Deloppgave a-2
;;Skriver ut #f grunnet 1 != 2. Prosedyren "and" stopper ved første false argument
;;(om det finnes), og skriver ut #f. Prosedyren er en special case siden den ikke
;;evaluerer resten av argumentene i et slikt scenario.

;;Deloppgave a-3
;;Skriver ut i-am-undefined, fordi det forventes en definert prosedyre mens
;;prosedyren i-am-undefined er udefinert.

;;DelOppgave b

(define (sign-if x)
  (if (positive? x)
      1
      (if (= x 0)
          0
          -1)))

(define (sign-cond x)
  (cond
    ((positive? x) 1)
    ((zero? x) 0)
    ((negative? x) -1)))

;;Deloppgave c
(define (sign x)
  (or
   (and (> x 0) 1)
   (and (= x 0) 0)
   -1))

;;Test av sign-if, sign-cond, og sign
;;(sign-if 10)
;;(sign-if 0)
;;(sign-if -10)
;;(sign-cond 10)
;;(sign-cond 0)
;;(sign-cond -10)
;;(sign 10)
;;(sign 0)
;;(sign -10)



;;Oppgave 3

;;Deloppgave a
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))

;;Deloppgave b
(define (plus x y)
  (if (or (negative? x) (negative? y))
      "Bare positive tall takk!"
      (if (zero? y)
          x
          (plus (add1 x) (sub1 y)))))

;;Deloppgave c

;;Prosedyren i deloppgave b oppretter en rekursiv prosess som ser slik ut: 

(plus 4 3)
;;(plus (plus 5 2))
;;(plus (plus (plus 6 1)))
;;(plus (plus (plus (plus 7 0))))
;;(plus (plus (plus 7 0)))
;;(plus (plus 7 0))
;;(plus 7 0)
;;7

;;En annen rekursiv prosedyre kan være på følgende vis:

(define (plus-iterative x y)
  (plus-iter x y 1))

(define (plus-iter x y c)
  (if (or (negative? x) (negative? y))
      "Bare positive tall takk!"
      (if (< y c)
          x
          (plus-iter (add1 x) y (add1 c)))))

;;Den iterative prosessen vil da se slik ut:

(plus-iterative 4 3)
;;(plus-iter 4 3 1)
;;(plus-iter 5 3 2)
;;(plus-iter 6 3 3)
;;(plus-iter 7 3 4)
;;7

;;Deloppgave d
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))

(power-close-to 2 512)
;;Dette forenkler definisjonen av hjelpe-prosedyren. Slipper gjentagelse
;;av hovedprosedyrens to argumenter, noe som er mulig ettersom de ikke endrer seg.

;;Deloppgave e
;;Nei, det er ikke mulig å forenkle prosedyren. Dette er fordi at eneste argument
;;som er felles i hoved- og hjelpeproseduren, count (også kalt n), endrer seg
;;for hver iterasjon.