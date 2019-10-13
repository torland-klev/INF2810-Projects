;;
;;Hjelp
;;

(define (lookup-sentence key sen)
  (if (and key sen)
      (let ((record (assoc key sen)))
        (if record
            (cdr record)
            #f))
      #f))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

(define (1d-put! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table (cons (cons key value) (cdr table))))))


(define (2d-put! x y value table)
  (let ((subtable (assoc x (cdr table))))
    (if subtable
        (1d-put! y value subtable)
        (let ((new-table (make-lm)))
          (1d-put! y value new-table)
          (1d-put! x (cdr new-table) table)))))

;;
;;Oppgave 2 a
;;
(define (make-lm)
  (list 'Language-Modell))

(define (lm-lookup-bigram table x y)
  (let ((subtable (assoc x (cdr table))))
    (if subtable
        (lookup y subtable)
        #f)))

(define (lm-record-bigram! table x y)
  (let ((value (lm-lookup-bigram table x y)))
    (if value
        (2d-put! x y (+ value 1) table)
        (2d-put! x y 1 table))))
;;
;;Tests 2 a
;;

;(define lm (make-lm))
;(1d-put! 'a 1 lm)
;(2d-put! 'first 'second 1 lm)
;(2d-put! 'first 'third 3 lm)
;lm
;(lm-lookup-bigram lm 'first 'third)
;(lm-record-bigram! lm 'first 'third)
;(lm-record-bigram! lm 'first '4th)
;(lm-record-bigram! lm '3rd '4th)
;lm
       
;;
;;Oppgave 2 b
;;
;;Dersom jeg får tid skal jeg prøve gjøre denne litt penere. Klokka er 02:23 nå, og
;;jeg er alt for opphopet på koffein til å lage pen kode. Dette gjør hvertfall jobben.
;;Har valgt å legge inn en ekstra frequency table foreløpig, ettersom jeg har enda ikke
;;funnet ut av hvordan jeg kan lage et binærtre, og få det til å oppdatere seg
;;selv hver gang jeg legger inn et nytt element.

;;TODO: Kom nå på at freq-table kan gjøres mye lettere, ettersom om
;;man summerer alle ordparfrekvensene til et gitt ord, så
;;finner man hvor mange ganger det ordet står først (ergo frekvensen).

(load "prekode3a.scm")
(define corpus-brown (read-corpus "brown.txt"))
(define corpus-test (read-corpus "test.txt"))

(define freq-table
  (list 'FREQ))

(define (update-freq! table key)
  (let ((record (lookup key table)))
    (if record
        (1d-put! key (+ 1 record) table)
        (1d-put! key 1 table))))
        

(define (lm-train! words model)
  (letrec ((local! (lambda (sen)
                    (if (> 2 (length sen))
                        (update-freq! freq-table (car sen))
                        (begin (update-freq! freq-table (car sen))
                               (lm-record-bigram! model (car sen) (cadr sen))
                               (local! (cdr sen)))))))
    (if (null? words)
        'done
        (begin (local! (car words))
               (lm-train! (cdr words) model)))))


;;
;;Tests 2 b
;;

(define lm (make-lm))
(lm-train! corpus-test lm)
;(lm-lookup-bigram lm "dismissed," "as") ;;4

;(update-freq! freq-table 'hei)
;(update-freq! freq-table 'hei)
;(update-freq! freq-table 'lol)


;;
;;Oppgave 2 c
;;
;;Denne prosedyren har to rekursive lokale prosedyrer.
;;Den første rekurserer om en nøkkel kun har et element (kun et par), mens
;;den andre rekurserer om en nøkkel har flere elementer (flere etterfølgere)
;;helt til den kommer til det siste elementet.
;;Har en veldig sterk følelse av at det finnes en mye lettere måte
;;å gjøre dette på, skal se på det om jeg får tid. Men dette
;;løser hvertfall oppgaven (uten bruk av bst).

(define (math x y)
  (/ x (lookup y freq-table)))

(define (lm-estimate lm)
  (let ((new-table (make-lm)))
    (letrec ((local! (lambda (t)
                       (if (null? t)
                           new-table
                           (if (< 2 (length (car t)))
                               (let ((key (caar t)))
                                 (letrec ((local2! (lambda (u)
                                                     (if (null? u)
                                                         (local! (cdr t))
                                                         (begin
                                                           (2d-put! key (caar u) (math (cdar u) key) new-table)
                                                           (local2! (cdr u)))))))
                                   (local2! (cdar t))))
                               (begin (2d-put! (caar t) (caadar t) (math (cdadar t) (caar t)) new-table)
                                      (local! (cdr t))))))))
      (local! (cdr lm)))))


;;
;;Tests 2 c
;;
(define newls (lm-estimate lm))
;newls


;;
;;Oppgave 2 d
;;

;;Har forstått det slik at man skal multiplisere 'ekte' frekvensen til alle
;;ordparene i en setning.

;;Lagde en ny hjelpeprosedyre som sjekker om et ord er i en setning.
;;Den tidligere lookup-prosedyren sjekke ikke det første elementet
;;i en setning, ettersom den regnte med det var nøkkelen.

;;lm-score kjører så lenge det er mer enn 2 elementer i setningen,
;;fordi man trenger to elementer i et par.

;:lm-score har en if-test som blir true dersom:
;; 1. (lookup (car sen) lm blir true, noe som vil si at ordet finnes i ordmodellen.
;; 2. (lookup-sentence (cadr sen)... blir true, noe som vil si at ordparet (car sen, cadr sen)
;;     finnes i ordmodellen.

;;Dersom if-testen er true, så vil score oppdateres med den 'ekte' frekvensen
;;til ordparet (car sen, cadr sen).

(define (lm-score lm sentence)
  (letrec ((score 0)(local! (lambda (sen)
                              (if (> 2 (length sen))
                                  score
                                  (begin
                                    (if (lookup-sentence (cadr sen) (lookup (car sen) lm))
                                        (set! score (+ score (lookup-sentence (cadr sen) (lookup (car sen) lm)))))
                                    (local! (cdr sen)))))))
    (local! sentence)))

;Test
;(lm-score newls '("<s>" "As" "the" "court"))

        
;;Oppgave 2 e) vil gjøre at jeg må skrive om alle prosedyrene mine.
;;Prøver derfor å løse den i en egen fil.

