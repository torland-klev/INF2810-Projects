;;henriktk INF2810 Oblig 3a

(load "prekode3a.scm")

;;
;;Oppgave 1 a
;;

(define (list-to-stream args)
  (if (null? args)
      the-empty-stream
      (cons-stream (car args) (list-to-stream (cdr args)))))

(define (stream-to-list args . int)
  (letrec ((local (lambda (stream i)
    (if (or (stream-null? stream)
            (= i 0))
        '()
        (cons (stream-car stream)
              (local (stream-cdr stream) (- i 1)))))))
  (if (pair? int)
      (local args (car int))
      (local args -1))))

;;
;;Tests
;;

(list-to-stream '(1 2 3 4 5))
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)


;;
;;Oppgave 1 b
;;
;;Oppgaveteksten sier "(...) avslutte rekursjonen så fort én av
;;input-strømmene er tomme." Lager en hjelpeprosedyre for å hjelpe
;;oss med dette. Prosedyren check-null? itererer gjennom input-strømmene
;;og sjekker om en er null.

(define (check-null? stream)
  (cond ((null? stream) #f)
        ((null? (car stream)) #t)
        (else (check-null? (cdr stream)))))

(define (stream-map proc . argstreams)
  (if (check-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define test1 (list-to-stream '("zzz" "hei" "hva" )))
(define test2 (list-to-stream '("mitt" "navn" "er" "Kjell" "Rune" "Olsen")))
(stream-to-list (stream-map string<=? test1 test2))

;;
;:Oppgave 1 c
;;

;;Problemet med å løse det slik er at det tar ikke høyde for
;;uendelige strømmer. Prosedyren funker ved å fjerne det første
;;elementet i listen dersom det kommer senere i listen. Tanken
;;er da at du til slutt sitter igjen med en liste hvor hvert
;;element er det siste eksemplaret. Dette funker ikke med uendelige
;;strømmer, ettersom du vil aldri kunne bevise at det aldri
;;kommer et duplikat senere i strømmen. En enkel løsning vil være
;;å gjøre det omvendt: legg til et element fra strømmen i en ny liste
;;dersom elementet ikke ligger i listen fra før.

;;
;;Oppgave 1 d
;;

(define x
  (stream-map show
              (stream-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 7)

;;Utskriften kommer av at stream-map tar bruk av cons-stream som bruker
;;prosedyren delay. Delay er memorisert, så derfor får vi en memorisering
;;av stream-ref. Det gjør igjen at vi ikke får de ønskede side-effektene
;;som show gir oss.
