;Oblig 1 b
;Henrik Klev, INF2810

;Oppgave 1 a-e et løst på vedlagt fil

;Oppgave 1 f)
(define 1f '(0 42 #t bar))
(car (cdr 1f))

;Oppgave 1 g)
(define 1g '((0 42) (#t bar)))
(cdr (car 1g))

;Oppgave 1 h)
(define 1h '((0) (42 #t) (bar)))
(car (car (cdr 1h)))

;Oppgave 1 i)
(cons (cons 0 42)(cons #t 'bar))
(list (list 0 42)(list #t 'bar))
