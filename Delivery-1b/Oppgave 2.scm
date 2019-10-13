
;Oppgave 2 a)
;Den halerekursive prosedyren len skaper en iterativ prosess ved hjelp av prosedyren len-help.
;Eneste len-help egentlig gjør, er å sikre slik at count alltid begynner på 0.
(define (len items)
  (define (len-help items count)
    (if (null? items)
        count
        (len-help (cdr items)(+ 1 count))))
  (len-help items 0))


;Oppgave 2 b)
;Prosedyren er rekursiv, og skaper en iterativ prosess.
;Prosessen blir iterativ, fordi den finner en car items for hver iterasjon.
;Prosessen trenger ikke treffe rekursjonsbrønnen før car items kan regnes ut.
(define (reduce-reverse proc init items)
  (if (null? items)
      init
      (proc (reduce-reverse proc init (cdr items))
            (car items))))


;Oppgave 2 c
;Dersom listen er tom, så har den rekursert seg gjennom hele listen (gitt at den blir
;kalt med en ikke-tom liste). Om den har kommet seg gjennom hele, så har alle items
;gitt #t på predikatet. Om predikatet ikke stemmer på en item, så vil ikke 2. condition slå
;inn. Det vil da ikke skje noe rekursjon, og prosedyren vil gå til 3. condition som alltid 
;returnerer #f.
(define (all? pred items)
  (COND
    ((null? items) #t)
    ((pred (car items)) (all? pred (cdr items)))
    (#t #f)))


;Oppgave 2 d
;Prosedyren nth benytter seg av hjelpeprosedyren nth-help, som er den som gjør jobben.
;For hver iterasjon blir første leddet i items fjernet.
;Dette fortsetter n (= index) ganger.
(define (nth index items)
  (define (nth-help start items)
    (if (= index start)
        (car items)
        (nth-help (+ start 1) (cdr items))))
  (if(>= index (length items))
     (display '(ugyldig index))) 
  (nth-help 0 items))



;Oppgave 2 e
;Prosedyren rekurserer helt til listen er tom (item ikke funnet, returnerer #f), eller
;den har funnet item som letes etter (returnerer da count). 
(define (where key items)
  (define (where-help count items)
    (cond
      ((null? items) #f)
      ((= (car items) key) count)
      (#t (where-help (+ 1 count) (cdr items)))))
  (where-help 0 items))


;Oppgave 2 f
;Ganske lik map, men tar 2 argumenter i stedet for 1.
(define (map2 proc items1 items2)
  (if (or (null? items1) (null? items2))
      '()
      (cons (proc (car items1)(car items2))
            (map2 proc (cdr items1) (cdr items2)))))
   

;Oppgave 2 g
(display '(2 3 4 52:))
(map2 (lambda (x y)
        (/ (+ x y) 2))
      '(1 2 3 4)
      '(3 4 5 100))
      
;Oppgave 2 h
;Returnerer en anonym prosedyre som sjekker om begge argumentene stemmer overens med predikatet.
(define (both? pred)
  (lambda (x y)
    (and (pred x)(pred y))))


;Oppgave 2 i 
(define (self proc)
  (lambda (x)
    (proc x x)))



;Tests

;Test for 2a
(display '(11:))
(len '(1 2 3 #t quote bar 4 5 6 7 8))


;Test for 2b
(display '(6 5 4 3 2 1:))
(reduce-reverse cons '() '(1 2 3 4 5 6))

;Tests for 2c
(display '(false:)) (all? odd? '(1 2 3 5 7 9))
(display '(true:)) (all? odd? '(1 3 5 7 9))
(display '(true:))
(all? (lambda (x)
        (< x 10))
      '(1 2 5 8 9))
(display '(false:))
(all? (lambda (x)
        (< x 10))
      '(1 2 5 8 9 12))

;Test for 2d
(display '(13:))
(nth 3 '(47 11 12 13))

;Tests for 2e
(display '(false:))
(where 6 '(1 2 3 4 5))
(display '(2:))
(where 3 '(1 2 3 4 5))

;Test for 2f
(display '(2 5 6:))
(map2 + '(1 3 3) '(1 2 3 4)) 

;Tests for 2h
(display '(#f #t #f))
(map2 (both? even?) '(1 2 3) '(3 4 5))
(display '(true:))
((both? even?) 2 4)
(display '(false:))
((both? even?) 2 5)  


;Tests for 2i
(display '(10:))
((self +) 5)
(display '(25:))
((self *) 5)
(display '(<procedure>:))
(self +)
(display '("hello" "hello:"))
((self list) "hello")




