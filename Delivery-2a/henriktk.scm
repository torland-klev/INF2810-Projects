;Henriktk obligatorisk innlevering 2a

;--------------- Oppgave 1 start ---------------

;Oppgave 1a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car items)
  (items (lambda (x y) x)))

(define (p-cdr items)
  (items (lambda (x y ) y)))

(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))


;Oppgave 1b

;For interpreteren er disse to identiske:
;((lambda (var1 var2...) body) exp1 exp2...)
;(let ((var1 exp1) (var2 exp2)...) body)

(define foo 42)

;De to neste er identiske, begge evaluerer til 'different.
(let ((foo 5)
      (x foo))
  (if (= x foo)
      'same
      'different))

((lambda (foo x)
   (if (= x foo)
       'same
       'different))
 5 foo)

;De to neste er identiske, begge evaluerer til (towel (42 towel)).
(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

((lambda (bar baz)
   ((lambda (bar foo)
   (list foo bar))
 (list bar baz) baz))
 foo 'towel)

;Oppgave 1c

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))

(define (infix-eval exp)
  ((lambda (x y z)
     (y x z))
   (car exp) (car (cdr exp)) ( car (cdr (cdr exp)))))

(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

;Oppgave 1d
(define bah '(84 / 2))
;(infix-eval bah)

;Her får vi error, ettersom andre element i bah ikke er en prosedyre, men tegnet '/.
;Dette kommer ifra quote-tegnet (').
;Dette kan vi se dersom vi får kaller:
;(car (cdr bah))
;Vi får da ikke #<procedure:/> som vi får når vi kaller:
;(car (cdr bar))
;Vi ser det også dersom vi gjør slik:
;(equal? (car (cdr bah)) (car (cdr bar)))
;at her er ikke elementene like.

;--------------- Oppgave 1 ferdig ---------------


;--------------- Oppgave 2 start ---------------

;Note: flere av prosedyrene her kompilerer ikke i denne fila. De prosedyrene det gjelder vil
;kompilere i huffman.scm-fila.

;Oppgave 2a
(define (member? proc element items)
  (if (null? items)
      #f
      (if (proc element (car items))
          #t
          (member? proc element (cdr items)))))

(member? eq? 'zoo '(bar foo zap))
(member? eq? 'foo '(bar foo zap))
;(member? = 'foo '(bar foo zap))
(member? = 1 '(3 2 1 0))
(member? eq? '(1 bar)
         '((0 foo) (1 bar) (2 baz)))
(member? equal? '(1 bar)
         '((0 foo) (1 bar) (2 baz)))

;Oppgave 2b
;Den interne prosedyren kalles med de samme argumentene som hovedprosedyren kun
;ved første kall. De påfølgende kallene på den interne prosedyren vil ikke nødvendigvis
;ha samme argumenter.

;Oppgave 2c
;Denne kommende prosedyren decode-it vil ikke kompilere i denne fila, men vil kompilere i huffman.scm
;Hjelpeprosedyren reverse-list vil brukes videre i oppgaven.

(define (decode-it bits tree)
  (define (decode-1 symbols bits current-branch)
    (if (null? bits)
        (reverse-list symbols)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cons (symbol-leaf next-branch) symbols ) (cdr bits) tree) 
              (decode-1 symbols (cdr bits) next-branch)))))
  (decode-1 '() bits tree))

(define (reverse-list items)
  (if (null? items)
      '()
      (append (reverse-list (cdr items)) (list (car items)))))
          
;Oppgave 2d

;Valgte å ikke legge inn alle funksjonene i denne fila, og kjørte istedet kommandoen i
;huffman.scm. Fikk da resultatet (ninjas fight ninjas by night). Får samme resulat
;av ?(decode-it sample-code sample-tree).

;Oppgave 2e
;Kommentarer til koden:
;
;1. Looper så lenge det er meldinger igjen å lese
;2. Sjekker om item er del av venstre subtre
;2a. Sjekker om vi er ved løvnode
;2a1. Om vi er ved løvnode har vi funnet item. Rekurserer med resterende meldinger, originale treet, og legger på 0 til bits
;2a2. Om ikke løvnode, rekurserer med samme meldinger, på det venstre subtreet, og legger på 0 til bits
;3. Samme prosedyre som 2, bare med høyre subtre
;4. Om ikke del av høyre eller venstre subtre, så er ikke item i treet.

(define (encode message tree)
  (define (encode1 message cur-tree bits)
    (if (null? message);
        (reverse-list bits)
        (if (member? equal? (car message) (symbols (left-branch cur-tree)));
            (if (leaf? (left-branch cur-tree));
                (encode1 (cdr message) tree (cons 0 bits))
                (encode1 message (left-branch cur-tree) (cons 0 bits)))
            (if (member? equal? (car message) (symbols (right-branch cur-tree)))
                (if (leaf? (right-branch cur-tree))
                    (encode1 (cdr message) tree (cons 1 bits))
                    (encode1 message (right-branch cur-tree) (cons 1 bits)))  
                '(error: (car message) ikke i treet)))))
  (encode1 message tree '()))
      
;Oppgave 2f
;Brukt psydokode fra her: http://cseweb.ucsd.edu/~kube/cls/100/Lectures/lec8/lec8-15.html

(define (grow-huffman-tree tree)
  (define (grow-huffman list)
    (if (> 2 (length list))
        (car list)
        (grow-huffman (adjoin-set (make-code-tree (cadr list) (car list)) (cddr list)))))
  (grow-huffman (make-leaf-set tree)))

;Oppgave 2g
;Valgte å lage prosedyrer for hver deloppgave.
;Ble litt ekstra trening.

(define list '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2)
                                 (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2)
                                 (forest 1) (wait 1) (poison 1)))
(define message '(ninjas fight
                         ninjas fight ninjas
                         ninjas fight samurais
                         samurais fight
                         samurais fight ninjas
                         ninjas fight by night))

(define codetree (grow-huffman-tree list))

;Del 1
;Svaret blir 43
(length (encode message codetree))
;Del 2
;Svaret blir 9*(16/17)

(define (average-length tree decoded-message)
  (/ (weight tree) (length decoded-message)))

(average-length codetree message)
;Del 3
;Svaret blir 4*16=64, fordi du trenger (base-log-2 av antall ord opprundet til
;nærmeste heltall) bits for å skrive ett ord med fixed-length.
;Multipliser det tallet med antall ord i setningen, så får du svaret.
;Her har vi 16 ord, så vi trenger 4 bits per ord. Har vi 17 ord trenger
;vi 5 bits per ord. 33-64 ord trenger 6 bits per ord, osv.
(define (fixed-length message)
  (if (= (length message) 0)
      0
      (let ((base-log-2 (lambda (n)
                          (/ (log n) (log 2)))))
        (letrec ((round-up (lambda (double i)
                             (if (= double i)
                                 i
                                 (if (and (> double i)(< double (+ i 1)))
                                     (+ i 1)
                                     (round-up double (+ i 1)))))))
          (* (length message) (round-up (base-log-2 (length message)) 0))))))

(fixed-length list)

;Oppgave 2h
;Denne løsningen baserer seg på å hente et leaf, fjerne det fra treet, og legge
;det inn i en ny liste. For å ikke endre på treet, så sier vi at et leaf er fjernet
;dersom det er lagt til i den nye listen.
(define (list-check l1 l2)
  (if (null? l1)
      #t
      (if (member (car l1) l2)
          (list-check (cdr l1) l2)
          #f)))

(define (huffman-leaves tree)
  (define (huffman cur-tree l)
    (if (and (not (null? l)) (list-check (symbols tree) l))
        (letrec ((trim (lambda (untrimmed trimmed)
                         (if (null? untrimmed)
                             trimmed
                             (trim (cdddr untrimmed)(cons (list (cadr untrimmed) (caddr untrimmed)) trimmed ))))))
          (trim l '()))           
        (if (list-check (symbols (left-branch cur-tree)) l)
            (if (leaf? (right-branch cur-tree))
                (huffman tree (append l (right-branch cur-tree)))
                (huffman (right-branch cur-tree) l))
            (if (leaf? (left-branch cur-tree))
                (huffman tree (append l (left-branch cur-tree)))
                (huffman (left-branch cur-tree) l)))))
  (huffman tree '()))

;Oppgave 2i
;Usikkert på hvordan indentere denne prosedyren, så ble kanskje litt kryptisk.
;p(si) = (/ (cadar rest) (weight tree))
;|ci| = (length (encode (list (caar rest)) tree))
;Rekursive kallet multipliserer de to over, og adderer dem til summen. Sender med
;summen og resten av leafs.
(define (expected-code-length tree)
  (letrec ((rec (lambda (sum rest)
                  (if (null? rest)
                      sum
                      (rec (+ sum (* (/ (cadar rest) (weight tree)) (length (encode (list (caar rest)) tree)))) (cdr rest))))))
                      (rec 0 (huffman-leaves tree))))