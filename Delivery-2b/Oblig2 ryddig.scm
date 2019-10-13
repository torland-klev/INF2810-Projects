;;Oppgave 1

;;Siden count er bundet inni make-counter, men utenfor prosedyren som blir returnert
;;så vil hvert objekt(?) av denne prosedyren ha sin egen count.

(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda ()
        (begin (set! count (+ count 1))
               count)))))

;;Oppgave 2
;;Deloppgave A

;;*Usikker på om dette er beste måten å løse pop! på en tom stack.
;;Det optimale ville nok vært å hatt en "do nothing", men vet ikke om
;;noe slikt i Scheme. Så istedet setter den stacken til å være seg selv,
;;altså tom.

(define (make-stack list)
  (let ((stack list))
    (lambda (msg . args)
      (cond
        ((eq? msg 'push!) (set! stack (append (reverse args) stack)))
        ((eq? msg 'pop!)
         (if (null? stack)
             (set! stack stack) ;;*
             (set! stack (cdr stack))))
        ((eq? msg 'stack) stack)
        (else 'error)))))

;;Deloppgave B
(define (pop! s)
  (s 'pop!))
(define (stack s)
  (s 'stack))

(define (push! s . items)
  (if (null? items)
      "Ingen argumenter funnet."
      (letrec ((help (lambda (x)
                       (if (= 1 (length x))
                           (s 'push! (car x))
                           (begin (s 'push! (car x))
                                  (help (cdr x)))))))
        (help items))))

;;Oppgave 4

;;Deloppgave A gikk greit, kunne basere mye på prosedyren fra boken.
;;Ble nødt til å endre den en del idet jeg begynte på deloppgave B.
;;Deloppgave B: denne var litt mer tricky. Prøvde først å lagre den
;;første funksjonen i en lokal variabel, men det ble problematisk
;;dersom func måtte kunne brukes med 0 argumenter.
;;Endret derfor over til å definere nye symboler, og
;;så sjekke dem for å sette igang av-memoriseringen.
;;Nå vil den originale prosedyren gjennopprettes dersom
;;check og trig er like. 

(define mem
    (let ((trig (list 'trig)) (check (list 'check)))
      (lambda (proc func)
      (cond
        ((equal? proc 'unmemoize)
         (let ((c check))
           (set! check trig)
           (let ((f (func)))
             (set! check c)
             f)))
        ((equal? proc 'memoize)
         (let ((table (make-table)))
           (lambda args
             (if (eq? check trig)
                 func
                 (let ((prev (lookup args table)))
                   (or prev
                       (let ((result (apply func args)))
                         (insert! args result table)
                         result)))))))

         (else (display "Ugyldig kommando"))))))
