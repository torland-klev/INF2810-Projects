
;;Oppgave 1
(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda ()
        (begin (set! count (+ count 1))
               count)))))


;;Oppgave 2
;;Deloppgave A

(define (make-stack list)
  (let ((stack list))
    (lambda (msg . args)
      (cond
        ((eq? msg 'push!) (set! stack (append (reverse args) stack)))
        ((eq? msg 'pop!)
         (if (null? stack)
             (set! stack stack) ;;Usikker på om dette er beste måte å løse det på
             (set! stack (cdr stack))))
        ((eq? msg 'stack) stack)
        (else 'error)))))

;;TESTS
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)

(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)

(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

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
                  
;;TESTS
(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

;; Tabell-abstraksjon fra seksjon 3.3.3 i SICP:

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (and record (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table 
		  (cons (cons key value) (cdr table))))))


;; mem-testprosedyre 1; fibonacci-tallene
(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; mem-testprosedyre 2; tar virkårlig mange argumenter (null eller flere).
;; (Returnerer summen av argumentenes kvadrerte forskjell fra 42.)
(define (test-proc . args)
  (display "computing test-proc of ")
  (display args) (newline)
  (if (null? args)
      0
      (+ (expt (- 42 (car args)) 2)
	 (apply test-proc (cdr args)))))


;;Oppgave 4

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


(set! fib (mem 'memoize fib))
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)