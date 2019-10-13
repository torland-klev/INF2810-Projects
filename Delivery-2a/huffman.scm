;;;;
;;;; Prekode til innlevering 2a i INF2810 (V17): Prosedyrer for å jobbe med
;;;; Huffman-trær, fra SICP, Seksjon 2.3.4.
;;;;

;;; Merk at koden under gjør bruk av diverse innebygde kortformer for
;;; kjeder av car og cdr. F.eks er (cadr x) det samme som (car (cdr x)), 
;;; og (caadr x) tilsvarer (car (car (cdr x))), osv. 

;;;NOTE FRA MEG: noe her kan virke litt uryddig, men har prøvd å få litt orden på det.
;;;Encode-prosedyren, og member?, ligger under ;;;Enkoding, og decode-it ligger under ;;;Dekoding
;;;Resten av prosedyrene som er lagt til ligger nærmere bunnen av denne fila.
;;;Om det er noen jeg har fjernet herfra, kan de hentes fra henriktk.scm. Alt skal funke,
;;;men jeg har vært litt dårlig på regresjonstestingen. Kan være det er kommet noen
;;;bugs i programmet, dersom jeg har endret på noen av hjelpeprosedyrene og glemt re-testing.

;;;
;;; Abstraksjonsbarriere:
;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;
;;; Enkoding
;;;

(define (member? proc element items)
  (if (null? items)
      #f
      (if (equal? element items)
          #t
          (if (proc element (car items))
              #t
              (member? proc element (cdr items))))))

(define (encode message tree)
  (define (encode1 message cur-tree bits)
    (if (null? message)
        (reverse-list bits)
        (if (member? equal? (car message) (symbols (left-branch cur-tree)))
            (if (leaf? (left-branch cur-tree))
                (encode1 (cdr message) tree (cons 0 bits))
                (encode1 message (left-branch cur-tree) (cons 0 bits)))
            (if (member? equal? (car message) (symbols (right-branch cur-tree)))
                (if (leaf? (right-branch cur-tree))
                    (encode1 (cdr message) tree (cons 1 bits))
                    (encode1 message (right-branch cur-tree) (cons 1 bits)))  
                '(error: minst én item ikke i treet)))))
  (encode1 message tree '()))

;;;
;;; Dekoding:
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

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

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))


;;;
;;; Sortering av node-lister:
;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;;
;;; Diverse test-data:
;;;

(define sample-tree
  (make-code-tree 
   (make-leaf 'ninjas 8) 
   (make-code-tree 
    (make-leaf 'fight 5) 
    (make-code-tree 
     (make-leaf 'night 1) 
     (make-leaf 'by 1)))))

(define sample-code '(0 1 0 0 1 1 1 1 1 0))

;;;
;;;Hjelpeprosedyrer
;;;

(define (average-length tree decoded-message)
  (/ (weight tree) (length decoded-message)))

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

(define (list-check l1 l2)
  (if (null? l1)
      #t
      (if (member (car l1) l2)
          (list-check (cdr l1) l2)
          #f)))

;;;
;;;Huffman-prosedyrer
;;;

(define (grow-huffman-tree tree)
  (define (grow-huffman list)
    (if (> 2 (length list))
        (car list)
        (grow-huffman (adjoin-set (make-code-tree (cadr list) (car list)) (cddr list)))))
  (grow-huffman (make-leaf-set tree)))

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

(define (expected-code-length tree)
  (letrec ((rec (lambda (sum rest)
                  (if (null? rest)
                      sum
                      (rec (+ sum (* (/ (cadar rest) (weight tree)) (length (encode (list (caar rest)) tree)))) (cdr rest))))))
                      (rec 0 (huffman-leaves tree))))

;;;
;;; Tests
;;;
(decode (encode '(ninjas fight by night ninjas by) sample-tree) sample-tree)
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)

(define codelist '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2)
                                 (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2)
                                 (forest 1) (wait 1) (poison 1)))
(define message '(ninjas fight
                         ninjas fight ninjas
                         ninjas fight samurais
                         samurais fight
                         samurais fight ninjas
                         ninjas fight by night))

(define codetree (grow-huffman-tree codelist))

(expected-code-length sample-tree)

