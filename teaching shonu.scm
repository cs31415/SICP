
(define (fibonacci x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci (- x 1))
         (fibonacci (- x 2)))))

;(map fibonacci (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
  
;(fibonacci 37)

; recursion  


(* 3 6)

(/ 24 2)

(/ (* (* (+ 3 1) 18) 17) 3)


(define a 2)
(define b 3)
(* a b)

(define e 9)
(define y 7)
(* e y)

(define (range m n)
  (if (= n m)
      (list n)
      (append (range m (- n 1)) (list n))))

(range 1 10)

(map fibonacci (range 1 20))

; multiplication is repeated addition
(define (multiply x y)
  (if (= y 1)
      x
      (+ x (multiply x (- y 1)))))

(multiply 6 4)

;(+ 6 (multiply 6 3))
;(+ 6 (+ 6 (multiply 6 2))
;(+ 6 (+ 6 (+ 6 (multiply 6 1))))
;(+ 6 (+ 6 (+ 6 6)))
;(+ 6 (+ 6 12))
;(+ 6 18)
;24

(define (add l1 l2)
  (let ((f1 (if (pair? l1) (car l1) 0))
        (f2 (if (pair? l2) (car l2) 0))
        (r1 (if (pair? l1) (cdr l1) '()))
        (r2 (if (pair? l2) (cdr l2) '())))
    (if (not (or (pair? l1) (pair? l2)))
        '()
        (append (list (+ f1 f2)) 
                (add r1 r2)))))
  
(add (list 1 2 3 4 ) (list 1 2 3 4 5))

; division is repeated subtraction
; returns (quotient remainder)
(define (divide x y)
  (if (< x y) 
      (list 0 x)
      (add (list 1 0) (divide (- x y) y))))

(divide 3 7)

(divide 24 6)



(define (quotient x y) (car (divide x y)))
(define (remainder x y) (cadr (divide x y)))
                              
(define (deconstruct n)
  (define (place x)
    (cond ((= x 1) 'ones)
          ((= x 10) 'tens)
          ((= x 100) 'hundreds)
          ((= x 1000) 'thousands)
          ((= x 10000) 'tenthousands)
          ((= x 100000) 'hundredthousands)
          ((= x 1000000) 'millions)))
  (define (trydeconstruct n multipleof10)
    (if (< n 10)
        (list (list n (place multipleof10)))
        (append (trydeconstruct (quotient n 10) (* 10 multipleof10)) 
                (list (list (remainder n 10) (place multipleof10))))))
  (trydeconstruct n 1))

(deconstruct 2461357)

;(define (factor n)
;  (define (divisibleby? n m)
;    (= (/ n m) (floor (/ n m))))
;  (append () ))

;(divisibleby? 54 2)

;54
;2 * 27
;2 * 3 * 9
;2 * 3 * 3 * 3


(define (smallest-divisor n)
  (find-divisor n 2))
 
; The end test for find-divisor is based on the result that if n is non-prime, 
; it's divisors must be <= sqrt(n)
; Proof using reduction ad absurdum:
; if d is a divisor, then n/d is also a divisor. 
; if the smallest divisor d is greater than sqrt(n), then n/d must be less than 
; sqrt(n). In other words, there exists a divisor smaller than d. This is a 
; contradiction.
; Hence the smallest divisor d can never be greater than sqrt(n).
; n = 36, d = 4. n/d = 9 which is > sqrt(36).
 
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))
 
(define (divides? d n)
  (= 0 (remainder n d)))
 
(define (square x) (* x x))
 
(smallest-divisor 2711)
2711
 
; since find-divisor is called at most sqrt(n) times, this algorithm is 
; theta(sqrt(n))
 
; smallest-divisor can be used as a prime number detector.  If a number's smallest 
; divisor is the number itself, then it must be prime.
(define (prime? n)
  (= n (smallest-divisor n)))


;; Creating a secret code

; Hash table definitions
(define hmap (make-hash-table 'equal))
(define rmap (make-hash-table 'equal))

(define a (char->integer #\a))
(define z (char->integer #\z))
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define punctuation (list #\space #\. #\, #\; #\: #\- #\! #\| #\newline))
(define lower-case-alphabets (range a z))
(define upper-case-alphabets (range A Z))
(define (contains? c lst) (not (equal? #f (memv c lst))))
(define (is-punctuation? c)   
  (if (char? c) 
      (contains? c punctuation) 
      (if (or (null? c) (equal? c ""))
          #t
          (contains? (car (string->list c)) punctuation))))

(define (is-lower-case? c) (contains? (char->integer c) lower-case-alphabets))
(define (is-upper-case? c) (contains? (char->integer c) upper-case-alphabets))
(define (to-upper c)
  (if (is-punctuation? c)
      c
      (if (is-upper-case? c)
          c
          (integer->char (- (char->integer c) (- a A))))))
(define (to-lower c)
  (if (is-punctuation? c)
      c
      (if (is-lower-case? c)
          c
          (integer->char (+ (char->integer c) (- a A))))))
(define (capitalize-word chars)
  (let ((first-char (car chars)))
    (append (list (to-upper first-char)) (map to-lower (cdr chars)))))


(define (add-birectional-map-entry hmap rmap k v)
  (begin
    (hash-table-put! hmap k v)
    (hash-table-put! rmap v k)))

(define (generate-secret-code range l u)
  (for-each (lambda (x) (add-birectional-map-entry hmap rmap x (+ l (modulo (+ (- x l) 2) (+ 1 (- u l))))))
            range))
(generate-secret-code lower-case-alphabets a z)
(generate-secret-code upper-case-alphabets A Z)

(define (translate char-map message)
  (list->string 
   (map 
    (lambda (x) 
      (if (is-punctuation? x)
          x
          (integer->char (hash-table-get char-map (char->integer x))))) 
    (string->list message))))

(define (encode message) (translate hmap message))
(define (decode message) (translate rmap message))

(encode "Mahika is my best friend or BFF.")
(decode (encode "Mahika is my best friend or BFF."))

(encode "hello world")
(decode "jgnnq yqtnf")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (tokenize msg)
  (define (try-tokenize chars word)
    (if (or (null? chars) (null? (car chars)))
        (if (not (null? word)) 
            (list (list->string word))
            '())
        (if (is-punctuation? (car chars))
            (append (list (list->string word)) (list (string (car chars))) (try-tokenize (cdr chars) '()))
            (try-tokenize (cdr chars) (append word (list (car chars)))))))
  (try-tokenize (string->list msg) '()))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (split msg)
  (filter (lambda (x) (not (is-punctuation? x))) (tokenize msg)))

;; Pig Latin
(define (is-vowel? c) (contains? c (list #\a #\e #\i #\o #\u)))
(define (is-pig-latin-vowel? c first-letter?) 
  (or (is-vowel? c)
      (and (not first-letter?)
           (eqv? #\y c))))
(define (first msg) (car msg))
(define (starts-with-vowel? chars) 
  (is-vowel? (first chars)))

(define (pig-split chars)
  (define (try-pig-split chars first-letter?)
    (if (or (null? chars) (null? (car chars)))
        '()
        (if (is-pig-latin-vowel? (car chars) first-letter?)
            chars 
            (try-pig-split (append (cdr chars) 
                                   (if first-letter? (list #\-) '()) 
                                   (list (car chars))) #f))))
  (let* ((first-char (car chars))
        (is-capitalized? (is-upper-case? first-char))
        (result (try-pig-split chars #t)))
    (if is-capitalized? (capitalize-word result) result)))
        
(define (pig-latin-word word)
  (let ((chars (string->list word)))
    (if (is-punctuation? (car chars))
        word
        (if (starts-with-vowel? chars)
            (list->string (append chars (list #\- #\w #\a #\y)))
            (list->string (append (pig-split chars) 
                                  (string->list "ay")))))))
  
(define (pig-latin msg)
  (accumulate string-append "" (map pig-latin-word (tokenize msg))))

(pig-latin "This is a test sentence.")
;"Is-thay is-way a-way est-tay entence-say."
(pig-latin "My name is Clark Kent.")
;"Y-may ame-nay is-way Ark-clay Ent-kay."

(tokenize "(adj)|simple |elemental|ultimate|oversimplified|simplistic|simplex|simplified|unanalyzable|undecomposable|uncomplicated|unsophisticated|easy|plain|unsubdivided")
(split "(adj)|simple |elemental|ultimate|oversimplified|simplistic|simplex|simplified|unanalyzable|undecomposable|uncomplicated|unsophisticated|easy|plain|unsubdivided")