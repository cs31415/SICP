;; Utility functions
(define (range m n)
  (if (= n m)
      (list n)
      (append (range m (- n 1)) (list n))))

(define a (char->integer #\a))
(define z (char->integer #\z))
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define punctuation 
  (list #\space #\. #\, #\; #\: #\- #\!))
(define lower-case-alphabets (range a z))
(define upper-case-alphabets (range A Z))
(define (contains? c lst) (not (equal? #f (memv c lst))))
(define (is-punctuation? c) (contains? c punctuation))
(define (is-lower-case? c) 
  (contains? (char->integer c) lower-case-alphabets))
(define (is-upper-case? c) 
  (contains? (char->integer c) upper-case-alphabets))
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
    (append (list (to-upper first-char)) 
            (map to-lower (cdr chars)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (split msg)
  (define (try-split chars word)
    (if (or (null? chars) (null? (car chars)))
        (if (not (null? word)) (list (list->string word)) '())
        (if (is-punctuation? (car chars))
            (append (list (list->string word)) 
                    (list (string (car chars))) 
                    (try-split (cdr chars) '()))
            (try-split (cdr chars) (append word 
                                           (list (car chars)))))))
  (try-split (string->list msg) '()))

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
  (accumulate string-append "" (map pig-latin-word (split msg))))

(pig-latin "This is a test sentence.")
;"Is-thay is-way a-way est-tay entence-say."
(pig-latin "My name is Clark Kent.")
;"Y-may ame-nay is-way Ark-clay Ent-kay."
(pig-latin "My name is Shivali.")