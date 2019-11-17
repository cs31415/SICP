;; Utility functions
(define (range m n)
  (if (= n m)
      (list n)
      (append (range m (- n 1)) (list n))))

;; Creating a secret code

; Hash table definitions
(define hmap (make-hash-table 'equal))
(define rmap (make-hash-table 'equal))

(define a (char->integer #\a))
(define z (char->integer #\z))
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define punctuation (list #\space #\. #\, #\; #\: #\- #\!))
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


(define (add-birectional-map-entry hmap rmap k v)
  (begin
    (hash-table-put! hmap k v)
    (hash-table-put! rmap v k)))

(define (generate-secret-code range l u)
  (for-each (lambda (x) 
              (add-birectional-map-entry
               hmap 
               rmap 
               x 
               (+ l (modulo (+ (- x l) 2) (+ 1 (- u l))))))
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

(encode "This is a secret.")
;"Vjku ku c ugetgv."
(decode (encode "This is a secret."))
;"This is a secret."