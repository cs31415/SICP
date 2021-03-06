;set-of-records is an unordered set
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;set-of-records is a balanced binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-rec key value)
  (cons key value))
(define (key record)
  (car record))
(define (value record)
  (cadr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define set (list->tree (list (make-rec 1 1) 
                              (make-rec 3 3) 
                              (make-rec 4 4) 
                              (make-rec 7 7) 
                              (make-rec 8 8) 
                              (make-rec 13 13) 
                              (make-rec 18 18) 
                              (make-rec 23 23)
                              (make-rec 29 29)
                              (make-rec 31 31)
                              (make-rec 37 37))))

(lookup 29 set)
;(29 . 29)
(lookup 23 set)
;(23 . 23)
(lookup 37 set)
;(37 . 37)
(lookup 4 set)
;(4 . 4)