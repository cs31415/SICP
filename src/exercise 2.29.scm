(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a. Selectors

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (ismobile? structure)
  (and (not (null? structure)) (list? structure)))

(define m (make-mobile 
           (make-branch 10 20) 
           (make-branch 15 30)))
(define m (make-mobile 
           (make-branch 10 (make-mobile 
                            (make-branch 10 20) 
                            (make-branch 20 40))) 
           (make-branch 15 30)))

m
;((10 ((10 20) (20 40))) (15 30))
(left-branch m)
;(10 ((10 20) (20 40)))
(right-branch m)
;(15 30)
(branch-length (left-branch m))
;10
(branch-length (right-branch m))
;15

;b. Total mobile weight
(define (total-weight structure)
  (cond ((null? structure) 0)
        ((not (ismobile? structure)) structure)
        (else (+ (total-weight (branch-structure 
                                (left-branch structure))) 
                 (total-weight (branch-structure 
                                (right-branch structure)))))))
  
(total-weight m)
;90
(total-weight ())
;0

;c. Balanced mobile test
(define (torque branch)
  (if (null? branch) 
      0
      (* (branch-length branch) 
         (total-weight (branch-structure branch)))))


(torque (make-branch 10 
                     (make-mobile 
                      (make-branch 10 5) 
                      (make-branch 10 20))))
;250

(define (is-balanced? structure)
  (if (not (ismobile? structure)) 
      #t
      (let ((lbranch (left-branch structure)) 
            (rbranch (right-branch structure)))
        (and 
         (= (torque lbranch) (torque rbranch))
         (is-balanced? (branch-structure lbranch))
         (is-balanced? (branch-structure rbranch))
         ))))


(torque (left-branch m))
;600
(torque (right-branch m))
;450
(is-balanced? m)
 ;#f

(define n (make-mobile 
           (make-branch 10 (make-mobile 
                            (make-branch 10 20) 
                            (make-branch 10 20))) 
           (make-branch 10 40)))

(torque (left-branch n))
;400
(torque (right-branch n))
;400
(is-balanced? n)
;#t
(is-balanced? ())
;#t
(torque ())
;0

;---------------------------------------------------------

;d. New representation
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; If we're only using the selectors to operate on the mobile
; without relying on the underlying representation details, 
; we only need to update the selectors to work with pairs 
; instead of lists

;a. Selectors

(define (left-branch mobile)
  (car mobile))

; modify to return cdr instead of (car (cdr))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

; modify to return cdr instead of (car (cdr))
(define (branch-structure branch)
  (cdr branch))

; modify to check for pair? instead of list?
(define (ismobile? structure)
  (and (not (null? structure)) (pair? structure)))

(define m (make-mobile 
           (make-branch 10 20) 
           (make-branch 15 30)))
(define m (make-mobile 
           (make-branch 10 (make-mobile 
                            (make-branch 10 20) 
                            (make-branch 20 40))) 
           (make-branch 15 30)))

m
;((10 ((10 20) (20 40))) (15 30))
(left-branch m)
;(10 ((10 20) (20 40)))
(right-branch m)
;(15 30)
(branch-length (left-branch m))
;10
(branch-length (right-branch m))
;15

;b. Total mobile weight
(define (total-weight structure)
  (cond ((null? structure) 0)
        ((not (ismobile? structure)) structure)
        (else (+ (total-weight (branch-structure 
                                (left-branch structure))) 
                 (total-weight (branch-structure 
                                (right-branch structure)))))))
  
(total-weight m)
;90
(total-weight ())
;0

;c. Balanced mobile test
(define (torque branch)
  (if (null? branch) 
      0
      (* (branch-length branch) 
         (total-weight (branch-structure branch)))))


(torque (make-branch 10 (make-mobile 
                         (make-branch 10 5) 
                         (make-branch 10 20))))
;250

(define (is-balanced? structure)
  (if (not (ismobile? structure)) 
      #t
      (let ((lbranch (left-branch structure)) 
            (rbranch (right-branch structure)))
        (and 
         (= (torque lbranch) (torque rbranch))
         (is-balanced? (branch-structure lbranch))
         (is-balanced? (branch-structure rbranch))
         ))))


(torque (left-branch m))
;600
(torque (right-branch m))
;450
(is-balanced? m)
 ;#f

(define n (make-mobile 
           (make-branch 10 (make-mobile 
                            (make-branch 10 20) 
                            (make-branch 10 20))) 
           (make-branch 10 40)))

(torque (left-branch n))
;400
(torque (right-branch n))
;400
(is-balanced? n)
;#t
(is-balanced? ())
;#t
(torque ())
;0

