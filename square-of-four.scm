(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

; square-of-four takes procedures as arguments and returns
; a procedure that takes painter as argument
(define (flipped-pairs painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; original version
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; My version
(define (square-limit painter n)
  (let ((quarter (lamda (x) (corner-split x n))))
    (let ((half ((lambda (x) (beside (flip-horiz (quarter x)) (quarter x)))))
          (bl (lambda (x) (flip-vert (flip-horiz (quarter x)))))
          (br (lambda (x) (flip-vert (quarter x))))
          (tl (lambda (x) (flip-horiz (quarter x))))
          (tr (lambda (x) (quarter x))))
      (square-of-four tl tr bl br))))

; textbook version
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

