;a.  Add some segments to the primitive wave painter of exercise 2.49 
;    (to add a smile, for example).

; We can represent the wave image as a series of line segments
(define body
 (list (make-segment a b)
       (make-segment b c)
       (make-segment c d)))

(define smile
  (list (make-segment u v)
        (make-segment v w)))

(define (wave frame)
  (segments->painter (append smile body)))

;b.  Change the pattern constructed by corner-split (for example, by 
;    using only one copy of the up-split and right-split images
;    instead of two).

up-split n-1, corner-split n-1
identity, right-split n-1

(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter 
                     (up-split painter (- n 1)))
              (below (right-split painter (- n 1)) 
                     (corner-split painter (- n 1))))))


;c.  Modify the version of square-limit that uses square-of-four 
;    so as to assemble the corners in a different pattern. 
;    (For example, you might make the big Mr. Rogers look outward from 
;    each corner of the square.)

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
      (below (flip-vert half) half))))

