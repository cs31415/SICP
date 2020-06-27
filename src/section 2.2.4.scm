(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


; v = (x,y) describes a vector corresponding to a point within the unit square.
; Thus, 0 <= x,y <= 1
; A frame coordinate map transforms x,y into x', y'. i.e. it maps a point within
; the unit square to a point within the frame.

((frame-coord-map a-frame) (make-vect 0 0))

(origin-frame a-frame)



