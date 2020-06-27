(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (area-rectangle x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (area-rectangle x1 x2 y1 y2) 
     (monte-carlo trials (region-test P x1 x2 y1 y2))))
(define (region-test P x1 x2 y1 y2)
   (lambda () 
     (P (random-in-range x1 x2) (random-in-range y1 y2))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (square x) (* x x))
(define (is-point-within-circle origin-x origin-y radius)
  (lambda (x y) (<= (+ (square (- x origin-x)) 
                       (square (- y origin-y))) 
                    (square radius))))

; Area of unit circle = π*1^2 = π
(define (estimate-pi trials)
  (* 1.0 
     (estimate-integral 
      (is-point-within-circle 0 0 1) -1 1 -1 1 trials)))

(estimate-pi 1000)
;3.116
;3.016
;2.992