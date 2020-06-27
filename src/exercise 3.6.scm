(define (rand mode)
  (cond ((eq? mode 'generate) (random))
        ((eq? mode 'reset) (lambda (x) (random-seed x)))
        (else 
         (error "rand: Invalid argument."))))

((rand 'reset) 10)

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
;0.11586349506387651
;0.21594793789954184
;0.027273325173382566
;0.7611891798040247
;0.41418324344561314
;0.9934134249645269
;0.9984134316141704

((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
;0.11586349506387651
;0.21594793789954184
;0.027273325173382566
;0.7611891798040247
;0.41418324344561314
;0.9934134249645269
;0.9984134316141704

((rand 'reset) 20)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
;0.5971941589881632
;0.20959420702317616
;0.587684919414684
;0.6188529426514665
;0.469868790528902
;0.44043617802921803
;0.8670100582153751