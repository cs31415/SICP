; Note: Language needs to be Textual(MzScheme, includes R5RS)
(define h (make-hash-table 'equal))
(hash-table-put! h 'key1 'val1)

(hash-table-get h 'key1)

(if 
 (hash-table-get h 'key1 (lambda () #f))
 #t
 #f)


(reduce + '(1 2 3 4))