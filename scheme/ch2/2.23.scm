(define (for-each action items)
  (if (null? items)
    ()
    (begin (action (car items))
     (for-each action (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))