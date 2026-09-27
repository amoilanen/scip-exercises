(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/optional-memoization.scm" (current-load-pathname)))

(define (show x)
  (display-line x)
  x)

(define (output-and-value thunk)
  (let* ((value #f)
         (output (with-output-to-string
                   (lambda () (set! value (thunk))))))
    (list output value)))

;; Returns what each of the three interactions prints and returns.
(define (interactions)
  (define x)
  (let* ((definition
          (output-and-value
           (lambda ()
             (set! x (stream-map show (stream-enumerate-interval 0 10)))
             'x)))
         (fifth (output-and-value (lambda () (stream-ref x 5))))
         (seventh (output-and-value (lambda () (stream-ref x 7)))))
    (list definition fifth seventh)))

;; Defining x prints only 0, because stream-map applies show to the first
;; element alone.  (stream-ref x 5) prints 1 to 5 and returns 5.  With
;; memoized promises (stream-ref x 7) prints just 6 and 7: the elements up to
;; 5 are already computed.
(check (interactions)
       => '(("\n0" x)
            ("\n1\n2\n3\n4\n5" 5)
            ("\n6\n7" 7)))

;; Without memoization every element is recomputed and printed again.
(check (without-memoization interactions)
       => '(("\n0" x)
            ("\n1\n2\n3\n4\n5" 5)
            ("\n1\n2\n3\n4\n5\n6\n7" 7)))
