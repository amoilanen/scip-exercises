(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))

(define (constant-stream value)
  (define s (cons-stream value s))
  s)

(define (streams-close? tolerance)
  (lambda (xs ys)
    (and (= (length xs) (length ys))
         (every (approx= tolerance) xs ys))))

;; A constant current of 1 charges the capacitor linearly: v = 5 + t.
(check (stream-head (RC1 (constant-stream 1) 0) 4)
       (=> (streams-close? 1e-12))
       '(5 5.5 6 6.5))
(check (stream-head (RC1 (constant-stream 0) 2) 3)
       (=> (streams-close? 1e-12))
       '(2 2 2))
(check (stream-head ((RC 2 0.5 0.1) (list->stream '(1 2 -1)) 0) 3)
       (=> (streams-close? 1e-12))
       '(2 4.2 -1.4))
