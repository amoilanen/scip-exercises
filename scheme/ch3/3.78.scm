(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.77.scm" (current-load-pathname)))

;; y'' = a y' + b y
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

(check (stream-head (solve-2nd 0 0 1 3 2) 4) => '(3 5 7 9))
(check (stream-ref (solve-2nd 0 -1 0.0001 0 1) 10000)
       (=> (approx= 1e-4))
       (sin 1))
(check (stream-ref (solve-2nd 1 0 0.0001 1 1) 10000)
       (=> (approx= 1e-3))
       (exp 1))
