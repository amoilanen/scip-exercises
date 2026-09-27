(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (monte-carlo trials experiment)
  (let iter ((trials-remaining trials) (trials-passed 0))
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed)))))

;; Converting the range to a flonum makes random return a real number even
;; when the bounds are exact integers.
(define (random-in-range low high)
  (+ low (random (exact->inexact (- high low)))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment)
     (- x2 x1)
     (- y2 y1)))

(define (inside-circle? cx cy r)
  (lambda (x y)
    (<= (+ (square (- x cx)) (square (- y cy)))
        (square r))))

(define (estimate-pi trials)
  (exact->inexact (estimate-integral (inside-circle? 0 0 1) -1 1 -1 1 trials)))

(check (estimate-integral (lambda (x y) #t) 2 4 0 3 100) => 6)
(check (estimate-integral (lambda (x y) #f) 2 4 0 3 100) => 0)

(check (let ((x (random-in-range 2 3))) (and (>= x 2) (< x 3))) => #t)

;; The standard deviation of the estimate with 100000 trials is about 0.005.
(check (estimate-pi 100000) (=> (approx= 0.05)) 3.14159)
(check (exact->inexact
        (estimate-integral (inside-circle? 5 7 3) 2 8 4 10 100000))
       (=> (approx= 0.5))
       (* 3.14159 9))
