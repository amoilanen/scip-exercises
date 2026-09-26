(load "lib/check.scm")

;; Suppose halts? existed and consider (try try):
;; - if (halts? try try) is true, (try try) runs forever, so it does not halt;
;; - if it is false, (try try) returns halted, so it does halt.
;; Either way halts? answers wrongly about (try try), so no such procedure
;; can exist.
;;
;; The checks build try from a candidate halts? and compare what halts?
;; predicts about (try try) with what (try try) really does; running forever
;; is simulated by escaping.

(define (prediction-and-behaviour halts?)
  (call-with-current-continuation
   (lambda (return)
     (define (try p)
       (if (halts? p p)
           (run-forever)
           'halted))
     (define (prediction)
       (if (halts? try try) 'halts 'runs-forever))
     (define (run-forever)
       (return (list (prediction) 'runs-forever)))
     (try try)
     (list (prediction) 'halts))))

(define (always-halts? p a) #t)
(define (never-halts? p a) #f)

(check (prediction-and-behaviour always-halts?) => '(halts runs-forever))
(check (prediction-and-behaviour never-halts?) => '(runs-forever halts))
