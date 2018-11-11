(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime n (- (real-time-clock) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display  " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes from to)
  (timed-prime-test from)
  (if (>= from to)
      ((newline) (display "finished"))
      (search-for-primes (+ 1 from) to)))

; Sizes of the input: first call n, second call 100 * n
; since time is square root, first call executes at t, second at 10 * t
(begin
  (search-for-primes 10000 10100)
  (search-for-primes 100 200)
)
