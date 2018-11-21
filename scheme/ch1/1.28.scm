(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (let (( potential-root-modulo-n (expmod base (/ exp 2) m) ))
            (let (( squared-modulo-n (remainder (square potential-root-modulo-n) m) ))
                 (if (and (= 1 squared-modulo-n) (< potential-root-modulo-n (- m 1)) (> potential-root-modulo-n 1)) 0
                     squared-modulo-n)) ))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (if (= times 0) true
      (and (miller-rabin-test n) (fast-prime? n (- times 1)))))

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

(begin
  (search-for-primes 10000 10100)
)