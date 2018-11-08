(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (sleep-current-thread 10)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (real-time-clock) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display  " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes from to)
  (timed-prime-test from)
  (if (>= from to)
      (display "finished")
      (search-for-primes (+ 1 from) to)))

; Sizes of the input: first call n, second call 100 * n
; since time is square root, first call executes at t, second at 10 * t
(
  (search-for-primes 100 200)
  (search-for-primes 10000 10100))