(load "lib/check.scm")
(load-option 'synchronous-subprocess)

;; Exercise 5.51: the explicit-control evaluator in C lives in ch5/5.51.
;; It prints the value of each expression it reads, like the driver loop
;; of section 5.4.4, and error messages begin with a semicolon.

(define (run-command command input)
  (let ((output (open-output-string)))
    (let ((status (run-shell-command command
                                     'input (open-input-string input)
                                     'output output)))
      (cons status (get-output-string output)))))

(define (lines . strings)
  (apply string-append
         (map (lambda (string) (string-append string "\n")) strings)))

;; The build must be free of warnings, so make prints nothing.
(check (run-command "make -s -B -j8 -C ch5/5.51" "") => '(0 . ""))

(define interpreter "ch5/5.51/build/scheme")

(define (scheme program)
  (cdr (run-command interpreter program)))

(define (scheme-status program)
  (car (run-command interpreter program)))

;;; Reader and printer

(check (scheme "42 -7 2.5 \"a \\\"b\\\"\" #t #f 'sym '() '(1 (2 . 3) . 4)")
       => (lines "42" "-7" "2.5" "\"a \\\"b\\\"\"" "#t" "#f"
                 "sym" "()" "(1 (2 . 3) . 4)"))

(check (scheme "; a comment\n(quote (a 'b))") => (lines "(a (quote b))"))

;;; Primitives

(check (scheme "(+ 1 2 3) (- 10 4 3) (- 5) (* 2 3 4) (/ 6 3) (/ 1 2)
                (* 1.5 2) (remainder 17 5) (quotient -17 5) (abs -3)")
       => (lines "6" "3" "-5" "24" "2" ".5" "3." "2" "-3" "3"))

(check (scheme "(< 1 2 3) (< 1 3 2) (= 2 2.) (>= 3 3 1)
                (eq? 'a 'a) (equal? '(1 (\"x\")) '(1 (\"x\"))) (not 0)")
       => (lines "#t" "#f" "#t" "#t" "#t" "#t" "#f"))

(check (scheme "(cons 1 2) (list 1 2 3) (cadr '(1 2 3)) (length '(a b))
                (null? '()) (pair? '()) (symbol? 'x) (number? 1.5)")
       => (lines "(1 . 2)" "(1 2 3)" "2" "2" "#t" "#f" "#t" "#t"))

(check (scheme "(display \"x = \") (display '(\"s\" 1)) (newline)")
       => (lines "x = (s 1)"))

;;; Special forms

(check (scheme "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                (fact 20)")
       => (lines "ok" "2432902008176640000"))

(check (scheme "(define (make-counter)
                  (let ((count 0))
                    (lambda () (set! count (+ count 1)) count)))
                (define c (make-counter))
                (c) (c)
                (begin 1 2 3)")
       => (lines "ok" "ok" "1" "2" "3"))

(check (scheme "(define (sign x)
                  (cond ((< x 0) 'negative) ((= x 0) 'zero) (else 'positive)))
                (list (sign -2) (sign 0) (sign 5))
                (cond ((assoc-free 1) 'no))")
       => (lines "ok" "(negative zero positive)"
                 ";Unbound variable assoc-free"))

(check (scheme "(cond (false 1) ((+ 1 1)))
                (if false 1)
                (let ((x 1) (y 2)) (let ((x y) (y x)) (list x y)))
                ((lambda (a . rest) (list a rest)) 1 2 3)
                ((lambda args args))")
       => (lines "2" "(2 1)" "(1 (2 3))" "()"))

;;; Proper tail calls: the stack holds 100000 values, so the loop would
;;; overflow it if the evaluator saved anything for each iteration.

(check (scheme "(define (loop n) (if (= n 0) 'done (loop (- n 1))))
                (loop 100000)
                (define (count n) (if (= n 0) 0 (+ 1 (count (- n 1)))))
                (count 100000)
                (count 1000)")
       => (lines "ok" "done" "ok"
                 ";Aborting!: maximum recursion depth exceeded" "1000"))

;;; Garbage collection: the memory holds 2^18 pairs, and churn allocates
;;; several times as many, while keep has to survive every collection.

(check (scheme "(define (iota n) (if (= n 0) '() (cons n (iota (- n 1)))))
                (define (sum list)
                  (if (null? list) 0 (+ (car list) (sum (cdr list)))))
                (define keep (iota 1000))
                (define (churn k)
                  (if (= k 0) 'done (begin (iota 1000) (churn (- k 1)))))
                (churn 150)
                (sum keep)")
       => (lines "ok" "ok" "ok" "ok" "done" "500500"))

(check (scheme "(define (grow list) (grow (cons list list)))
                (grow '())
                'recovered")
       => (lines "ok" ";Aborting!: out of memory" "recovered"))

;;; Errors are reported and the driver loop goes on; the exit status tells
;;; whether any occurred.

(check (scheme "(car '()) (undefined) ((lambda (x) x)) (1 2) (/ 1 0) 'next")
       => (lines ";The object passed to car is not a pair: ()"
                 ";Unbound variable undefined"
                 ";Too few arguments supplied for (x)"
                 ";The object is not applicable: 1"
                 ";Division by zero signalled by /."
                 "next"))

(check (scheme "(error \"Something bad:\" 'x 42)")
       => (lines ";Something bad: x 42"))

(check (scheme-status "(+ 1 2)") => 0)
(check (scheme-status "(car 1)") => 1)

;;; A program can also be read from a file.

(call-with-output-file "ch5/5.51/build/program.scm"
  (lambda (port)
    (write '(define (square x) (* x x)) port)
    (write '(square 12) port)))

(check (cdr (run-command (string-append interpreter
                                        " ch5/5.51/build/program.scm")
                         ""))
       => (lines "ok" "144"))
