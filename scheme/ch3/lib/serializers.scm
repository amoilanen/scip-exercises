;; Concurrency of section 3.4, simulated deterministically.
;;
;; Processes are ordinary procedures run as coroutines.  Every access to
;; shared state is written as (atomic expr ...), an indivisible step; before
;; each step the running process gives control back to a scheduler, which
;; decides which process moves next.
;;
;; (possible-outcomes world) calls the thunk world once for every possible
;; order of the steps of the processes it starts with parallel-execute, and
;; returns the distinct values world returned.  A run in which the
;; remaining processes can only wait for each other ends in the outcome
;; deadlock.  Outside parallel-execute an atomic step just runs.

(define-record-type process
    (make-process continue ready?)
    process?
  (continue process-continue set-process-continue!)
  (ready? process-ready? set-process-ready!))

(define (finished? process) (not (process-continue process)))

(define (runnable? process)
  (and (not (finished? process))
       ((process-ready? process))))

(define running-process #f)
(define return-to-scheduler #f)
(define scheduled-choices '())
(define trail '())
(define end-run #f)

;; Runs thunk as one step, once ready? holds; ready? must not have effects.
(define (atomically-when ready? thunk)
  (if running-process
      (call-with-current-continuation
       (lambda (continue)
         (set-process-continue! running-process continue)
         (set-process-ready! running-process ready?)
         (return-to-scheduler 'paused)))
      (if (not (ready?))
          (error "A step outside parallel-execute would wait forever")))
  (thunk))

(define (always) #t)

(define (atomically thunk)
  (atomically-when always thunk))

(define-syntax atomic
  (syntax-rules ()
    ((_ body ...) (atomically (lambda () body ...)))))

(define (spawn thunk)
  (letrec ((process
            (make-process (lambda (resume)
                            (thunk)
                            (set-process-continue! process #f)
                            (return-to-scheduler 'finished))
                          always)))
    process))

(define (run! process)
  (set! running-process process)
  ((process-continue process) 'resume))

;; A run follows the given choices of which ready process moves next, then
;; always picks the first one, recording every choice in the trail, most
;; recent first.  The next run changes the last choice that has an untried
;; alternative, so the runs go through the orders depth first.
(define (choose processes)
  (if (null? (cdr processes))
      (car processes)
      (let ((choice (if (pair? scheduled-choices) (car scheduled-choices) 0)))
        (if (pair? scheduled-choices)
            (set! scheduled-choices (cdr scheduled-choices)))
        (set! trail (cons (cons choice (length processes)) trail))
        (list-ref processes choice))))

(define (next-choices trail)
  (cond ((null? trail) #f)
        ((< (+ (caar trail) 1) (cdar trail))
         (reverse (cons (+ (caar trail) 1) (map car (cdr trail)))))
        (else (next-choices (cdr trail)))))

(define (parallel-execute . thunks)
  (let* ((processes (map spawn thunks))
         (unstarted processes))
    ;; Each process comes back here whenever it pauses or finishes.
    (call-with-current-continuation
     (lambda (scheduler) (set! return-to-scheduler scheduler)))
    (set! running-process #f)
    (if (pair? unstarted)
        (let ((process (car unstarted)))
          (set! unstarted (cdr unstarted))
          (run! process))
        (let ((ready (filter runnable? processes)))
          (cond ((pair? ready) (run! (choose ready)))
                ((not (every finished? processes)) (end-run 'deadlock)))))))

(define (run-world world choices)
  (set! scheduled-choices choices)
  (set! trail '())
  (call-with-current-continuation
   (lambda (return)
     (set! end-run return)
     (world))))

(define (possible-outcomes world)
  (let loop ((choices '()) (outcomes '()))
    (let* ((outcome (run-world world choices))
           (outcomes (if (member outcome outcomes)
                         outcomes
                         (cons outcome outcomes)))
           (next (next-choices trail)))
      (if next
          (loop next outcomes)
          (reverse outcomes)))))

(define (same-set? outcomes expected)
  (lset= equal? outcomes expected))

;;; Mutexes and serializers

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (atomic (set-car! cell false)))

;; A process only takes this step when the cell is clear, so it never
;; fails.  A failing test changes nothing and is simply retried, so leaving
;; it out loses no behaviour, and it keeps waiting from looping forever.
(define (test-and-set! cell)
  (atomically-when (lambda () (not (car cell)))
                   (lambda () (set-car! cell true) false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
