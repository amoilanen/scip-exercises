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

;; Runs process until it pauses before its next step or finishes.
(define (resume! process)
  (call-with-current-continuation
   (lambda (return)
     (set! return-to-scheduler return)
     (set! running-process process)
     ((process-continue process) 'resume)))
  (set! running-process #f))

;; A run replays the choices made so far; when they run out at a real
;; choice, the run ends and asks for a choice among the ready processes.
(define (choose processes)
  (cond ((null? (cdr processes)) (car processes))
        ((pair? scheduled-choices)
         (let ((choice (car scheduled-choices)))
           (set! scheduled-choices (cdr scheduled-choices))
           (list-ref processes choice)))
        (else (end-run (cons 'choose (length processes))))))

(define (parallel-execute . thunks)
  (let ((processes (map spawn thunks)))
    (for-each resume! processes)
    (let loop ()
      (let ((ready (filter runnable? processes)))
        (cond ((pair? ready)
               (resume! (choose ready))
               (loop))
              ((not (every finished? processes))
               (end-run (cons 'outcome 'deadlock))))))))

(define (run-world world choices)
  (call-with-current-continuation
   (lambda (return)
     (set! end-run return)
     (set! scheduled-choices choices)
     (cons 'outcome (world)))))

(define (possible-outcomes world)
  (define (explore choices)
    (let ((result (run-world world choices)))
      (if (eq? (car result) 'outcome)
          (list (cdr result))
          (append-map (lambda (choice)
                        (explore (append choices (list choice))))
                      (iota (cdr result))))))
  (delete-duplicates (explore '())))

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
