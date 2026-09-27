;; Streams whose delayed parts memoize like memo-proc of section 3.5.1, except
;; while running inside without-memoization, where forcing a promise always
;; re-evaluates it, as if (delay <exp>) were just (lambda () <exp>).
;; Load this instead of ch3/lib/streams.scm, before defining any stream.

(define memoize-promises? #t)

(define (make-lazy-promise thunk) (cons #f thunk))

(define (force-promise promise)
  (if (car promise)
      (cdr promise)
      (let ((value ((cdr promise))))
        (if memoize-promises?
            (begin (set-car! promise #t)
                   (set-cdr! promise value)))
        value)))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (make-lazy-promise (lambda () b))))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force-promise (cdr s)))

(define (without-memoization thunk)
  (fluid-let ((memoize-promises? #f))
    (thunk)))

(load (merge-pathnames "streams.scm" (current-load-pathname)))
