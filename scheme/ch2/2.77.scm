(define put '())
(define get '())

; Adapted version from https://stackoverflow.com/a/5499256/1461965
(define (define-put-get)
  (define global-entries '())

  (define (make-entry key value) (cons key value))
  (define (entry-key entry) (car entry))
  (define (entry-value entry) (cdr entry))

  (define (put-global key1 key2 value)
    (define (put-local key entries)
      (cond ((null? entries) (list (make-entry key value)))
            ((equal? (entry-key (car entries)) key) entries)
            (else (cons (car entries) (put-local key (cdr entries))))))
    (set! global-entries (put-local (cons key1 key2) global-entries)))

  (define (get-global key1 key2)
    (define (get-local key entries)
      (cond ((null? entries) #f)
            ((equal? (entry-key (car entries)) key) (entry-value (car entries)))
            (else (get-local key (cdr entries)))))
    (get-local (cons key1 key2) global-entries))

  (set! put put-global)
  (set! get get-global)
'done)

(define-put-get)

(put 'foo '(bar bar) (lambda (x y) (+ x y)))

(display ((get 'foo '(bar bar)) 2 3))