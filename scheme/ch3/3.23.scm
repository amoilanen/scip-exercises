(load "lib/check.scm")

;; A deque is a pair of pointers to the first and last nodes of a doubly
;; linked list.  Each node is a list (item prev next), so every operation
;; takes constant time.

(define (make-node item prev next) (list item prev next))
(define (node-item node) (car node))
(define (node-prev node) (cadr node))
(define (node-next node) (caddr node))
(define (set-node-prev! node prev) (set-car! (cdr node) prev))
(define (set-node-next! node next) (set-car! (cddr node) next))

(define (front-node deque) (car deque))
(define (rear-node deque) (cdr deque))
(define (set-front-node! deque node) (set-car! deque node))
(define (set-rear-node! deque node) (set-cdr! deque node))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque)
  (null? (front-node deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (node-item (front-node deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty deque" deque)
      (node-item (rear-node deque))))

(define (front-insert-deque! deque item)
  (let ((node (make-node item '() (front-node deque))))
    (if (empty-deque? deque)
        (set-rear-node! deque node)
        (set-node-prev! (front-node deque) node))
    (set-front-node! deque node)
    deque))

(define (rear-insert-deque! deque item)
  (let ((node (make-node item (rear-node deque) '())))
    (if (empty-deque? deque)
        (set-front-node! deque node)
        (set-node-next! (rear-node deque) node))
    (set-rear-node! deque node)
    deque))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "FRONT-DELETE-DEQUE! called with an empty deque" deque)
      (let ((next (node-next (front-node deque))))
        (set-front-node! deque next)
        (if (null? next)
            (set-rear-node! deque '())
            (set-node-prev! next '()))
        deque)))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "REAR-DELETE-DEQUE! called with an empty deque" deque)
      (let ((prev (node-prev (rear-node deque))))
        (set-rear-node! deque prev)
        (if (null? prev)
            (set-front-node! deque '())
            (set-node-next! prev '()))
        deque)))

(define (deque->list deque)
  (let loop ((node (rear-node deque)) (items '()))
    (if (null? node)
        items
        (loop (node-prev node) (cons (node-item node) items)))))

(define d (make-deque))
(check (empty-deque? d) => #t)
(check (deque->list d) => '())
(check-error (front-deque d))
(check-error (rear-deque d))
(check-error (front-delete-deque! d))
(check-error (rear-delete-deque! d))

(rear-insert-deque! d 'b)
(check (front-deque d) => 'b)
(check (rear-deque d) => 'b)

(front-insert-deque! d 'a)
(rear-insert-deque! d 'c)
(check (deque->list d) => '(a b c))
(check (front-deque d) => 'a)
(check (rear-deque d) => 'c)

(check (deque->list (front-delete-deque! d)) => '(b c))
(check (deque->list (rear-delete-deque! d)) => '(b))
(check (deque->list (rear-delete-deque! d)) => '())
(check (empty-deque? d) => #t)

(front-insert-deque! d 'x)
(front-insert-deque! d 'w)
(check (deque->list (front-delete-deque! d)) => '(x))
(check (deque->list (front-delete-deque! d)) => '())
(rear-insert-deque! d 'y)
(check (front-deque d) => 'y)
(check (rear-deque d) => 'y)
