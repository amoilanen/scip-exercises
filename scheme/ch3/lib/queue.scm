;; Queues represented as a pair of pointers to the first and last pairs of
;; an ordinary list (section 3.3.2).

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (list item)))
    (if (empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-cdr! (rear-ptr queue) new-pair))
    (set-rear-ptr! queue new-pair)
    queue))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))
