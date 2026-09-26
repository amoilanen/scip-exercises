(load "lib/check.scm")
(load "ch3/lib/queue.scm")

;; The printer shows the queue as the pair it is: the car is the list of
;; items and the cdr is the last pair of that same list, so the last item
;; seems to appear twice.  delete-queue! only moves the front pointer, so
;; once the queue is empty the rear pointer still refers to the old last
;; pair and b is still shown.  The items are just the list in the car.

(define (print-queue queue)
  (display (front-ptr queue)))

(define (queue->string queue)
  (with-output-to-string (lambda () (print-queue queue))))

(define q1 (make-queue))
(check (insert-queue! q1 'a) => '((a) a))
(check (insert-queue! q1 'b) => '((a b) b))
(check (delete-queue! q1) => '((b) b))
(check (delete-queue! q1) => '(() b))

(define q2 (make-queue))
(check (queue->string q2) => "()")
(insert-queue! q2 'a)
(insert-queue! q2 'b)
(check (queue->string q2) => "(a b)")
(delete-queue! q2)
(check (queue->string q2) => "(b)")
(delete-queue! q2)
(check (queue->string q2) => "()")
