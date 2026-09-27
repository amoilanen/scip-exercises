(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

;; Each element is the previous one doubled: the powers of two.
(define s (cons-stream 1 (add-streams s s)))

(check (stream-head s 8) => '(1 2 4 8 16 32 64 128))
