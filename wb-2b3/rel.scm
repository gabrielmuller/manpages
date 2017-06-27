
;;; Keep write latency down.
(define (alarm-interrupt)
  (alarm 1)
  (flush-ents 40 1))

(alarm 1)
