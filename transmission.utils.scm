(module
  transmission.utils
  (
   reply-arguments
   reply-ref
   reply-ref-path
   reply-result
   reply-success?

   status-check
   status-check-wait
   status-download
   status-download-wait
   status-seed
   status-seed-wait
   status-stopped

   unique-tag
   )

  (import
    scheme
    scheme.base
    (only chicken.base
          add1
          alist-ref
          cute))

  (define unique-tag
    (let ((n 0))
      (lambda ()
        (let ((ret n))
          (set! n (+ n 1))
          ret))))

  (define reply-ref alist-ref)

  (define (reply-result reply)
    (reply-ref 'result reply))

  (define (reply-arguments reply)
    (reply-ref 'arguments reply))

  (define (reply-result-success? result)
    (and (string? result) (string=? result "success")))

  (define (reply-success? reply)
    (reply-result-success? (reply-result reply)))

  (define (reply-ref-path reply path #!optional (==? equal?))
    (cond
      ((null? path)
       reply)

      ((list? reply) ; table?
       (let ((phead (car path))
             (ptail (cdr path)))
         (let ((branch (reply-ref phead reply ==?)))
           (reply-ref-path branch ptail ==?))))

      ((vector? reply) ; array?
       (vector-map (cute reply-ref-path <> path ==?) reply))

      (else #f)))

  ;; tr_torrent_activity from libtransmission/transmission.h
  (define status-stopped       0)
  (define status-check-wait    1)
  (define status-check         2)
  (define status-download-wait 3)
  (define status-download      4)
  (define status-seed-wait     5)
  (define status-seed          6)
  )
