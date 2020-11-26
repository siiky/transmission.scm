(module
  transmission.utils
  (
   avector-ref
   avector?

   reply-arguments
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
   )

  (import
    scheme
    (only chicken.base
          add1
          alist-ref
          cute))

  (define (avector? obj)
    (vector? obj))

  (define (avector-ref key avector #!optional (==? equal?))
    (alist-ref key (vector->list avector) ==?))

  (define (reply-result reply)
    (avector-ref "result" reply))

  (define (reply-arguments reply)
    (avector-ref "arguments" reply))

  (define (reply-result-success? result)
    (and (string? result) (string=? result "success")))

  (define (reply-success? reply)
    (reply-result-success? (reply-result reply)))

  (define (reply-ref-path reply path #!optional (==? equal?))
    (cond
      ((null? path)
       reply)

      ((avector? reply) ; table?
       (let ((phead (car path))
             (ptail (cdr path)))
         (let ((branch (avector-ref phead reply ==?)))
           (reply-ref-path branch ptail ==?))))

      ((list? reply) ; array?
       (map (cute reply-ref-path <> path ==?) reply))

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
