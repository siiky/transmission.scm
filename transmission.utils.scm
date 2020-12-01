(module
  transmission.utils
  (
   reply-arguments
   reply-ref
   reply-ref-path
   reply-result
   reply-success?
   reply-tag

   status/check
   status/check-wait
   status/download
   status/download-wait
   status/seed
   status/seed-wait
   status/stopped

   alist-keep-keys
   unique-tag

   :treply
   alist-let/and
   alist-let/nor
   )

  (import
    scheme
    scheme.base
    (only chicken.base
          add1
          alist-ref
          cute
          fixnum?))

  (import
    (only srfi-1
          filter))

  ; This seems to work even without importing SRFI-42, which is awesome.
  (define-syntax :treply
    (syntax-rules ()
      ((:treply cc var reply key ...)
       (:vector cc var (reply-ref-path (reply-arguments reply) '(key ...))))))

  (define-syntax alist-let/and
    (syntax-rules ()
      ((alist-let/and alist (key ...)
                      body ...)
       (and alist
            (let ((key (alist-ref 'key alist))
                  ...)
              body
              ...)))))

  (define-syntax alist-let/nor
    (syntax-rules ()
      ((alist-let/nor alist (key ...)
                      body ...)
       (or (not alist)
           (let ((key (alist-ref 'key alist))
                 ...)
             body
             ...)))))


  (define unique-tag
    (let ((n 0))
      (lambda (#!optional (new-n #f))
        (if (fixnum? new-n)
            (begin
              (set! n new-n)
              (unique-tag))
            (let ((ret n))
              (set! n (add1 n))
              ret)))))

  (define (alist-keep-keys alist . keys)
    (filter (lambda (kv) (member (car kv) keys eq?)) alist))

  (define reply-ref alist-ref)

  (define (reply-result reply)
    (reply-ref 'result reply))

  (define (reply-arguments reply)
    (reply-ref 'arguments reply))

  (define (reply-tag reply)
    (reply-ref 'tag reply))

  (define (reply-result-success? result)
    (and (string? result) (string=? result "success")))

  (define (reply-success? reply)
    (and reply (reply-result-success? (reply-result reply))))

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
  (define status/stopped       0)
  (define status/check-wait    1)
  (define status/check         2)
  (define status/download-wait 3)
  (define status/download      4)
  (define status/seed-wait     5)
  (define status/seed          6)

  ; TODO: Take a look at SRFI-189.
  (define (transmission-do reply func)
    (if (reply-success? reply)
        (func (reply-arguments reply))
        #f))
  )
