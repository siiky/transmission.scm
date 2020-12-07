(module
  transmission.utils
  (
   reply-ref-path

   default-error-proc
   with-transmission-result

   status/check
   status/check-wait
   status/download
   status/download-wait
   status/seed
   status/seed-wait
   status/stopped

   priority/high
   priority/low
   priority/normal

   alist-keep-keys
   unique-tag

   alist-let/and
   alist-let/nor
   )

  (import
    (except scheme
            member)
    (only chicken.base
          add1
          cute
          error
          fixnum?))

  (import
    (only srfi-1
          filter
          member)
    (only scheme.base
          vector-map))

  (import
    (only transmission
          reply-ref
          result-ref))

  ; TODO: Is there a way to extract the common parts of `alist-let/and` and
  ;       `alist-let/nor`? Tried creating an auxiliary macro, but it wasn't
  ;       expanded when used inside of these two, and I don't understand why.

  (define-syntax alist-let/and
    (syntax-rules ()
      ((alist-let/and "rec" let-list alist () body ...)
       (let let-list
         body ...))

      ((alist-let/and "rec" let-list
                      alist ((variable-name key) . let-tail)
                      body ...)
       (alist-let/and "rec" ((variable-name (alist-ref 'key alist)) . let-list)
                      alist let-tail body ...))

      ((alist-let/and "rec" let-list
                      alist (key . let-tail)
                      body ...)
       (alist-let/and "rec" ((key (alist-ref 'key alist)) . let-list)
                      alist let-tail body ...))

      ((alist-let/and alist (key ...)
                      body ...)
       (let ((%alist alist))
         (and %alist
              (alist-let/and "rec" ()
                             %alist (key ...)
                             body ...))))))

  (define-syntax alist-let/nor
    (syntax-rules ()
      ((alist-let/nor "rec" let-list alist () body ...)
       (let let-list
         body ...))

      ((alist-let/nor "rec" let-list
                      alist ((variable-name key) . let-tail)
                      body ...)
       (alist-let/nor "rec" ((variable-name (alist-ref 'key alist)) . let-list)
                      alist let-tail body ...))

      ((alist-let/nor "rec" let-list
                      alist (key . let-tail)
                      body ...)
       (alist-let/nor "rec" ((key (alist-ref 'key alist)) . let-list)
                      alist let-tail body ...))

      ((alist-let/nor alist (key ...)
                      body ...)
       (let ((%alist alist))
         (or (not %alist)
             (alist-let/nor "rec" ()
                            %alist (key ...)
                            body ...))))))

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

  ;; tr_priority_t from libtransmission/transmission.h
  (define priority/low   -1) ; TR_PRI_LOW
  (define priority/normal 0) ; TR_PRI_NORMAL
  (define priority/high   1) ; TR_PRI_HIGH

  ; TODO: API calls can fail with an exception; handle that too.
  (define (default-error-proc result tag req resp)
    (let ((msg (string-append
                 "RPC call "
                 (if (fixnum? tag)
                     (string-append "with tag " (number->string tag))
                     "")
                 " failed with the following error")))
      (error 'default-error-proc msg result)))

  ; NOTE: The same as result-ref, except the success and failure procedures are
  ;       flipped.
  (define (with-transmission-result result success-proc #!optional (error-proc default-error-proc))
    (result-ref result error-proc success-proc))
  )
