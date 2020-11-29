(import chicken.port)
(import test srfi-1)
(import transmission transmission.utils)

(define-syntax define!
  (syntax-rules ()
    ((define! func formals body ...)
     (set! func (lambda formals body ...)))))

(define-syntax alist-let
  (syntax-rules ()
    ((alist-let alist (key ...)
                body ...)
     (and alist
          (let ((key (alist-ref 'key alist))
                ...)
            body
            ...)))))

(define-syntax defhandler
  (syntax-rules ()
    ((defhandler (handler-name msg method arguments tag) body ...)
     `(,(symbol->string 'handler-name)
        . ,(lambda (msg)
             (alist-let msg (method arguments tag)
                        (and tag (begin body ...))))))))

(define (false-or pred? obj)
  (or (not obj) (pred? obj)))

(define (fields? obj)
  (and (list? obj) (every string? obj)))

(define (ids? obj)
  (or (id? obj)
      (and (vector? obj) (every id? (vector->list obj)))))

(define handlers
  (let ((test-3.1/4.6
          (lambda (arguments)
            (or (not arguments)
                (alist-ref 'ids arguments eq? #f)))))
    `(
      ,(defhandler (blocklist-update msg method arguments tag)
                   (not arguments))

      ,(defhandler (free-space msg method arguments tag)
                   (alist-let arguments (path)
                              path))

      ,(defhandler (port-test msg method arguments tag)
                   (not arguments))

      ,(defhandler (queue-move-bottom msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-down msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-top msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-up msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (session-close msg method arguments tag)
                   (not arguments))

      ,(defhandler (session-get msg method arguments tag)
                   (or (not arguments)
                       (alist-let arguments (fields)
                                  (and (vector? fields)
                                       (fields? (vector->list fields))))))

      ,(defhandler (session-set msg method arguments tag)
                   (let ((right-type-for-key?
                           (lambda (kv)
                             (let ((key (car kv))
                                   (value (cdr kv)))
                               (case key
                                 ((
                                   alt-speed-enabled
                                   alt-speed-time-enabled
                                   blocklist-enabled
                                   dht-enabled
                                   download-queue-enabled
                                   idle-seeding-limit-enabled
                                   incomplete-dir-enabled
                                   lpd-enabled
                                   peer-port-random-on-start
                                   pex-enabled
                                   port-forwarding-enabled
                                   queue-stalled-enabled
                                   rename-partial-files
                                   script-torrent-done-enabled
                                   seed-queue-enabled
                                   seed-ratio-limited
                                   speed-limit-down-enabled
                                   speed-limit-up-enabled
                                   start-added-torrents
                                   trash-original-torrent-files
                                   utp-enabled)
                                  (boolean? value))

                                 ((
                                   alt-speed-down
                                   alt-speed-time-begin
                                   alt-speed-time-day
                                   alt-speed-time-end
                                   alt-speed-up
                                   cache-size-mb
                                   download-queue-size
                                   idle-seeding-limit
                                   peer-limit-global
                                   peer-limit-per-torrent
                                   peer-port
                                   queue-stalled-minutes
                                   seed-queue-size
                                   seed-ratio-limit
                                   speed-limit-down
                                   speed-limit-up)
                                  (number? value))

                                 ((
                                   blocklist-url
                                   download-dir
                                   encryption
                                   incomplete-dir
                                   script-torrent-done-filename
                                   )
                                  (string? value))

                                 ; NOTE: I don't understand what this argument is
                                 ;       supposed to have, so I'll assume that
                                 ;       whatever value it has, it's right.
                                 ((units) #t)

                                 (else #f))))))
                     (false-or (cute every right-type-for-key?  <>)
                               arguments)))

      ,(defhandler (session-stats msg method arguments tag)
                   (not arguments))

      ,(defhandler (torrent-add msg method arguments tag)
                   #f)

      ,(defhandler (torrent-get msg method arguments tag)
                   (alist-let arguments (fields ids format)
                              (and fields
                                   (false-or ids? ids)
                                   (false-or format? format))))

      ,(defhandler (torrent-reannounce msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-remove msg method arguments tag)
                   #f)

      ,(defhandler (torrent-rename-path msg method arguments tag)
                   #f)

      ,(defhandler (torrent-set msg method arguments tag)
                   (let ((right-type-for-key?
                           (lambda (kv)
                             (let ((key (car kv))
                                   (value (cdr kv)))
                               (case key
                                 ((
                                   bandwidthPriority
                                   downloadLimit
                                   peer-limit
                                   queuePosition
                                   seedIdleLimit
                                   seedIdleMode
                                   seedRatioLimit
                                   seedRatioMode
                                   uploadLimit)
                                  (number? value))

                                 ((
                                   files-unwanted
                                   files-wanted
                                   labels
                                   priority-high
                                   priority-low
                                   priority-normal
                                   trackerAdd
                                   trackerRemove
                                   trackerReplace)
                                  #t) ; TODO: array

                                 ((
                                   downloadLimited
                                   honorsSessionLimits
                                   uploadLimited)
                                  (boolean? value))

                                 ((ids) (ids? value))
                                 ((location) (string? value))
                                 (else #f))))))
                     (false-or (cute every right-type-for-key?  <>)
                               arguments)))

      ,(defhandler (torrent-set-location msg method arguments tag)
                   #t)

      ,(defhandler (torrent-start msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-start-now msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-stop msg method arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-verify msg method arguments tag)
                   (test-3.1/4.6 arguments)))))

(define (test-ids function #!optional (test-default? #t))
  (when test-default?
    (test-assert (function)))
  (test-assert (function #f))
  (test-assert (function '()))
  (test-assert (function "recently-active"))
  (test-assert (function 42))
  (test-assert (function "0000000000000000000000000000000000000000"))
  (test-assert (function '(42 "0000000000000000000000000000000000000000"))))

(define (test-group-3.1/4.6 group-name function)
  (test-group group-name
    (test-ids (lambda (#!optional (ids 'none))
                (apply function #:tag (unique-tag)
                       (if (eq? ids 'none) '() `(#:ids ,ids)))))))

(define!
  http-call (req msg)
  (and-let* ((method (alist-ref 'method msg eq?))
             (handler (alist-ref method handlers string=?)))
    (let ((test-res (handler msg)))
      (unless test-res
        (with-output-to-port
          (current-error-port)
          (lambda ()
            (print "\n\nTHE MESSAGE:\t" msg "\n"))))
      test-res)))

(set! make-serialized-message make-message)

; Make sure it doesn't connect to any running transmission daemon.
(*host* "<null>")
(*port* 0)

(test-group "transmission"
  (test-assert (blocklist-update #:tag (unique-tag)))
  (test-assert (free-space "/some/phony/path/" #:tag (unique-tag)))
  (test-assert (port-test #:tag (unique-tag)))
  (test-group-3.1/4.6 "queue-move-bottom" queue-move-bottom)
  (test-group-3.1/4.6 "queue-move-down" queue-move-down)
  (test-group-3.1/4.6 "queue-move-top" queue-move-top)
  (test-group-3.1/4.6 "queue-move-up" queue-move-up)
  (test-assert (session-close #:tag (unique-tag)))
  (test-group "session-get"
    (test-assert (session-get #:tag (unique-tag)))
    (test-assert (session-get #:fields '("encryption") #:tag (unique-tag))))

  ; TODO
  (test-assert (session-set #:tag (unique-tag)))

  (test-assert (session-stats #:tag (unique-tag)))

  ;(test-assert (torrent-add #:tag (unique-tag)))
  (test-group "torrent-get"
    (test-ids (lambda (#!optional (ids 'none))
                (apply torrent-get '("id") #:tag (unique-tag)
                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
  (test-group-3.1/4.6 "torrent-reannounce" torrent-reannounce)

  ;(test-assert (torrent-remove #:tag (unique-tag)))
  ;(test-assert (torrent-rename-path #:tag (unique-tag)))

  ; TODO
  (test-assert (torrent-set #:tag (unique-tag)))

  (test-group "torrent-set-location"
    (test-ids (cute torrent-set-location <> "/some/phony/path/" #:tag (unique-tag))
              #f))

  (test-group-3.1/4.6 "torrent-start" torrent-start)
  (test-group-3.1/4.6 "torrent-start-now" torrent-start-now)
  (test-group-3.1/4.6 "torrent-stop" torrent-stop)
  (test-group-3.1/4.6 "torrent-verify" torrent-verify))

(test-group "transmission.utils"
  )

(test-exit)
