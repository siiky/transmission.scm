(import chicken.port)
(import test)
(import transmission transmission.utils)

(define-syntax define!
  (syntax-rules ()
    ((define! func formals body ...)
     (set! func (lambda formals body ...)))))

(define-syntax let-aref
  (syntax-rules ()
    ((let-aref (alist key ...)
               body ...)
     (let ((key (alist-ref 'key alist))
           ...)
       body
       ...))))

(define-syntax defhandler
  (syntax-rules ()
    ((defhandler handler-name (msg method arguments tag) body ...)
     `(,(symbol->string 'handler-name)
        . ,(lambda (msg)
             (let-aref (msg method arguments tag)
                       (and tag (begin body ...))))))))

(define handlers
  (let ((test-3.1/4.6
          (lambda (arguments)
            (or (not arguments)
                (alist-ref 'ids arguments eq? #f)))))
    `(
      ,(defhandler
         blocklist-update (msg method arguments tag)
         (not arguments))

      ,(defhandler
         free-space (msg method arguments tag)
         (and arguments
              (let-aref (arguments path)
                        path)))

      ,(defhandler
         port-test (msg method arguments tag)
         (not arguments))

      ,(defhandler
         queue-move-bottom (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         queue-move-down (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         queue-move-top (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         queue-move-up (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         session-close (msg method arguments tag)
         (not arguments))

      ,(defhandler
         session-get (msg method arguments tag)
         #f)

      ,(defhandler
         session-set (msg method arguments tag)
         #f)

      ,(defhandler
         session-stats (msg method arguments tag)
         (not arguments))

      ,(defhandler
         torrent-add (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-get (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-reannounce (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         torrent-remove (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-rename-path (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-set (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-set-location (msg method arguments tag)
         #f)

      ,(defhandler
         torrent-start (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         torrent-start-now (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         torrent-stop (msg method arguments tag)
         (test-3.1/4.6 arguments))

      ,(defhandler
         torrent-verify (msg method arguments tag)
         (test-3.1/4.6 arguments)))))

(define (test-group-3.1/4.6 group-name function)
  (test-group group-name
    (test-assert (function #:tag (unique-tag)))
    (test-assert (function #:ids #f #:tag (unique-tag)))
    (test-assert (function #:ids '() #:tag (unique-tag)))
    (test-assert (function #:ids "recently-active" #:tag (unique-tag)))
    (test-assert (function #:ids 42 #:tag (unique-tag)))
    (test-assert (function #:ids "0000000000000000000000000000000000000000" #:tag (unique-tag)))
    (test-assert (function #:ids '(42 "0000000000000000000000000000000000000000") #:tag (unique-tag)))))

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

; Make sure it doesn't connect to a possibly running transmission daemon.
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

  ;(test-assert (session-get #:tag (unique-tag)))
  ;(test-assert (session-set #:tag (unique-tag)))

  (test-assert (session-stats #:tag (unique-tag)))

  ;(test-assert (torrent-add #:tag (unique-tag)))
  ;(test-assert (torrent-get '("id") #:tag (unique-tag)))

  (test-group-3.1/4.6 "torrent-reannounce" torrent-reannounce)

  ;(test-assert (torrent-remove #:tag (unique-tag)))
  ;(test-assert (torrent-rename-path #:tag (unique-tag)))
  ;(test-assert (torrent-set #:tag (unique-tag)))
  ;(test-assert (torrent-set-location #:tag (unique-tag)))

  (test-group-3.1/4.6 "torrent-start" torrent-start)
  (test-group-3.1/4.6 "torrent-start-now" torrent-start-now)
  (test-group-3.1/4.6 "torrent-stop" torrent-stop)
  (test-group-3.1/4.6 "torrent-verify" torrent-verify))

(test-group "transmission.utils"
  )

(test-exit)
