(import chicken.pretty-print)
(import chicken.process-context)
(import chicken.repl)
(import chicken.string)

(import optimism)
(import srfi-1)
(import srfi-42)

(import transmission)
(import transmission.utils)

(define (torrent-get-example)
  (display "torrent-get-example: ")
  (print-result (torrent-get '("id" "totalSize") #:tag (unique-tag))))

(define (session-get-example)
  (display "session-get-example: ")
  (print-result (session-get #:tag (unique-tag))))

(define (print-result result)
  (when result ; rpc-call returns #f on wrong parameters
    (let ((reply (vector->list result)))
      (let ((arguments (cdr (assoc "arguments" reply)))
            (result (cdr (assoc "result" reply))))
        (if (string=? result "success")
            (pp arguments)
            (pp result))))))

(define (init args)
  (define *OPTS*
    `(((--host) . host)
      ((--port) . ,string->number)
      ((--authenv -ne))
      ((--username) . username)
      ((--password) . password)
      ((--session-id) . session-id)
      ((--repl))))

  (let ((pargs (parse-command-line args *OPTS*)))
    (let ((host (alist-ref '--host pargs))
          (port (alist-ref '--port pargs))
          (authenv (or (alist-ref '--auth-env pargs) (alist-ref '-ne pargs)))
          (username (alist-ref '--username pargs))
          (password (alist-ref '--password pargs))
          (session-id (alist-ref '--session-id pargs))
          (repl? (not (not (assoc '--repl pargs)))))
      (when host (*host* host))
      (when port (*port* port))
      (when session-id (*session-id* session-id))

      ; Update authentication variables
      (cond
        (authenv
          (let ((tr_auth (get-environment-variable "TR_AUTH")))
            (unless tr_auth
              (eprint "TR_AUTH not set"))
            (let ((username/password (string-split tr_auth ":")))
              (unless (= (length username/password) 2)
                (eprint "Splitting TR_AUTH by ':' didn't result in 2 elements; authentication will most likely fail."))
              (let ((username (car username/password))
                    (password (cadr username/password)))
                (*username* username)
                (*password* password)))))

        ((or username password)
         (unless (and username password)
           (eprint "Did you forget to set both username and password? Authentication may fail."))
         (when username (*username* username))
         (when password (*password* password))))

      repl?)))

(define (main args)
  (if (init args)
      (repl)

      (with-transmission-result
        (result-bind
          (torrent-get '("id" "downloadDir" "status" "name") #:ids #f #:tag (unique-tag))

          (lambda (arguments . _)
            (alist-let/and arguments (torrents)
                           (let ((ids (list-ec (:vector tor torrents)
                                               (:let status (reply-ref 'status tor))
                                               (:let download-dir (reply-ref 'downloadDir tor))
                                               (and (member status `(,status/seed ,status/seed-wait))
                                                    (string=? download-dir "/some/path/to/files/"))
                                               (:let id (reply-ref 'id tor))
                                               id)))
                             (torrent-get '("id" "uploadRatio") #:ids ids #:tag (unique-tag)))))

          (lambda (arguments . _)
            (alist-let/and arguments (torrents)
                           (result/ok (list-ec (:vector tor torrents)
                                               (:let upload-ratio (reply-ref 'uploadRatio tor))
                                               (if (> upload-ratio 2))
                                               (:let id (reply-ref 'id tor))
                                               id)))))
        (lambda (ids)
          (pp ids))

        (lambda (result . _)
          (print "something failed: " result)))))

(main (command-line-arguments))
