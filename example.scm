;;; torrent-get-example makes the RPC call
;;; ```json
;;; {
;;;     "method": "torrent-get",
;;;     "arguments": {
;;;         "fields": [ "id", "totalSize" ]
;;;     }
;;; }
;;; ```
;;;
;;; session-get-example makes the RPC call
;;; ```json
;;; {
;;;     "method": "session-get"
;;; }
;;; ```

(import chicken.pretty-print)
(import chicken.process-context)
(import chicken.repl)

(import optimism)
(import srfi-1)

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

(define *OPTS*
  `(((--host) . host)
    ((--port) . ,string->number)
    ((--username) . username)
    ((--password) . password)
    ((--session-id) . session-id)
    ((--repl))))

(define (init args)
  (let ((pargs (parse-command-line args *OPTS*)))
    (let ((host (alist-ref '--host pargs))
          (port (alist-ref '--port pargs))
          (username (alist-ref '--username pargs))
          (password (alist-ref '--password pargs))
          (session-id (alist-ref '--session-id pargs))
          (repl? (not (not (assoc '--repl pargs)))))
      (when host (*host* host))
      (when port (*port* port))
      (when username (*username* username))
      (when password (*password* password))
      (when session-id (*session-id* session-id))
      repl?)))

(define (main args)
  (if (init args)
      (repl)
      (let ((reply (torrent-get '("id" "downloadDir" "status" "uploadRatio") #:ids #f #:tag (unique-tag))))
        (assert (reply-success? reply))
        (pp (filter
              (lambda (obj)
                (let ((status (reply-ref 'status obj))
                      (upload-ratio (reply-ref 'uploadRatio obj))
                      (download-dir (reply-ref 'downloadDir obj)))
                  (and (member status `(,status-seed ,status-seed-wait) =)
                       (> upload-ratio 2)
                       (string=? download-dir "/some/path/to/files/"))))
              (vector->list (reply-ref-path (reply-arguments reply) '(torrents))))))))

(main (command-line-arguments))
