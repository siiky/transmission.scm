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

(define unique-tag
  (let ((n 0))
    (lambda ()
      (let ((ret n))
        (set! n (+ n 1))
        ret))))

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
  `(((--port) . ,string->number)
    ((--username) . username)
    ((--password) . password)
    ((--session-id) . session-id)
    ((--repl))))

(define (assoc-val key alist)
  (let ((key/val (assoc key alist)))
    (if key/val (cdr key/val) #f)))

(define (init args)
  (let ((pargs (parse-command-line args *OPTS*)))
    (let ((username (assoc-val '--username pargs))
          (password (assoc-val '--password pargs))
          (port (assoc-val '--port pargs))
          (session-id (assoc-val '--session-id pargs))
          (repl? (assoc '--repl pargs)))
      (when username (*username* username))
      (when password (*password* password))
      (when port (*port* port))
      (when session-id (*session-id* session-id))
      repl?)))

(define (main args)
  (if (init args)
      (repl)
      (begin
        (torrent-get-example)
        (session-get-example))))

(main (command-line-arguments))
