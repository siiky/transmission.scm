;;; This example shows how to make the RPC call below
;;;
;;; ```json
;;; {
;;;     "method": "torrent-get",
;;;     "arguments": {
;;;         "fields": [ "id", "totalSize" ]
;;;     }
;;; }
;;; ```

(import chicken.process-context)
(import transmission)
(import srfi-1)

(define (maybe-list-ref l n)
  (cond
    ((null? l) #f)
    ((zero? n) (car l))
    (else (maybe-list-ref (cdr l) (- n 1)))))

(define (main args)
  (let ((username (maybe-list-ref args 0))
        (password (maybe-list-ref args 1)))
    (parameterize ((*username* username)
                   (*password* password)
                   (*session-id* #f)) ; 2.3.1 x-transmission-session-id
      (let ((result (rpc-call "torrent-get" #:arguments '#(("fields" . ("id" "totalSize"))))))
        (if result ; rpc-call returns #f on wrong parameters
            (let ((reply (vector->list result)))
              (let ((arguments (cdr (assoc "arguments" reply)))
                    (result (cdr (assoc "result" reply))))
                (if (string=? result "success")
                    (print arguments)
                    (print result))))

            (print (program-name) " [USERNAME PASSWORD]"))))))

(main (command-line-arguments))
