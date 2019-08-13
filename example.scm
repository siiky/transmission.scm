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

(define unique-tag
  (let ((n 0))
    (lambda ()
      (let ((ret n))
        (set! n (+ n 1))
        ret))))

(define (maybe-list-ref l n)
  (cond
    ((null? l) #f)
    ((zero? n) (car l))
    (else (maybe-list-ref (cdr l) (- n 1)))))

(define (maybe-rest-args args)
  (if (maybe-list-ref args 2)
      (cddr args)
      '()))

(define (torrent-get-example args)
  (let ((result (rpc-call "torrent-get" #:tag (unique-tag)
                          #:arguments '#(("fields" . ("id" "totalSize"))))))
    (print-result result (lambda (pn) (print pn  " [USERNAME PASSWORD]")))))

(define (torrent-start-example args)
  (print-result (torrent-start #:ids args #:tag (unique-tag))
                (lambda (pn) (print pn " USERNAME PASSWORD [IDS ...]"))))

(define (torrent-stop-example args)
  (print-result (torrent-stop #:ids args #:tag (unique-tag))
                (lambda (pn) (print pn " USERNAME PASSWORD [IDS ...]"))))

(define (print-result result usage)
  (if result ; rpc-call returns #f on wrong parameters
      (let ((reply (vector->list result)))
        (let ((arguments (cdr (assoc "arguments" reply)))
              (result (cdr (assoc "result" reply))))
          (if (string=? result "success")
              (print arguments)
              (print result))))
      (usage (program-name))))

(define (main args)
  (let ((username (maybe-list-ref args 0))
        (password (maybe-list-ref args 1))
        (rest (maybe-rest-args args)))
    (parameterize ((*username* username)
                   (*password* password)
                   (*session-id* #f)) ; 2.3.1 x-transmission-session-id
      (torrent-get-example rest)
      ;(torrent-start-example rest)
      ;(torrent-stop-example rest)
      )))

(main (command-line-arguments))
