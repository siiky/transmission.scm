;;; TODO:
;;; - [ ] Consistent error handling
;;; - [ ] Create a base procedure that does all the heavy lifting

(import
  chicken.process-context
  chicken.string
  chicken.port)

(import
  cling
  (only srfi-1
        any
        lset-difference
        lset-union
        member)
  typed-records)

(import
  transmission
  transmission.utils)

(include "connection-options.scm")

(define-constant *HELP-TEXT*
#<<EOF
transmission-labels add    TORRENTS LABELS ...
transmission-labels get    TORRENTS
transmission-labels remove TORRENTS LABELS ...
transmission-labels set    TORRENTS [LABELS ...]

`add` adds the given LABELS to the specified TORRENTS.
`get` lists the labels associated with the specified TORRENTS.
`remove` removes the given LABELS from the specified TORRENTS.
`set` sets the given LABELS for the specified TORRENTS.

`add` and `remove` have no effect when called with no labels.
To remove all labels just call `set` with no labels.

EOF
)

(define (help* . _)
  (help *connection-opts*)
  (print *HELP-TEXT*)
  (exit 1))

(define (torrent-ids-string->transmission-ids str)
  (define (hash/id-string->hash/id str)
    (if (= (string-length str) 40)
        str
        (string->number str)))

  (cond
    ((string=? str "all")
     #f)
    ((member str '("active" "recently-active") string=?)
     "recently-active")
    (else
      (let ((ret (map hash/id-string->hash/id (string-split str "," #t))))
        (if (any not ret)
            '()
            ret)))))

(define ((add/remove op) torrents labels-to-add/remove)
  (unless (null? labels-to-add/remove)
    (let ((torrents (torrent-ids-string->transmission-ids torrents)))
      (unless (null? torrents)
        (with-transmission-result (torrent-get '("hashString" "labels") #:ids torrents)
                                  (lambda (arguments . _)
                                    (alist-let/and arguments (torrents)
                                                   (for-each
                                                     (lambda (torrent)
                                                       (alist-let/and torrent ((hash-string hashString) labels)
                                                                      (let ((labels (op string=? (vector->list labels) labels-to-add/remove)))
                                                                        (with-transmission-result (torrent-set #:ids hash-string #:labels labels)
                                                                                                  (lambda _ print "Success!")
                                                                                                  (lambda (result/con . _)
                                                                                                    (if (condition? result/con)
                                                                                                        (print-error-message result/con (current-output-port) "Failed:")
                                                                                                        (print "Failed: " result/con)))))))
                                                     (vector->list torrents)))))))))

(define (add torrents . labels-to-add)
  ((add/remove lset-union) torrents labels-to-add))

(define (get torrents . rest)
  (unless (null? rest)
    (help*))

  (let ((torrents (torrent-ids-string->transmission-ids torrents)))
    (unless (null? torrents)
      (with-transmission-result (torrent-get '("id" "hashString" "labels") #:ids torrents)
                                (lambda (arguments . _)
                                  (alist-let/and arguments (torrents)
                                                 (print "Hash                                    \tID\tLabels")
                                                 (for-each
                                                   (lambda (torrent)
                                                     (alist-let/and torrent (id (hash-string hashString) labels)
                                                                    (print hash-string #\tab id #\tab labels)))
                                                   (vector->list torrents))))))))

(define (remove torrents . labels-to-remove)
 ((add/remove lset-difference) torrents labels-to-remove))

(define (set torrents . labels)
  (let ((torrents (torrent-ids-string->transmission-ids torrents)))
    (unless (null? torrents)
      (torrent-set #:ids torrents #:labels labels))))

(define (main args)
  (let ((args (set-connection-options! args)))
    (if (or (null? args)
            (null? (cdr args)))
        (help*)
        (apply (alist-ref (car args)
                          `(("add"    . ,add)
                            ("get"    . ,get)
                            ("remove" . ,remove)
                            ("set"    . ,set))
                          string=?
                          help*)
               (cdr args)))))

(main (command-line-arguments))
