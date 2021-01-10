;;; TODO:
;;; - [ ] Add some way to exclude auto updates (maybe with labels)
(import
  chicken.process-context
  chicken.string
  chicken.port)

(import
  ; NOTE: Not an egg yet. Can be found here: https://github.com/siiky/cling
  cling
  srfi-1
  srfi-42
  transmission
  transmission.utils
  typed-records)

(include "connection-options.scm")

(defstruct options
           host
           port
           authenv
           username
           password

           high-priority-ratio
           low-priority-ratio

           help
           rest
           )

(define *OPTS*
  (cling
    (lambda (ret . rest)
      (update-options ret #:rest rest))

    (arg '((-h --help))
         #:help "Show this help text"
         #:kons (lambda (ret _ _) (update-options ret #:help #t)))


    (arg '((--high-priority-ratio) . ratio)
         #:help "Torrents with ratio < this are considered high priority."
         #:kons (lambda (ret _ ratio)
                  (let ((ratio (string->number ratio)))
                    (assert (number? ratio))
                    (update-options ret #:high-priority-ratio ratio))))

    (arg '((--low-priority-ratio) . ratio)
         #:help "Torrents with ratio >= this are considered low priority."
         #:kons (lambda (ret _ ratio)
                  (let ((ratio (string->number ratio)))
                    (assert (number? ratio))
                    (update-options ret #:low-priority-ratio ratio))))
    ))

(define (help*)
  (help *connection-opts*)
  (help *OPTS*))

(define (process-arguments* #!optional (args (command-line-arguments)))
  (process-arguments *OPTS*
                     (make-options #:low-priority-ratio 4 #:high-priority-ratio 2)
                     (set-connection-options! args)))

(define ((alist? key-pred? #!optional (value-predicate? (constantly #t))) lst)
  (and (list? lst)
       (every (lambda (elem)
                (and (pair? elem)
                     (key-pred? (car elem))
                     (value-predicate? (cdr elem))))
              lst)))

(define (group-by groups lst)
  (define (kons elem ret)
    (let ((group-key (member elem groups
                             (lambda (elem group-key/group-pred?)
                               (let ((group-key (car group-key/group-pred?))
                                     (group-pred? (cdr group-key/group-pred?)))
                                 (group-pred? elem))))))
      (assert group-key)
      (let* ((group-key (caar group-key))
             (group-elems (alist-ref group-key ret)))
        (assert group-elems)
        (let ((group-elems (cons elem group-elems)))
          (alist-update group-key group-elems ret)))))

  (define knil (map (lambda (kv) (cons (car kv) '())) groups))

  (assert ((alist? symbol? procedure?) groups))
  (fold kons knil lst))

(define ((*-priority? pred?) torrent)
  (alist-let/and torrent (uploadRatio)
                 (pred? uploadRatio)))

(define (low-priority? low) (*-priority? (cute >= <> low)))
(define (normal-priority? low high) (*-priority? (lambda (ratio) (and (<= ratio low) (>= ratio high)))))
(define (high-priority? high) (*-priority? (cute < <> high)))

(define ((transmission-update-seed-priority* priority) tors)
  (let ((hashes (list-ec (:list tor tors)
                         (:let bandwidthPriority (alist-ref 'bandwidthPriority tor))
                         (:let hashString (alist-ref 'hashString tor))
                         (:let status (alist-ref 'status tor))
                         (not (= bandwidthPriority priority))
                         (if (or (= status status/seed-wait) (= status status/seed)))
                         hashString)))
    (unless (null? hashes)
      (with-transmission-result (torrent-set #:ids hashes #:bandwidth-priority priority)
                                (constantly #t)
                                (lambda (result . _) (eprint "Failed to update priority: " result))))))

(define transmission-update-seed-priority/low (transmission-update-seed-priority* priority/low))
(define transmission-update-seed-priority/normal (transmission-update-seed-priority* priority/normal))
(define transmission-update-seed-priority/high (transmission-update-seed-priority* priority/high))

(define (main args)
  (let ((options (process-arguments* args)))
    (if (options-help options)
        (help*)
        (with-transmission-result (torrent-get '("bandwidthPriority" "hashString" "status" "uploadRatio") #:ids #f)
                                  (lambda (arguments . _)
                                    (alist-let/and arguments (torrents)
                                                   (alist-let/and (group-by `((high . ,(high-priority? (options-high-priority-ratio options)))
                                                                              (normal . ,(normal-priority? (options-low-priority-ratio options) (options-high-priority-ratio options)))
                                                                              (low . ,(low-priority? (options-low-priority-ratio options))))
                                                                            (vector->list torrents))
                                                                  (low normal high)
                                                                  (transmission-update-seed-priority/low low)
                                                                  (transmission-update-seed-priority/normal normal)
                                                                  (transmission-update-seed-priority/high high))))
                                  (lambda (result . _)
                                    (eprint "Failed to get torrents: " result))))))

(main (command-line-arguments))
