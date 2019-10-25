(module
  transmission.utils
  (
   avector-get
   avector?

   reply-arguments
   reply-get
   reply-result
   reply-success?
   )

  (import
    scheme
    (only chicken.base
          add1
          cute))

  (define (avector? obj)
    (vector? obj))

  (define (avector-get avector key #!key (==? equal?))
    (and (avector? avector)
         (let ((len (vector-length avector)))
           (let loop ((idx 0))
             (and (< idx len)
                  (let ((elem (vector-ref avector idx)))
                    (if (==? key (car elem))
                        (cdr elem)
                        (loop (add1 idx)))))))))

  (define (reply-result reply)
    (avector-get reply "result"))

  (define (reply-arguments reply)
    (avector-get reply "arguments"))

  (define (reply-result-success? result)
    (and (string? result) (string=? result "success")))

  (define (reply-success? reply)
    (reply-result-success? (reply-result reply)))

  (define (reply-get reply path #!key (==? equal?))
    (cond
      ((null? path)
       reply)

      ((avector? reply) ; table?
       (let ((phead (car path))
             (ptail (cdr path)))
         (let ((branch (avector-get reply phead #:==? ==?)))
           (reply-get branch ptail #:==? ==?))))

      ((list? reply) ; array?
       (map (cute reply-get <> path #:==? ==?) reply))

      (else #f)))
  )
