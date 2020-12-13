(import srfi-1 transmission transmission.utils)

(parameterize ((*host* "hostname")
               (*username* "username")
               (*password* "password"))
  (let ((tag (unique-tag!)))
    (with-transmission-result
      (torrent-get '("downloadDir" "id" "name" "status" "uploadRatio") #:ids #f #:tag tag)
      (lambda (arguments tag req resp)
        (define (want-torrent? tor)
          (alist-let/and tor (downloadDir status uploadRatio)
                         (and (= status status/seed)
                              (> uploadRatio 1)
                              (string=? downloadDir "/some/path/"))))
        (alist-let/and arguments (torrents)
                       (let ((wanted-tors (filter want-torrent? (vector->list torrents))))
                         (for-each print wanted-tors))))
      ; NOTE: The error case handling procedure doesn't have to accept
      ;       #!optional arguments -- the "missing" arguments will be #f.
      (lambda (result tag req resp)
        (error 'here "torrent-get call failed with the following error" result)))))
