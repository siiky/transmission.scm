(module
  transmission
  (
   *host*
   *password*
   *port*
   *session-id*
   *url*
   *username*

   rpc-call

   make-rpc-call
   define-rpc-call
   export-rpc-call
   )

  (import
    scheme
    (only chicken.base
          assert
          cut
          fixnum?
          identity
          make-parameter
          print)
    (only chicken.condition
          condition-case
          get-condition-property
          signal)
    (only chicken.module
          export)
    (only chicken.port
          with-output-to-string))

  (import
    (only http-client
          with-input-from-request)
    (only intarweb
          header-values
          headers
          make-request
          response-code
          response-headers)
    (only json
          json-read
          json-write)
    (only srfi-1
          every
          filter)
    (only uri-common
          make-uri))

  (define (assert* loc type type?)
    (lambda (x)
      (assert (type? x) loc "Expected " type ", but got " x)
      x))

  (define false? not)

  (define (or? . rest)
    (lambda (x)
      (let loop ((l rest))
        (or (null? l)
            ((car l) x)
            (loop (cdr l))))))

  (define *host* (make-parameter "localhost" (assert* '*host* "a string" string?)))

  ;;; RPC Parameters
  ;;; @see https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc

  ;; rpc-url
  (define *url* (make-parameter '(/ "transmission" "rpc")))

  ;; rpc-port
  (define *port*
    (make-parameter 9091 (assert* '*port* "an integer" fixnum?)))

  ;; rpc-username
  (define *username*
    (make-parameter #f (assert* '*username* "a string or #f" (or? false? string?))))

  ;; rpc-password
  (define *password*
    (make-parameter #f (assert* '*password* "a string or #f" (or? false? string?))))

  ;; 2.3.1 X-Transmission-Session-Id
  ;; @see https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  (define *session-id*
    (make-parameter #f (assert* '*session-id* "a string or #f" (or? false? string?))))

  (define (just x)     `(just . ,x))
  (define nothing      'nothing)
  (define (just? x)    (and (pair? x) (eq? (car x) 'just)))
  (define (nothing? x) (eq? x nothing))
  (define (maybe? x)   (or (nothing? x) (just? x)))
  (define (unwrap x)   (cdr x))
  (define (->maybe b)  (if b (just b) nothing))

  (define (maybe f x)
    (cond
      ((just? x) (just (f (unwrap x))))
      ((nothing? x) nothing)
      (else (f x))))

  (define (maybe-do . procs)
    (lambda (x)
      (let loop ((x x) (procs procs))
        (cond
          ((null? procs) x)
          ((nothing? x) nothing)
          (else (loop (maybe (car procs) x) (cdr procs)))))))

  ;; @brief Make an RPC call to a Transmission daemon
  ;; @param method A string naming an RPC method
  ;; @param arguments The arguments of this method
  ;; @param tag The tag for this call
  ;; @returns A response object read with json-read, or #f in case of wrong
  ;;          parameters
  ;;
  ;; Throws exceptions for HTTP errors, except for 409, which is handled
  ;;   according to 2.3.1; see http-client for other HTTP errors.
  ;;
  ;; `rpc-call` returns #f if parameters are wrong. Some parameters are
  ;; mandatory (with defaults provided): `*host*`, `*url*`, `*port*`.
  ;; Others are optional: `*session-id*`, `*password*`, `*username*`.
  ;; `*password*` and `*username*` are, however, optional "together"; they must
  ;; both be #f or a string.
  ;;
  ;; See the json egg for how to encode arguments and decode responses.
  (define (rpc-call method #!key (arguments #f) (tag #f))
    (define (call host url port username password method arguments tag)

      ;; @brief Create a request object
      ;; @param host The hostname of the RPC server
      ;; @param url See `rpc-url`
      ;; @param port See `rpc-port`
      ;; @param username See `rpc-username`
      ;; @param password See `rpc-password`
      ;; @returns A request object
      ;;
      ;; @a username and @a password are only required if
      ;;   `rpc-authentication-required` is enabled
      (define (make-req host url port username password)
        (make-request #:method 'POST
                      #:uri
                      (make-uri #:scheme 'http
                                #:host host
                                #:port port
                                #:path url
                                #:username username
                                #:password password)
                      ; NOTE: Not sure, but x-transmission-session-id set to #f
                      ;       seems to work
                      #:headers (headers `((x-transmission-session-id ,(*session-id*))))))

      ;; @brief Serialize a message, according to the spec
      ;; @param method The `method` field
      ;; @param arguments The `arguments` field
      ;; @param tag The `tag` field
      ;; @returns A string of the serialized JSON message
      (define (make-message method arguments tag)
        (define (message-req method arguments tag)
          (let ((optional (filter cdr `((arguments . ,arguments) (tag . ,tag)))))
            (list->vector `((method . ,method) . ,optional))))

        (with-output-to-string
          (cut json-write (message-req method arguments tag))))

      ;; @brief Handle 409, according to the spec
      ;; @param con The condition object
      ;; @param message The RPC message
      ;; @returns The deserialized response of the RPC call
      ;;
      ;; If an error other than 409 occurs, the exception is propagated
      (define (client-error-handler con message)
        (let ((response (get-condition-property con 'client-error 'response)))

          ; See 2.3.1 for how to handle 409
          (cond

            ; The condition object may not have a response property, in which
            ;   case, it is not a 409
            ((and response
                  (= (response-code response) 409))
             (let ((session-id (car (header-values 'x-transmission-session-id (response-headers response)))))
               ;(print "GOT A 409!")
               (*session-id* session-id)
               (let ((req (make-req host url port username password)))
                 (with-input-from-request req message json-read))))

            ; Rethrow any other errors
            (else
              ;(print "NOT A 409!")
              (signal con)))))

      (let ((req (make-req host url port username password))
            (message (make-message method arguments tag)))
        (condition-case
          (with-input-from-request req message json-read)
          (con (exn http client-error) ; 409 is in this condition kind
               (client-error-handler con message)))))

    (let ((host (*host*))
          (url (*url*))
          (port (*port*))
          (username (*username*))
          (password (*password*))
          (arguments (and arguments
                          (positive? (vector-length arguments))
                          arguments)))
      (and host url port
           (or (and username password)
               (not (or username password)))
           (call host url port username password method arguments tag))))

  ;;;
  ;;; General & common utilities
  ;;;

  ;; @brief Make a function that returns #f or an argument pair
  ;; @param key The argument's key
  ;; @param pre-proc Function that pre-processes an input value
  ;; @returns A function that, from an input value, returns #f or an argument pair
  ;;
  ;; @a pre-proc must return a Maybe. If the result of pre-proc is Nothing, the
  ;;   result is #f, and if the result is Just, then it returns an argument pair.
  (define (make-*->arguments key pre-proc)
    (lambda (x)
      (let ((x (pre-proc x)))
        (and (just? x) `(,key . ,(unwrap x))))))

  (define (id? id)
    (or (string? id) (fixnum? id)))

  ;; @brief Pre-process an ID
  ;; @param id An ID
  ;; @returns A Maybe
  (define (pre-proc-id id)
    (->maybe
      (and id
           (or (string? id) (fixnum? id))
           id)))

  ;; @brief Pre-process a list of IDs
  ;; @param ids A list of IDs
  ;; @returns A Maybe
  (define (pre-proc-ids ids)
    ((maybe-do
       ; Handle string ("recently-active") and list or vector of strings (hash)
       ;   and integers (id)
       (lambda (ids)
         (->maybe
           (cond
             ((id? ids) ids)
             ((and (vector? ids)
                   (positive? (vector-length ids)))
              (vector->list ids))
             ((list? ids) ids)
             (else #f))))

       (lambda (ids)
         (just (if (id? ids)
                   ids
                   (map unwrap (filter just? (map pre-proc-id ids)))))))

     ids))

  (define (pre-proc-array array)
    (->maybe
      (cond
        ; The json egg serializes scheme lists as JSON arrays
        ((vector? array) (vector->list array))
        ((list? array) array)
        (else #f))))

  (define (pre-proc-list-of-strings strs)
    (->maybe
      ((assert*
         'pre-proc-list-of-strings
         "a list of strings or #f"
         (or? false? (cut every string? <>)))
       strs)))

  (define pre-proc-fields pre-proc-list-of-strings)

  ;; TODO: Strict version
  (define pre-proc-object ->maybe)

  (define (pre-proc-bool b) (just (and (just? b) (unwrap b))))
  (define (pre-proc-string str) (->maybe (and str (string? str) str)))
  (define (pre-proc-number n) (->maybe (and n (number? n) n)))

  (define ids->arguments (make-*->arguments 'ids pre-proc-ids))
  (define fields->arguments (make-*->arguments 'fields pre-proc-fields))
  (define (make-number->arguments key) (make-*->arguments key pre-proc-number))
  (define (make-string->arguments key) (make-*->arguments key pre-proc-string))
  (define (make-bool->arguments key) (make-*->arguments key pre-proc-bool))
  (define (make-array->arguments key) (make-*->arguments key pre-proc-array))

  ;; @brief Create an RPC procedure
  ;; @param method The name of the RPC method
  ;; @param required A required parameter
  ;; @param required-handler A handler for a required parameter
  ;; @param key A key parameter
  ;; @param default The default value for a key parameter
  ;; @param key-handler A handler for a key parameter
  ;; @returns An RPC procedure
  ;;
  ;; This macro defines and exports an RPC procedure described by the given
  ;;   parameters.
  ;;
  ;; On top of the given key parameters, every RPC procedure has the `tag` key
  ;;   parameter, as per the RPC spec.
  ;;
  ;; All handlers (both required and key) must return either #f or a pair. This
  ;;   pair is the key and associated value of an RPC call argument.
  ;;
  ;; Examples:
  ;;
  ;; A call of no arguments (other than `tag`)
  ;;   (make-rpc-call some-method)
  ;;
  ;; A call with one required argument `fields` and one key argument `ids`
  ;;   (make-rpc-call (some-method (fields fields->arguments)) (ids #f ids->arguments))
  (define-syntax make-rpc-call
    (syntax-rules ()
      ((make-rpc-call (method (required required-handler) ...) (key default key-handler) ...)
       (let ((method-str (symbol->string 'method)))
         (lambda (required ... #!key (tag #f) (key default) ...)
           (let ((required (required-handler required)) ... (key (key-handler key)) ...)
             (let ((arguments (list->vector (filter identity `(,required ... ,key ...)))))
               (rpc-call method-str #:arguments arguments #:tag tag))))))
      ((make-rpc-call method (key default key-handler) ...)
       (make-rpc-call (method) (key default key-handler) ...))))

  ;; Like `make-rpc-call` but defines the created procedure
  (define-syntax define-rpc-call
    (syntax-rules ()
      ((define-rpc-call (method required ...) key ...)
       (define method (make-rpc-call (method required ...) key ...)))
      ((define-rpc-call method key ...)
       (define-rpc-call (method) key ...))))

  ;; Like `define-rpc-call` but exports the defined procedure
  (define-syntax export-rpc-call
    (syntax-rules ()
      ((export-rpc-call (method required ...) key ...)
       (begin
         (export method)
         (define-rpc-call (method required ...) key ...)))
      ((export-rpc-call method key ...)
       (export-rpc-call (method) key ...))))

  ;; Export RPC procedures of sections 3.1 and 4.6. These procedures have a
  ;;   single optional parameter `ids`.
  (define-syntax export-3.1/4.6
    (syntax-rules ()
      ((export-3.1/4.6 method)
       (export-rpc-call method (ids #f ids->arguments)))))

  (export-3.1/4.6 torrent-start)
  (export-3.1/4.6 torrent-start-now)
  (export-3.1/4.6 torrent-stop)
  (export-3.1/4.6 torrent-verify)
  (export-3.1/4.6 torrent-reannounce)

  (export-rpc-call
    torrent-set
    (bandwidth-priority    #f      (make-number->arguments 'bandwidthPriority))
    (download-limit        #f      (make-number->arguments 'downloadLimit))
    (download-limited      nothing (make-bool->arguments   'downloadLimited))
    (files-wanted          #f      (make-array->arguments  'files-wanted))
    (files-unwanted        #f      (make-array->arguments  'files-unwanted))
    (honors-session-limits nothing (make-bool->arguments   'honorsSessionLimits))
    (ids                   #f      (make-array->arguments  'ids))
    (labels                #f      (make-array->arguments  'labels))
    (location              #f      (make-string->arguments 'location))
    (peer-limit            #f      (make-number->arguments 'peer-limit))
    (priority-high         #f      (make-array->arguments  'priority-high))
    (priority-low          #f      (make-array->arguments  'priority-low))
    (priority-normal       #f      (make-array->arguments  'priority-normal))
    (queue-position        #f      (make-number->arguments 'queuePosition))
    (seed-idle-limit       #f      (make-number->arguments 'seedIdleLimit))
    (seed-idle-mode        #f      (make-number->arguments 'seedIdleMode))
    (seed-ratio-limit      #f      (make-number->arguments 'seedRatioLimit))
    (seed-ratio-mode       #f      (make-number->arguments 'seedRatioMode))
    (tracker-add           #f      (make-array->arguments  'trackerAdd))
    (tracker-remove        #f      (make-array->arguments  'trackerRemove))
    (tracker-replace       #f      (make-array->arguments  'trackerReplace))
    (upload-limit          #f      (make-number->arguments 'uploadLimit))
    (upload-limited        nothing (make-bool->arguments   'uploadLimited)))

  (export-rpc-call (torrent-get (fields fields->arguments)) (ids #f ids->arguments))

  ; As per the spec, either filename or metainfo must be given
  ; (torrent-add #f           "<info>")
  ; (torrent-add "<filename>" #f)
  ; TODO: Improve this; make it a pre-call error to provide neither or both
  (export-rpc-call
    (torrent-add
      (filename (make-string->arguments 'filename))
      (metainfo (make-string->arguments 'metainfo)))
    (cookies            #f      (make-string->arguments 'cookies))
    (download-dir       #f      (make-string->arguments 'download-dir))
    (paused             nothing (make-bool->arguments   'paused))
    (peer-limit         #f      (make-number->arguments 'peer-limit))
    (bandwidth-priority #f      (make-number->arguments 'bandwidthPriority))
    (files-wanted       #f      (make-array->arguments  'files-wanted))
    (files-unwanted     #f      (make-array->arguments  'files-unwanted))
    (priority-high      #f      (make-array->arguments  'priority-high))
    (priority-low       #f      (make-array->arguments  'priority-low))
    (priority-normal    #f      (make-array->arguments  'priority-normal)))

  (export-rpc-call
    (torrent-remove (ids ids->arguments))
    (delete-local-data nothing (make-bool->arguments 'delete-local-data)))

  (export-rpc-call
    (torrent-set-location
      (ids ids->arguments)
      (location (make-string->arguments 'location)))
    (move nothing (make-bool->arguments 'move)))

  (export-rpc-call
    (torrent-rename-path
      (ids ids->arguments)
      (path (make-string->arguments 'path))
      (name (make-string->arguments 'name))))

  (export-rpc-call
    session-set
    (alt-speed-down               #f      (make-number->arguments 'alt-speed-down))
    (alt-speed-enabled            nothing (make-bool->arguments   'alt-speed-enabled))
    (alt-speed-time-begin         #f      (make-number->arguments 'alt-speed-time-begin))
    (alt-speed-time-day           #f      (make-number->arguments 'alt-speed-time-day))
    (alt-speed-time-enabled       nothing (make-bool->arguments   'alt-speed-time-enabled))
    (alt-speed-time-end           #f      (make-number->arguments 'alt-speed-time-end))
    (alt-speed-up                 #f      (make-number->arguments 'alt-speed-up))
    (blocklist-enabled            nothing (make-bool->arguments   'blocklist-enabled))
    (blocklist-url                #f      (make-string->arguments 'blocklist-url))
    (cache-size-mb                #f      (make-number->arguments 'cache-size-mb))
    (dht-enabled                  nothing (make-bool->arguments   'dht-enabled))
    (download-dir                 #f      (make-string->arguments 'download-dir))
    (download-queue-enabled       nothing (make-bool->arguments   'download-queue-enabled))
    (download-queue-size          #f      (make-number->arguments 'download-queue-size))
    (encryption                   #f      (make-string->arguments 'encryption))
    (idle-seeding-limit           #f      (make-number->arguments 'idle-seeding-limit))
    (idle-seeding-limit-enabled   nothing (make-bool->arguments   'idle-seeding-limit-enabled))
    (incomplete-dir               #f      (make-string->arguments 'incomplete-dir))
    (incomplete-dir-enabled       nothing (make-bool->arguments   'incomplete-dir-enabled))
    (lpd-enabled                  nothing (make-bool->arguments   'lpd-enabled))
    (peer-limit-global            #f      (make-number->arguments 'peer-limit-global))
    (peer-limit-per-torrent       #f      (make-number->arguments 'peer-limit-per-torrent))
    (peer-port                    #f      (make-number->arguments 'peer-port))
    (peer-port-random-on-start    nothing (make-bool->arguments   'peer-port-random-on-start))
    (pex-enabled                  nothing (make-bool->arguments   'pex-enabled))
    (port-forwarding-enabled      nothing (make-bool->arguments   'port-forwarding-enabled))
    (queue-stalled-enabled        nothing (make-bool->arguments   'queue-stalled-enabled))
    (queue-stalled-minutes        #f      (make-number->arguments 'queue-stalled-minutes))
    (rename-partial-files         nothing (make-bool->arguments   'rename-partial-files))
    (script-torrent-done-enabled  nothing (make-bool->arguments   'script-torrent-done-enabled))
    (script-torrent-done-filename #f      (make-string->arguments 'script-torrent-done-filename))
    (seed-queue-enabled           nothing (make-bool->arguments   'seed-queue-enabled))
    (seed-queue-size              #f      (make-number->arguments 'seed-queue-size))
    (seed-ratio-limit             #f      (make-number->arguments 'seedRatioLimit))
    (seed-ratio-limited           nothing (make-bool->arguments   'seedRatioLimited))
    (speed-limit-down             #f      (make-number->arguments 'speed-limit-down))
    (speed-limit-down-enabled     nothing (make-bool->arguments   'speed-limit-down-enabled))
    (speed-limit-up               #f      (make-number->arguments 'speed-limit-up))
    (speed-limit-up-enabled       nothing (make-bool->arguments   'speed-limit-up-enabled))
    (start-added-torrents         nothing (make-bool->arguments   'start-added-torrents))
    (trash-original-torrent-files nothing (make-bool->arguments   'trash-original-torrent-files))
    (units                        #f      (make-*->arguments      'units pre-proc-object))
    (utp-enabled                  nothing (make-bool->arguments   'utp-enabled)))

  (export-rpc-call session-get (fields #f fields->arguments))

  (export-rpc-call session-stats)
  (export-rpc-call blocklist-update)
  (export-rpc-call port-test)
  (export-rpc-call session-close)

  (export-3.1/4.6 queue-move-top)
  (export-3.1/4.6 queue-move-up)
  (export-3.1/4.6 queue-move-down)
  (export-3.1/4.6 queue-move-bottom)

  (export-rpc-call (free-space (path (make-string->arguments 'path))))
  )
