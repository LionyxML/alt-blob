(in-package #:alt)

(defparameter *app*
  (list :name        "AltBlob"
        :bin-name    "alt"
        :version     (asdf:component-version (asdf:find-system :alt))
        :description "The alternative blob sharing. Anonymous paste and file host, with no database and no JavaScript."
        :repository  "https://github.com/LionyxML/alt-blob"))

(defun option-value (key options)
  (cdr (assoc key options)))

(defun env-truthy-p (name)
  "True when environment variable NAME is set to a truthy string."
  (let ((v (uiop:getenv name)))
    (and v (member (string-downcase v)
                   '("1" "true" "yes" "on")
                   :test #'string=))))

(defun env-falsy-p (name)
  "True when environment variable NAME is set to a falsy string.
Used for opt-out of behaviour that is on by default."
  (let ((v (uiop:getenv name)))
    (and v (member (string-downcase v)
                   '("0" "false" "no" "off")
                   :test #'string=))))

(defun parse-size (value default)
  "Byte count from VALUE, accepting a plain number or a K/M suffix (5M)."
  (cond
    ((integerp value) value)
    ((not (stringp value)) default)
    (t (let* ((trimmed (string-trim " " value))
              (last-char (and (plusp (length trimmed))
                              (char-upcase (char trimmed (1- (length trimmed))))))
              (multiplier (case last-char
                            (#\K 1024)
                            (#\M (* 1024 1024))
                            (t 1)))
              (digits (if (= multiplier 1) trimmed (subseq trimmed 0 (1- (length trimmed)))))
              (n (parse-integer digits :junk-allowed t)))
         (if n (* n multiplier) default)))))

(defun print-banner (port data-dir base-url blobs max-blob admin-open)
  (format t "
>>>      _    _   _____
>>>     / \\  | | |_   _|
>>>    / _ \\ | |   | |
>>>   / ___ \\| |___| |
>>>  /_/   \\_\\_____|_|
>>>
>>>  Wake up, samurai. We have blobs to paste.
>>>
>>> Port     : ~A
>>> Data     : ~A
>>> Domain   : ~A
>>> Stored   : ~A blob(s)
>>> Max blob : ~A
>>> Admin    : ~A

" port data-dir base-url blobs (alt.utils:human-size max-blob)
    (if admin-open
        "enabled"
        "CLOSED (set ADMIN_PASSWORD to allow uploads)")))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (multiple-value-bind (status data)
        (alt.cli:parse-args args)

      (case status
        (:version
         (alt.cli:print-app-version)
         (finish-output)
         (sb-ext:exit :code 0))

        (:help
         (alt.cli:print-app-help)
         (finish-output)
         (sb-ext:exit :code 0))

        (:error
         (format *error-output*
                 "~%Error: ~A~2%Use --help for usage information.~%"
                 data)
         (finish-output *error-output*)
         (sb-ext:exit :code 1))

        (:ok
         (handler-case
             (let* ((port (or (option-value :port data)
                              (uiop:getenv "PORT")
                              3900))
                    (data-dir (or (option-value :data-dir data)
                                  (uiop:getenv "DATA_DIR")
                                  "data"))
                    (base-url (or (option-value :base-url data)
                                  (uiop:getenv "BASE_URL")
                                  "http://localhost:3900/"))
                    (admin-user (or (uiop:getenv "ADMIN_USER") "admin"))
                    (admin-password (uiop:getenv "ADMIN_PASSWORD"))
                    (max-blob (parse-size (or (option-value :max-blob data)
                                              (uiop:getenv "MAX_BLOB"))
                                          (* 5 1024 1024)))
                    (rate-limit-enabled (not (or (option-value :no-rate-limit data)
                                                 (env-falsy-p "RATE_LIMIT"))))
                    (rate-limit-max (or (option-value :rate-limit-max data)
                                        (uiop:getenv "RATE_LIMIT_MAX")
                                        60))
                    (rate-limit-window (or (option-value :rate-limit-window data)
                                           (uiop:getenv "RATE_LIMIT_WINDOW")
                                           60))
                    (trust-proxy (or (option-value :trust-proxy data)
                                     (env-truthy-p "TRUST_PROXY")))
                    (max-threads (or (uiop:getenv "MAX_THREADS") 100))
                    (accept-backlog (or (uiop:getenv "ACCEPT_BACKLOG") 200))
                    (list-page-size (or (option-value :list-page-size data)
                                        (uiop:getenv "LIST_PAGE_SIZE")
                                        50))
                    (blobs (alt.store:open-store data-dir)))

               (print-banner port
                             (alt.store:store-directory)
                             base-url
                             blobs
                             max-blob
                             (and (stringp admin-password)
                                  (plusp (length admin-password))))

               (alt.web:start-server
                :port port
                :base-url base-url
                :admin-user admin-user
                :admin-password admin-password
                :max-blob max-blob
                :rate-limit-enabled rate-limit-enabled
                :rate-limit-max rate-limit-max
                :rate-limit-window rate-limit-window
                :trust-proxy trust-proxy
                :max-threads max-threads
                :accept-backlog accept-backlog
                :list-page-size list-page-size)

               ;; Block main thread until signal received
               (let ((shutdown (sb-thread:make-semaphore :name "shutdown")))
                 (sb-sys:enable-interrupt sb-unix:sigterm
                                          (lambda (sig info ctx)
                                            (declare (ignore sig info ctx))
                                            (sb-thread:signal-semaphore shutdown)))
                 (sb-sys:enable-interrupt sb-unix:sigint
                                          (lambda (sig info ctx)
                                            (declare (ignore sig info ctx))
                                            (sb-thread:signal-semaphore shutdown)))
                 (sb-thread:wait-on-semaphore shutdown))

               ;; Ordered shutdown
               (alt.web:stop-server)
               (sb-ext:exit :code 0))

           (error (e)
             (format *error-output*
                     "~%Runtime error: ~A~%" e)
             (finish-output *error-output*)
             (sb-ext:exit :code 1))))))))
