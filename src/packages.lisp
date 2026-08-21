(defpackage #:alt
  (:use #:cl)
  (:export
   #:main
   #:*app*))

(defpackage #:alt.cli
  (:use #:cl)
  (:export
   #:parse-args
   #:print-app-help
   #:print-app-version))

(defpackage #:alt.utils
  (:use #:cl)
  (:export
   #:random-base62
   #:valid-id-p
   #:human-size
   #:human-age
   #:universal-to-iso8601
   #:safe-filename
   #:constant-time-string=))

(defpackage #:alt.mime
  (:use #:cl)
  (:export
   #:sniff
   #:type-content-type
   #:type-extension
   #:type-label
   #:type-inline-p
   #:accepted-types
   #:*text-type*))

(defpackage #:alt.store
  (:use #:cl)
  (:export
   #:open-store
   #:store-directory
   #:blob-path
   #:put-blob
   #:get-meta
   #:list-blobs
   #:delete-blob
   #:delete-blobs
   #:store-totals
   #:count-blobs))

(defpackage #:alt.web
  (:use #:cl)
  (:export
   #:start-server
   #:stop-server))
