(asdf:defsystem #:alt
    :description "Anonymous, database-free paste and blob host."
    :author "Rahul Martim Juliato"
    :license "MIT"
    :version "0.1.1"
    :depends-on (#:hunchentoot #:easy-routes #:djula #:quri)
    :entry-point "alt:main"
    :components
    ((:module "src"
              :components
              ((:file "packages")
               (:file "utils"     :depends-on ("packages"))
               (:file "mime"      :depends-on ("packages"))
               (:file "store"     :depends-on ("packages" "utils" "mime"))
               (:file "cli"       :depends-on ("packages"))
               (:file "webserver" :depends-on ("packages" "utils" "mime" "store"))
               (:file "main"      :depends-on ("packages" "utils" "cli" "store" "webserver"))))))
