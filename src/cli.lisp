(in-package #:alt.cli)

(defparameter *options*
  '(("-p"                  . :port)
    ("--port"              . :port)
    ("-b"                  . :base-url)
    ("--base"              . :base-url)
    ("-d"                  . :data-dir)
    ("--data-dir"          . :data-dir)
    ("--max-blob"          . :max-blob)
    ("--no-rate-limit"     . :no-rate-limit)
    ("--rate-limit-max"    . :rate-limit-max)
    ("--rate-limit-window" . :rate-limit-window)
    ("--trust-proxy"       . :trust-proxy)
    ("--list-page-size"    . :list-page-size)
    ("-h"                  . :help)
    ("--help"              . :help)
    ("-v"                  . :version)
    ("--version"           . :version)))

(defun app-prop (key)
  (getf alt:*app* key))

(defun option-keyword (arg)
  (cdr (assoc arg *options* :test #'string=)))

(defun print-app-version ()
  (write-line (app-prop :version)))

(defun print-app-help ()
  (format t
          "~2%
~A [version: ~A]

~A

Usage:
  ~A [options]

Examples:
  ~A --base https://alt.example.org --port 3900 --data-dir ./data

  BASE_URL=https://alt.example.org PORT=3900 DATA_DIR=./data ~A

Accepted uploads: plain text (UTF-8), png, jpeg, gif, webp, pdf. The type is
decided by looking at the bytes, so renaming a file changes nothing.

Options:
  -b, --base                Set app base URL
  -d, --data-dir            Directory the blobs live in (default: ./data)
  -h, --help                Show help
  -p, --port                Set web server port
      --max-blob BYTES      Max upload size (default: 5242880). Accepts K/M
                            suffixes, e.g. --max-blob 5M.
      --no-rate-limit       Disable per-IP rate limiting of reads (on by default).
      --rate-limit-max N    Max reads per IP per window (default: 60).
      --rate-limit-window S Rate-limit window in seconds (default: 60).
      --trust-proxy         Read client IP from X-Forwarded-For. Enable ONLY
                            behind a trusted reverse proxy (e.g. Caddy/nginx).
      --list-page-size N    Blobs shown per page on /admin (default: 50).
  -v, --version             Show version

Environment (env-only):
  ADMIN_USER        Username for the admin routes (default: admin)
  ADMIN_PASSWORD    Password for /new and /admin. If unset, both stay closed
                    (401), which means nothing can be uploaded at all.
                    Basic Auth is unencrypted; serve it only behind HTTPS.
  MAX_THREADS       Worker threads (default: 100).
  ACCEPT_BACKLOG    Extra queued connections (default: 200).

Environment (switch alternatives):
  PORT, BASE_URL, DATA_DIR, MAX_BLOB, LIST_PAGE_SIZE
                    Same as the matching options.
  RATE_LIMIT        Set to 0/false/no/off to disable rate limiting (default: on).
  RATE_LIMIT_MAX    Same as --rate-limit-max.
  RATE_LIMIT_WINDOW Same as --rate-limit-window.
  TRUST_PROXY       Same as --trust-proxy when set to 1/true/yes/on.

"
          (app-prop :name)
          (app-prop :version)
          (app-prop :description)
          (app-prop :bin-name)
          (app-prop :bin-name)
          (app-prop :bin-name)))

(defun parse-args (args)
  (let ((result '()))
    (loop while args
          for arg = (pop args)
          for option = (option-keyword arg)
          do (case option
               (:help
                (return-from parse-args
                  (values :help nil)))

               (:version
                (return-from parse-args
                  (values :version nil)))

               ((:no-rate-limit :trust-proxy)
                (push (cons option t) result))

               ((:port :base-url :data-dir :max-blob
                       :rate-limit-max :rate-limit-window :list-page-size)
                (if args
                    (push (cons option (pop args)) result)
                    (return-from parse-args
                      (values :error
                              (format nil "Missing value for ~A" arg)))))

               (otherwise
                (return-from parse-args
                  (values :error
                          (format nil "Unknown option: ~A" arg))))))

    (values :ok (nreverse result))))
