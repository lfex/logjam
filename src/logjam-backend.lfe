(defmodule logjam-backend
  (export all))

(defun options (config)
  (proplists:get_value 'options config))

(defun detect ()
  (if (> (logjam-util:erl-version) 20) 'logger
      'lager))

(defun output (config-key log-record)
  (case (detect)
    ('logger 'tbd)
    ('lager (logjam-backend-lager:output config-key log-record))))

;; Note that `setup` is intended to be called before any logging backend
;; is started, and thus before logjam itself is started. As such, for this
;; call, the config values can't be read from the server, but instead need
;; to be read from the file.
(defun setup (config)
  (case (detect)
    ('lager (logjam-backend-lager:setup (options config)))
    ('logger (logjam-backend-logger:setup config))))

(defun log (msg)
  (case (detect)
    ('lager (logjam-backend-lager:log msg))
    ('logger (logjam-backend-logger:log msg))))

(defun log (level msg)
  (case (detect)
    ('lager (logjam-backend-lager:log level msg))
    ('logger (logjam-backend-logger:log level msg))))
