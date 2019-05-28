(defmodule logjam-cfg
  (export all))

;;; File-based access to config data

(defun read-config ()
  (let ((local (read-local)))
    (if (and (=/= local '()) (=/= local 'undefined))
        local
        (read-global))))

(defun read-local ()
  (lists:sort 
    (proplists:get_value 'logging (lcfg-file:parse-local))))

(defun read-global ()
  (lists:sort
    (proplists:get_value 'logging (lcfg-file:parse-global))))

;;; Pre-startup Config API

;; Note that `setup` is intended to be called before even lager is started,
;; so logjam isn't started. As such, for this call, the config values can't 
;; be read from the server, but instead need to be read from the file.
(defun setup ()
  (setup (read-config)))

(defun setup (config)
  (case (logjam-util:backend)
    ('lager (setup-lager config))
    ('logger (setup-logger config))))

(defun options (config)
  (proplists:get_value 'options config))

;;; Lager backend functions

(defun setup-lager (config)
  (application:load 'lager)
  (application:set_env
    'lager
    'handlers
    (options config)))

;;; Logger backend functions

;; TBD
(defun setup-logger (config)
  )

;;; Post-startup Config API

;;; logjamd alias functions for config stored in memory; note that each 
;;;  function in this API depends upon the logjamd gen_serve to be running.

(defun reload ()
  (logjamd:reload-config))

(defun reload (key)
  (logjamd:reload-config key))

(defun set (config-data)
  (logjamd:set-config config-data))

(defun set (key value)
  (logjamd:set-config key value))

(defun get ()
  (logjamd:get-config))

(defun get (keys)
  (logjamd:get-config keys))

;;; Deprecated functions for file-based API

(defun get-logging-config ()
  "DEPRECATED: Use `config` instead."
  (read-config))

(defun get-local-logging ()
  "DEPRECATED: Use `local` instead."
  (read-local))

(defun get-global-logging ()
  "DEPRECATED: Use `global` instead."
  (read-global))
