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
;;;
;;; Note that `setup` is intended to be called before any logging backend
;;; is started, and thus before logjam itself is started. As such, for this
;;; call, the config values can't be read from the server, but instead need
;;; to be read from the file.
(defun setup ()
  (logjam-backend:setup (read-config)))

;;; Post-startup Config API
;;;
;;; logjamd alias functions for config stored in memory; note that each 
;;; function in this API depends upon the logjamd gen_serve to be running.

(defun reload ()
  (logjamd:reload-config))

(defun reload (key)
  (logjamd:reload-config key))

(defun set (config-data)
  (logjamd:set-config config-data))

(defun set (key value)
  (logjamd:set-config key value))

(defun get ()
  (try
    (logjamd:get-config)
    (catch 
      (`#(,_ #(noproc ,_) ,_)
        (read-config)))))

(defun get (keys)
  (try
    (logjamd:get-config keys)
    (catch 
      (`#(,_ #(noproc ,_) ,_)
        (read-config)))))

(defun colored-opt ()
  (proplists:lookup 'colored (get)))

(defun color? ()
  (get '(colored)))

(defun get-color (color-key)
  (get `(colors ,color-key)))

(defun color (color-key str)
  (erlang:apply 'erlang
                'apply
                (++ (get-color color-key) `((,str)))))

(defun lager-handlers ()
  (get '(backend lager handlers)))

(defun lager-handlers (config)
  (logjam-util:get-in config '(backend lager handlers)))

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
