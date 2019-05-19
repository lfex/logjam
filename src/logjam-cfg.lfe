(defmodule logjam-cfg
  (export all))

(defun setup ()
  (setup (get-logging-config)))

(defun setup (config)
  (case (lcfg:get-in config '(backend))
    ('lager (setup-lager config))))

(defun config ()
  (let ((local (local)))
    (if (and (=/= local '()) (=/= local 'undefined))
        local
        (global))))

(defun local ()
  (get-logging (lcfg-file:parse-local)))

(defun global ()
  (get-logging (lcfg-file:parse-global)))

(defun get-logging
  (('())
    '())
  ((config)
    (lcfg:get-in config '(logging))))

(defun setup-lager (config)
  (application:load 'lager)
  (application:set_env
    'lager
    'handlers
    (lcfg:get-in config '(options))))

;;; Deprecated functions

(defun get-logging-config ()
  "DEPRECATED: Use `config` instead."
  (config))

(defun get-local-logging ()
  "DEPRECATED: Use `local` instead."
  (local))

(defun get-global-logging ()
  "DEPRECATED: Use `global` instead."
  (global))