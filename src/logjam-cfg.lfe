(defmodule logjam-cfg
  (export all))

(defun setup ()
  (setup (get-logging-config)))

(defun setup (config)
  (case (lcfg:get-in config '(backend))
    ('lager (setup-lager config))))

(defun get-logging-config ()
  (let ((local (get-local-logging)))
    (if (and (=/= local '()) (=/= local 'undefined))
        local
        (get-global-logging))))

(defun get-local-logging ()
  (get-logging (lcfg-file:parse-local)))

(defun get-global-logging ()
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
