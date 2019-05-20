(defmodule logjam-cfg
  (export all))

;;; File-based access to config data

(defun read-config ()
  (let ((local (read-local)))
    (if (and (=/= local '()) (=/= local 'undefined))
        local
        (read-global))))

(defun read-local ()
  (proplists:get_value 'logging (lcfg-file:parse-local)))

(defun read-global ()
  (proplists:get_value 'logging (lcfg-file:parse-global)))

;;; Support for in-memory config data

(defun ets-table () 'logjam)
(defun cfg-key () 'config)

(defun create-config-table
  ()
  (ets:new (ets-table) `(bag named_table)))

(defun insert-config
  ()
  (ets:insert (ets-table) `#(,(cfg-key) ,(read-config))))

(defun lookup-config
  ()
  (proplists:get_value 
    (cfg-key) 
    (ets:lookup (ets-table) (cfg-key))))

;;; Config API

(defun setup ()
  (insert-config)
  (setup (lookup-config)))

(defun setup (config)
  (case (proplists:get_value (backend config) config)
    ('lager (setup-lager config))
    ('logger (setup-logger config))))

(defun backend
  ()
  (backend (lookup-config)))

(defun backend
  (config)
  (proplists:get_value 'backend config))

(defun options
  ()
  (options (lookup-config)))

(defun options
  (config)
  (proplists:get_value 'options config))

;;; Lager backend functions

(defun setup-lager (config)
  (application:load 'lager)
  (application:set_env
    'lager
    'handlers
    (options)))

;;; Logger backend functions

;; TBD
(defun setup-logger
  (config)
  )

;;; Deprecated functions

(defun get-logging-config ()
  "DEPRECATED: Use `config` instead."
  (read-config))

(defun get-local-logging ()
  "DEPRECATED: Use `local` instead."
  (read-local))

(defun get-global-logging ()
  "DEPRECATED: Use `global` instead."
  (read-global))
