(defmodule logjam
  (export all))

(include-lib "logjam/include/logjam.lfe")

(defun setup ()
  (lcfg-log:setup))

(defun set-level (log-level)
  (set-level 'lager_console_backend log-level))

(defun set-level (lager-backend log-level)
  (lager:set_loglevel lager-backend log-level))

(defun set-level (lager-backend file-name log-level)
  (lager:set_loglevel lager-backend file-name log-level))
