(defmodule logjamapp
  (behaviour application)
  (export (start 0) (start 2) 
          (stop 0) (stop 1)))

;;; Application implementation

(defun start ()
  (start 'normal '()))

(defun start (_start-type start-args)
  (logjam-cfg:setup)
  (application:ensure_all_started 'lager)
  (logjam:debug "Starging logjam application ...")
  (let ((sup (logjammgr:start_link start-args)))
    (logjam:debug "Supervisor start status: ~p" (list sup))
    sup))

(defun stop ()
  (stop 'undefined))

(defun stop (_state)
  'ok)
