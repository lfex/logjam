(defmodule logjamapp
  (behaviour application)
  (export (start 0) (start 2) 
          (stop 0) (stop 1)))

;;; Application implementation

(defun start ()
  (start 'normal '(load)))

;; This is the function indicated in the .app.src; since we put our config
;; data in lfe.config and not in .app.src, we need to pull the config file
;; data in here. After the point, the only time that the file system config
;; data should be referenced is when a reload is occurring.
(defun start 
  ((start-type '())
    (start start-type 'load))
  ((start-type 'load)
    (logjam:debug "Got arg 'load; reading config ...")
    (start start-type (logjam-cfg:read-config)))
  ((_start-type config-data)
    (logjam-backend:setup config-data)
    (application:ensure_all_started 'lager)
    (logjam:debug "Starting logjam application ...")
    ;; (logjam:debug "Using config data: ~p" (list config-data))
    (let ((sup (logjammgr:start_link config-data)))
      (logjam:debug 'logjamapp 'start/2 "Supervisor start status: ~p" (list sup))
      sup)))

(defun stop ()
  (stop 'undefined))

(defun stop (_state)
  'ok)
