;;; The lager msg format, or what in other frameworks is sometimes called the
;;; log record:
(defrecord lager-msg
  destinations
  metadata
  severity
  datetime
  timestamp
  message)

;;; Straight-up log function wrappers
(defun debug (msg)
  (logjam:log 'debug msg))

(defun info (msg)
  (logjam:log 'info msg))

(defun notice (msg)
  (logjam:log 'notice msg))

(defun warning (msg)
  (logjam:log 'warning msg))

(defun err (msg)
  (logjam:log 'error msg))

(defun error (msg)
  (logjam:log 'error msg))

(defun critical (msg)
  (logjam:log 'critical msg))

(defun alert (msg)
  (logjam:log 'alert msg))

(defun emergency (msg)
  (logjam:log 'emergency msg))

;;; Log function wrappers with args for (io_lib:format ...)
(defun debug (format args)
  (logjam:log-format 'debug format args))

(defun info (format args)
  (logjam:log-format 'info format args))

(defun notice (format args)
  (logjam:log-format 'notice format args))

(defun warning (format args)
  (logjam:log-format 'warning format args))

(defun err (format args)
  (logjam:log-format 'error format args))

(defun error (format args)
  (logjam:log-format 'error format args))

(defun critical (format args)
  (logjam:log-format 'critical format args))

(defun alert (format args)
  (logjam:log-format 'alert format args))

(defun emergency (format args)
  (logjam:log-format 'emergency format args))

;;; Log functions with mod/func and message arguments
(defun debug (mod func msg)
  (logjam:log-mod-func 'debug mod func msg))

(defun info (mod func msg)
  (logjam:log-mod-func 'info mod func msg))

(defun notice (mod func msg)
  (logjam:log-mod-func 'notice mod func msg))

(defun warning (mod func msg)
  (logjam:log-mod-func 'warning mod func msg))

(defun err (mod func msg)
  (logjam:log-mod-func 'error mod func msg))

(defun error (mod func msg)
  (logjam:log-mod-func 'error mod func msg))

(defun critical (mod func msg)
  (logjam:log-mod-func 'critical mod func msg))

(defun alert (mod func msg)
  (logjam:log-mod-func 'alert mod func msg))

(defun emergency (mod func msg)
  (logjam:log-mod-func 'emergency mod func msg))

;;; Log functions with mod/func and format arguments for (io_lib:format ...)
(defun debug (mod func format args)
  (logjam:log-mod-func-format 'debug mod func format args))

(defun info (mod func format args)
  (logjam:log-mod-func-format 'info mod func format args))

(defun notice (mod func format args)
  (logjam:log-mod-func-format 'notice mod func format args))

(defun warning (mod func format args)
  (logjam:log-mod-func-format 'warning mod func format args))

(defun err (mod func format args)
  (logjam:log-mod-func-format 'error mod func format args))

(defun error (mod func format args)
  (logjam:log-mod-func-format 'error mod func format args))

(defun critical (mod func format args)
  (logjam:log-mod-func-format 'critical mod func format args))

(defun alert (mod func format args)
  (logjam:log-mod-func-format 'alert mod func format args))

(defun emergency (mod func format args)
  (logjam:log-mod-func-format 'emergency mod func format args))

(defun loaded-logjam ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
