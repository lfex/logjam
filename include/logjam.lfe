(defun log (msg)
  (lager:log 'info '() msg))

(defun log (level msg)
  (lager:log level '() msg))

(defun log-format (level format args)
  (log level (io_lib:format format args)))

(defun log-mod-func (level mod func msg)
  (log-format level "[~s:~s] ~s" `(,mod ,func ,msg)))

(defun log-mod-func-format (level mod func format args)
  (log-format level (++ "[~s:~s] " format) (++ `(,mod ,func) args)))

;;; Straight-up log function wrappers
(defun debug (msg)
  (log 'debug msg))

(defun info (msg)
  (log 'info msg))

(defun notice (msg)
  (log 'notice msg))

(defun warning (msg)
  (log 'warning msg))

(defun error (msg)
  (log 'error msg))

(defun critical (msg)
  (log 'critical msg))

(defun alert (msg)
  (log 'alert msg))

(defun emergency (msg)
  (log 'emergency msg))

;;; Log function wrappers with args for (io_lib:format ...)
(defun debug (format args)
  (log-format 'debug format args))

(defun info (format args)
  (log-format 'info format args))

(defun notice (format args)
  (log-format 'notice format args))

(defun warning (format args)
  (log-format 'warning format args))

(defun error (format args)
  (log-format 'error format args))

(defun critical (format args)
  (log-format 'critical format args))

(defun alert (format args)
  (log-format 'alert format args))

(defun emergency (format args)
  (log-format 'emergency format args))

;;; Log functions with mod/func and message arguments
(defun debug (mod func msg)
  (log-mod-func 'debug mod func msg))

(defun info (mod func msg)
  (log-mod-func 'info mod func msg))

(defun notice (mod func msg)
  (log-mod-func 'notice mod func msg))

(defun warning (mod func msg)
  (log-mod-func 'warning mod func msg))

(defun error (mod func msg)
  (log-mod-func 'error mod func msg))

(defun critical (mod func msg)
  (log-mod-func 'critical mod func msg))

(defun alert (mod func msg)
  (log-mod-func 'alert mod func msg))

(defun emergency (mod func msg)
  (log-mod-func 'emergency mod func msg))

;;; Log functions with mod/func and format arguments for (io_lib:format ...)
(defun debug (mod func format args)
  (log-mod-func-format 'debug mod func format args))

(defun info (mod func format args)
  (log-mod-func-format 'info mod func format args))

(defun notice (mod func format args)
  (log-mod-func-format 'notice mod func format args))

(defun warning (mod func format args)
  (log-mod-func-format 'warning mod func format args))

(defun error (mod func format args)
  (log-mod-func-format 'error mod func format args))

(defun critical (mod func format args)
  (log-mod-func-format 'critical mod func format args))

(defun alert (mod func format args)
  (log-mod-func-format 'alert mod func format args))

(defun emergency (mod func format args)
  (log-mod-func-format 'emergency mod func format args))

(defun loaded-logjam ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
