(defun log (level msg)
  (lager:log level '() msg))

(defun log-format (level format args)
  (log level (io_lib:format format args)))

(defun log-mod-func (level mod func format args)
  (log level (++ "[~s:~s] " format) (++ `(,mod ,func) args)))

;;; Straight-up log function wrappers
(defun debug (args)
  (log 'debug args))

(defun info (args)
  (log 'info args))

(defun notice (args)
  (log 'notice args))

(defun warning (args)
  (log 'warning args))

(defun error (args)
  (log 'error args))

(defun critical (args)
  (log 'critical args))

(defun alert (args)
  (log 'alert args))

(defun emergency (args)
  (log 'emergency args))

;;; Log function wrappers with args for (io:format ...)
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

;;; Log functions with mod/func arguments
(defun debug (mod func format args)
  (log-mod-func 'debug mod func format args))

(defun info (mod func format args)
  (log-mod-func 'info mod func format args))

(defun notice (mod func format args)
  (log-mod-func 'notice mod func format args))

(defun warning (mod func format args)
  (log-mod-func 'warning mod func format args))

(defun error (mod func format args)
  (log-mod-func 'error mod func format args))

(defun critical (mod func format args)
  (log-mod-func 'critical mod func format args))

(defun alert (mod func format args)
  (log-mod-func 'alert mod func format args))

(defun emergency (mod func format args)
  (log-mod-func 'emergency mod func format args))
