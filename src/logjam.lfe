(defmodule logjam
  (export (start 0)
          (stop 0))
  (export (caller 0) (caller 1)
          (debug 1) (debug 2) (debug 3) (debug 4)
          (info 1) (info 2) (info 3) (info 4)
          (notice 1) (notice 2) (notice 3) (notice 4)
          (warn 1) (warn 2) (warn 3) (warn 4)
          (warning 1) (warning 2) (warning 3) (warning 4)
          (err 1) (err 2) (err 3) (err 4)
          (error 1) (error 2) (error 3) (error 4)
          (critical 1) (critical 2) (critical 3) (critical 4)
          (alert 1) (alert 2) (alert 3) (alert 4)
          (emergency 1) (emergency 2) (emergency 3) (emergency 4)
          (fail 1) (fail 2) (fail 3) (fail 4)
          (failure 1) (failure 2) (failure 3) (failure 4)))

;;; Logjam control API

(defun start ()
  (application:start 'logjam))

(defun stop ()
  (logjam:info 'logjam
               'stop/0
               "Shutting down logjam application ...")
  (application:stop 'logjam))

(defun log-format
  ((level `#(c ,caller) msg)
    (logjam-backend:log level (++ caller msg)))
  ((level pid msg) (when (is_pid pid))
    (logjam-backend:log level (++ (logjam-formatter:format-func pid) msg)))
  ((level format args)
    (logjam-backend:log level (io_lib:format format args))))

(defun log-mod-func
  ((level `#(c ,caller) format args)
    (log-format level (++ caller format) args))
  ((level pid format args) (when (is_pid pid))
    (log-format level (++ (logjam-formatter:format-func pid) format) args))
  ((level mod func msg)
    (logjam-backend:log level
         (++ (logjam-formatter:format-func mod func)
             (logjam-formatter:format-msg msg)))))

(defun log-mod-func-format (level mod func format args)
  (log-mod-func level mod func (io_lib:format format args)))

;;; Straight-up log function wrappers
(defun debug (msg)
  (logjam-backend:log 'debug msg))

(defun info (msg)
  (logjam-backend:log 'info msg))

(defun notice (msg)
  (logjam-backend:log 'notice msg))

(defun warning (msg)
  (logjam-backend:log 'warning msg))

(defun warn (msg)
  (logjam-backend:log 'warning msg))

(defun err (msg)
  (logjam-backend:log 'error msg))

(defun error (msg)
  (logjam-backend:log 'error msg))

(defun critical (msg)
  (logjam-backend:log 'critical msg))

(defun alert (msg)
  (logjam-backend:log 'alert msg))

(defun emergency (msg)
  (logjam-backend:log 'emergency msg))

(defun failure (msg)
  (logjam-backend:log 'emergency msg))

(defun fail (msg)
  (logjam-backend:log 'emergency msg))

;;; Log function wrappers with args for (io_lib:format ...)
(defun debug (format args)
  (log-format 'debug format args))

(defun info (format args)
  (log-format 'info format args))

(defun notice (format args)
  (log-format 'notice format args))

(defun warning (format args)
  (log-format 'warning format args))

(defun warn (format args)
  (log-format 'warning format args))

(defun err (format args)
  (log-format 'error format args))

(defun error (format args)
  (log-format 'error format args))

(defun critical (format args)
  (log-format 'critical format args))

(defun alert (format args)
  (log-format 'alert format args))

(defun emergency (format args)
  (log-format 'emergency format args))

(defun failure (format args)
  (log-format 'emergency format args))

(defun fail (format args)
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

(defun warn (mod func msg)
  (log-mod-func 'warning mod func msg))

(defun err (mod func msg)
  (log-mod-func 'error mod func msg))

(defun error (mod func msg)
  (log-mod-func 'error mod func msg))

(defun critical (mod func msg)
  (log-mod-func 'critical mod func msg))

(defun alert (mod func msg)
  (log-mod-func 'alert mod func msg))

(defun emergency (mod func msg)
  (log-mod-func 'emergency mod func msg))

(defun failure (mod func msg)
  (log-mod-func 'emergency mod func msg))

(defun fail (mod func msg)
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

(defun warn (mod func format args)
  (log-mod-func-format 'warning mod func format args))

(defun err (mod func format args)
  (log-mod-func-format 'error mod func format args))

(defun error (mod func format args)
  (log-mod-func-format 'error mod func format args))

(defun critical (mod func format args)
  (log-mod-func-format 'critical mod func format args))

(defun alert (mod func format args)
  (log-mod-func-format 'alert mod func format args))

(defun emergency (mod func format args)
  (log-mod-func-format 'emergency mod func format args))

(defun failure (mod func format args)
  (log-mod-func-format 'emergency mod func format args))

(defun fail (mod func format args)
  (log-mod-func-format 'emergency mod func format args))

;; Convenience funcs
(defun caller
  "Impute calling function from stack"
  (('raw)
    (let* ((`#(current_stacktrace ,trace) (process_info (self) 'current_stacktrace))
           (`#(,mod ,func ,arity ,_) (lists:nth 3 trace)))
      `(,mod ,func ,arity))))

(defun caller ()
  "Impute calling function from stack"
  (apply 'logjam-formatter 'format-func (caller 'raw)))
