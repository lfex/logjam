(defmodule logjam
  (export all))

(include-lib "logjam/include/logjam.lfe")

(defun setup ()
  (lcfg-log:setup))

(defun start ()
  (setup))

(defun log (msg)
  (lager:log 'info '() msg))

(defun log (level msg)
  (lager:log level '() msg))

(defun log-format (level format args)
  (log level (io_lib:format format args)))

(defun log-mod-func (level mod func msg)
  (case (lcfg:get-in (lcfg-log:get-logging-config) '(colored))
    ('true
      (log-format
        level
        (binary_to_list
          (erlang:iolist_to_binary
            (list (logjam-formatter:color 'modfunc "~s:~s")
                  (logjam-formatter:color 'message " ~s"))))
        `(,mod ,func ,msg)))
    (_
      (log-format level "~s:~s ~s" `(,mod ,func ,msg)))))

(defun log-mod-func-format (level mod func format args)
  (case (lcfg:get-in (lcfg-log:get-logging-config) '(colored))
    ('true
      (log-format
        level
        (binary_to_list
          (erlang:iolist_to_binary
            (list (logjam-formatter:color 'modfunc "~s:~s")
                  (logjam-formatter:color 'message (++ " " format)))))
        (++ `(,mod ,func) args)))
    (_
      (log-format level (++ "~s:~s " format) (++ `(,mod ,func) args)))))
(defun set-level (log-level)
  (set-level 'lager_console_backend log-level))

(defun set-level (lager-backend log-level)
  (lager:set_loglevel lager-backend log-level))

(defun set-level (lager-backend file-name log-level)
  (lager:set_loglevel lager-backend file-name log-level))

(defun check ()
  (info (MODULE) 'check/0 "Checking all log levels ...")
  (lists:foreach
    (lambda (x)
      (call 'logjam x (MODULE) 'check/0 (++ "Testing log output of "
                                     (atom_to_list x)
                                     " with args: ~s, ~s, and ~s ...")
              '(apple banana cranberry)))
    '(debug info notice warning error critical alert emergency))
  (info (MODULE) 'check/0 "Check complete.")
  (timer:sleep 500)
  'ok)
