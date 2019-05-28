(defmodule logjam-util
  (export all))

(defun version ()
  (lutil:get-app-version 'logjam))

(defun versions ()
  (++ (lutil:get-versions)
      `(#(logjam ,(version)))))

(defun check ()
  (let ((caller (logjam:caller)))
    (logjam:info `#(c ,caller) "Checking all log levels ...")
    (lists:foreach
      (lambda (x)
        (call 'logjam x `#(c ,caller)  (++ "Testing log output of "
                                           (atom_to_list x)
                                           " with args: ~s, ~s, and ~s ...")
                '(apple banana cranberry)))
      '(debug info notice warning error critical alert emergency))
    (logjam:info `#(c ,caller) "Check complete.")
    (timer:sleep 500)
    'ok))

(defun check (level)
  (let ((caller (logjam:caller)))
    (logjam:info `#(c ,caller) "Checking log-level ~p ..." `(,level))
    (call 'logjam level `#(c ,caller) (++ "Testing log output of "
                                          (atom_to_list level)
                                          " ... "))
    (logjam:info `#(c ,caller) "Checked log-level ~p." `(,level))))

(defun color-opt ()
  (lists:keyfind 'colored 1 (logjam-cfg:get-logging-config)))

(defun erl-version ()
  (list_to_integer (erlang:system_info 'otp_release)))

(defun backend ()
  (if (> (erl-version) 20) 'logger
      'lager))
