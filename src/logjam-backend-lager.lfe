(defmodule logjam-backend-lager
  (export all))

(include-lib "logjam/include/lager.lfe")

(defun setup (options)
  (io:format "Running setup ...~n")
  (io:format "Lager options:~n~p~n" (list options))
  (application:load 'lager)
  (application:set_env
    'lager
    'handlers
    options))

(defun handlers ()
  (application:get_env 'lager 'handlers))

(defun log (msg)
  (lager:log 'info '() msg))

(defun log (level msg)
  (lager:log level '() msg))

(defun get-level (lager-backend)
  (logjam-cfg:get `(options ,lager-backend level)))

(defun set-level (log-level)
  (set-level 'lager_console_backend log-level))

(defun set-level (lager-backend log-level)
  (lager:set_loglevel lager-backend log-level))

(defun set-level (lager-backend file-name log-level)
  (lager:set_loglevel lager-backend file-name log-level))

(defun output
  (('message msg)
    (logjam-util:make-printable (lager_msg:message msg)))
  (('datetime msg)
    (logjam-util:make-printable (lager_msg:datetime msg)))
  (('severity msg)
    (let ((sev (lager_msg:severity msg)))
      (logjam-cfg:color sev (logjam-util:make-printable sev))))
  (('date msg)
    (logjam-cfg:color 'date (logjam-util:make-printable (element 1 (lager_msg:datetime msg)))))
  (('time msg)
    (logjam-cfg:color 'time (logjam-util:make-printable (element 2 (lager_msg:datetime msg)))))
  (('timestamp msg)
    (logjam-util:make-printable (lager_msg:timestamp msg)))
  (('pid msg)
    (logjam-util:make-printable (get-master-pid msg)))
  (('metadata msg)
    (logjam-util:make-printable (lager_msg:metadata msg)))
  (('destinations msg)
    (logjam-util:make-printable (lager_msg:destinations msg)))
  (('sev msg)
    (logjam-util:make-printable (lager_util:level_to_chr
      (lager_msg:severity_as_int msg))))
  ((other _)
    (logjam-util:make-printable other)))

(defun get-metadata (key metadata)
  (get-metadata key metadata 'undefined))

(defun get-metadata (key metadata default)
  (case (lists:keyfind key 1 metadata)
    ('false default)
    (`#(,key ,val) val)))

(defun get-master-pid (msg)
  (case (get-metadata 'pid (lager_msg:metadata msg))
    ('undefined (group_leader))
    (pid pid)))
