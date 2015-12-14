(defmodule logjam-formatter
  (export all))

(include-lib "logjam/include/logjam.lfe")

(defun no-color (x)
  x)

(defun get-color (cfg-key)
  (lcfg:get-in
    (lcfg-log:get-logging-config)
    `(colors ,cfg-key)))

; (defun color (cfg-key str)
;   (funcall (get-color cfg-key) str))

(defun color (cfg-key str)
  (erlang:apply 'erlang 'apply (++ (get-color cfg-key) `((,str)))))

(defun format (lager-msg format-config)
  (format lager-msg format-config '()))

(defun format (lager-msg format-config colors)
  (lists:map
    (lambda (config-key)
      (output config-key lager-msg))
    format-config))

(defun output
  (('message msg)
    (make-printable (lager_msg:message msg)))
  (('datetime msg)
    (make-printable (lager_msg:datetime msg)))
  (('severity msg)
    (let ((sev (lager_msg:severity msg)))
      (color sev (make-printable sev))))
  (('date msg)
    (color 'date (make-printable (element 1 (lager_msg:datetime msg)))))
  (('time msg)
    (color 'time (make-printable (element 2 (lager_msg:datetime msg)))))
  (('timestamp msg)
    (make-printable (lager_msg:timestamp msg)))
  (('pid msg)
    (make-printable (get-metadata 'pid (lager_msg:metadata msg))))
  (('metadata msg)
    (make-printable (lager_msg:metadata msg)))
  (('destinations msg)
    (make-printable (lager_msg:destinations msg)))
  (('sev msg)
    (make-printable (lager_util:level_to_chr
      (lager_msg:severity_as_int msg))))
  ((other _)
    (make-printable other)))

(defun make-printable
  ((a) (when (is_atom a))
    (atom_to_list a))
  ((p) (when (is_pid p))
    (pid_to_list p))
  ((l) (when (orelse (is_list l) (is_binary l)))
    l)
  ((other)
    (io_lib:format "~p" `(,other))))

(defun get-metadata (key metadata)
  (get-metadata key metadata 'undefined))

(defun get-metadata (key metadata default)
  (case (lists:keyfind key 1 metadata)
    ('false default)
    (`#(,key ,val) val)))
