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
    (make-printable (get-master-pid msg)))
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

(defun get-master-pid (msg)
  (case (get-metadata 'pid (lager_msg:metadata msg))
    ('undefined (group_leader))
    (pid pid)))

(defun format-func
  ((mod func arity) (when (is_integer arity))
    (format-func mod func (integer_to_list arity)))
  ((mod func arity) (when (is_atom func))
    (format-func mod (atom_to_list func) arity))
  ((mod func arity) (when (is_atom mod))
    (format-func (atom_to_list mod) func arity))
  ((mod func-arity #(colored true))
    (binary_to_list
      (erlang:iolist_to_binary
        (color 'modfunc (++ mod ":" func-arity " ")))))
  ((mod func-arity #(colored false))
    (++ mod ":" func-arity " "))
  ((mod func arity)
    (format-func mod (++ func "/" arity) (logjam-util:color-opt))))

(defun format-func
  ((mod func-arity) (when (is_atom mod))
    (format-func (atom_to_list mod) func-arity (logjam-util:color-opt))))

(defun format-func (pid)
  (let ((`#(current_function #(,m ,f ,a)) (process_info pid 'current_function)))
    (logjam:debug "Current pid for (self): ~p" `(,(self)))
    (logjam:debug "Passed pid: ~p" `(,pid))
    (logjam:debug "Function info for pid: ~p" `(,(process_info pid 'current_function)))
    (logjam:debug "Stacktrace info for pid: ~p" `(,(process_info pid 'current_stacktrace)))
    (logjam:debug "Function info for (self): ~p" `(,(process_info (self) 'current_function)))
    (logjam:debug "Stacktrace info for (self): ~p" `(,(process_info (self) 'current_stacktrace)))
    (format-func (atom_to_list m) (atom_to_list f) (integer_to_list a))))
  ; (let ((`#(current_stacktrace ,data) (process_info pid 'current_stacktrace)))
  ;   (io_lib:format "~p" `(,data))))

(defun format-msg (msg)
  (format-msg msg (logjam-util:color-opt)))

(defun format-msg
  ((msg #(colored true))
    (binary_to_list
      (erlang:iolist_to_binary
        (color 'message msg))))
  ((msg #(colored false))
    msg))
