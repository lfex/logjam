(defmodule logjam-formatter
  (export all))

(defun no-color (x)
  x)

(defun format (log-record format-config)
  (format log-record format-config '()))

(defun format (log-record format-config colors)
  (lists:map
    (lambda (config-key)
      (logjam-backend:output config-key log-record))
    format-config))

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
        (logjam-cfg:color 'modfunc (++ mod ":" func-arity " ")))))
  ((mod func-arity #(colored false))
    (++ mod ":" func-arity " "))
  ((mod func arity)
    (format-func mod (++ func "/" arity) (logjam-cfg:colored-opt))))

(defun format-func
  ((mod func-arity) (when (is_atom mod))
    (format-func (atom_to_list mod) func-arity (logjam-cfg:colored-opt))))

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
  (format-msg msg (logjam-cfg:colored-opt)))

(defun format-msg
  ((msg #(colored true))
    (binary_to_list
      (erlang:iolist_to_binary
        (logjam-cfg:color 'message msg))))
  ((msg #(colored false))
    msg))
