;;; The lager msg format, or what in other frameworks is sometimes called the
;;; log record:
(defrecord lager-msg
  destinations
  metadata
  severity
  datetime
  timestamp
  message)

(defun loaded-logjam ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
