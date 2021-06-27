(defmodule logjam-demo
  (export
   (run 0)))

(include-lib "logjam/include/logjam.hrl")

(defun run ()
  (io:format "~n*** Using logjam's macros from LFE ***~n~n")
  (log-debug "This is a debug-level message")
  (log-info "This is an info-level message")
  (log-notice "This is a notice-level message")
  (log-warn "This is a warning-level message")
  (log-error "This is an error-level message")
  (log-crit "This is a critical-level message")
  (log-alert "This is an alert-level message")
  (log-emergency "This is an emergency-level message")
  (log-info #m(some "structured" logging "examples" might "be useful too")))
