(defmodule logjam-demo
  (export (run 0)))

(include-lib "kernel/include/logger.hrl")

(defun run ()
  (logjam:debug "This is a debug-level message")
  (logger:info "This is an info-level message")
  (logger:notice "This is a notice-level message")
  (logger:warning "This is a warning-level message")
  (logger:error "This is an error-level message")
  (logger:critical "This is a critical-level message")
  (logger:alert "This is an alert-level message")
  (logger:emergency "This is an emergency-level message")
  (logger:info #m(some "structured" logging "examples" might "be useful too")))
