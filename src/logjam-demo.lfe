(defmodule logjam-demo
  (export (run 0)))

(defun app-env ()
  '(#(logger (#(handler default
                        logger_std_h
                        #M(formatter #(logjam #M(map_depth 3
                                                term_depth 50
                                                colored true))))))
    #(logger_level debug)))

(defun run ()
  ;; (application:set_env `(#(logjam ,(app-env))))
  ;; (application:ensure_all_started 'sasl)
  ;; (timer:sleep 500)
  ;; (application:set_env 'kernel 'logger_level 'debug)
  ;; (application:set_env 'logjam 'logger_level 'debug)
  ;; (logger:set_application_level 'kernel 'debug)
  ;; (logger:set_application_level 'logjam 'debug)
  ;; (logger:update_handler_config
  ;;  'default
  ;;  #m(formatter #(logjam #m(map_depth 3
  ;;                           term_depth 50))))
  ;; (logger:set_application_level 'default 'debug)
  ;; (lfe_io:format "~p~n" `(,(application:which_applications)))
  ;; (lfe_io:format "~p~n" `(,(logger:get_config)))
  (logger:debug "This is a debug-level message")
  (logger:info "This is an info-level message")
  (logger:notice "This is a notice-level message")
  (logger:warning "This is a warning-level message")
  (logger:error "This is an error-level message")
  (logger:critical "This is a critical-level message")
  (logger:alert "This is an alert-level message")
  (logger:emergency "This is an emergency-level message"))
