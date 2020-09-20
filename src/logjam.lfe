;;;; This is the main module that exposes custom formatting to the OTP logger
;;;; library (part of the `kernel' application since OTP-21).
;;;;
;;;; The module honors the standard configuration of the kernel's default
;;;; logger formatter regarding: max depth, templates.
(defmodule logjam
  (export
   (format 2))
  (export-macro
   log debug info notice warn warning error critical alert emergency))

(include-lib "logjam/include/logjam.hrl")

;;;==========================================================================
;;; API functions
;;;==========================================================================
(defun format
  (((= `#m(msg #(report #m(label #(error_logger ,_) format ,format args ,terms))) data) user-config)
    (format (map-update 
              data 
              'msg `#(report #m(text ,(logjam_formatter:format_to_binary format terms))))
            user-config))
  ((`#m(level ,level msg #(report ,msg) meta ,meta) user-config) (when (is_map msg))
    (let ((config (logjam_formatter:apply_defaults user-config))
          (new-meta (maps:merge meta `#m(level ,level
                                         colored_start ,level
                                         colored_end ,(COLOR_END)))))
      (logjam_formatter:format_log (maps:get 'template config) config msg new-meta)))
  (((= `#m(msg #m(report ,key-val)) data) user-config) (when (is_list key-val))
    (format (map-update
              data
              'msg `#(report ,(maps:from_list key-val)))
            user-config))
  (((= `#m(msg #(string ,string)) data) user-config)
    (format (map-update
              data
              'msg `#(report #m(text ,(logjam_formatter:string_to_binary string))))
            user-config))
  (((= `#m(msg #(,format ,terms)) data) user-config)
    (format (map-update
              data
              'msg `#(report #m(text ,(logjam_formatter:format_to_binary format terms))))
            user-config)))

;;;==========================================================================
;;; API macros
;;;==========================================================================

;;; XXX these aren't working yet ...

(defmacro log args
  `(apply LOG ,args))

(defmacro debug args
  `(apply log-debug ,args))

(defmacro info args
  `(log-info ,@args))

(defmacro notice args
  `(log-notice ,@args))

(defmacro warn args
  `(log-warn ,@args))

(defmacro warning args
  `(log-warning ,@args))

(defmacro error args
  `(log-error ,@args))

(defmacro critical args
  `(log-critical ,@args))

(defmacro alert args
  `(log-alert ,@args))

(defmacro emergency args
  `(log-emergency ,@args))
