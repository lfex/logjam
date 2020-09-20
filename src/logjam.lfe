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

(include-lib "include/logjam.hrl")

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
              'msg `#(report #m(text (logjam_formatter:string_to_binary string))))
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
  `(apply LOG_DEBUG ,args))

(defmacro info args
  `(LOG_INFO ,@args))

(defmacro notice args
  `(LOG_NOTICE ,@args))

(defmacro warn args
  `(LOG_WARNING ,@args))

(defmacro warning args
  `(LOG_WARNING ,@args))

(defmacro error args
  `(LOG_ERROR ,@args))

(defmacro critical args
  `(CRITICAL ,@args))

(defmacro alert args
  `(LOG_ALERT ,@args))

(defmacro emergency args
  `(LOG_EMERGENCY ,@args))
