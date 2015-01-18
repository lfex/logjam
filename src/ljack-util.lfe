(defmodule ljack-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'ljack))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(lumberjack ,(get-version)))))
