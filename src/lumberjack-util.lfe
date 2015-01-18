(defmodule lumberjack-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'lumberjack))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(lumberjack ,(get-version)))))
