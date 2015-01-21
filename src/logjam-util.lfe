(defmodule logjam-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'logjam))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(logjam ,(get-version)))))
