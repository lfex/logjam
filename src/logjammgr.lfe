(defmodule logjammgr
  (behaviour supervisor)
  (export (start_link 1)
          (init 1))
  (export (sup-flags 0) (child-spec 1)))

(defun start_link (config-data)
  (logjam:debug "Starting supervision tree with data: ~p" (list config-data))
  (let ((link (supervisor:start_link `#(local ,(MODULE))
                                     (MODULE)
                                     `(,config-data))))
    (logjam:debug "Linked supervisor status: ~p" `(,link))
    link))

(defun init (config-data)
  (logjam:debug "Initializing logjam ...")
  (let* ((children-specs `(,(child-spec config-data)))
         (check (supervisor:check_childspecs children-specs)))
    (logjam:debug "Children specs: ~p" children-specs)
    (logjam:debug "Supervisor child specs validation: ~p" `(,check))
    (logjam:debug "Logjam initialized.")
    `#(ok #(,(sup-flags)
            ,children-specs))))

(defun status ()
  (supervisor:count_children (MODULE)))

;;; Setup functions

(defun sup-flags ()
  #(one_for_all
    1
    3000))

(defun child-spec (config-data)
  `#(logjamd
     #(logjamd start_link ,config-data)
     permanent
     5000
     worker
     (logjamd)))
