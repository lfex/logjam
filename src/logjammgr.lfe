(defmodule logjammgr
  (behaviour supervisor)
  (export (start_link 0)  (start_link 1)
          (init 1))
  (export (sup-flags 0) (child-spec 1)))

(defun start_link ()
  (start_link '()))

(defun start_link (args)
  (let ((link (supervisor:start_link `#(local ,(MODULE))
                                     (MODULE)
                                     args)))
    (logjam:debug "Linked supervisor status: ~p" (list link))
    link))

(defun init (args)
  (logjam:debug "Initializing logjam ...")
  (let ((check (supervisor:check_childspecs (list (child-spec args)))))
    (logjam:debug "Supervisor child specs status: ~p" (list check)))
  `#(ok #(,(sup-flags)
          ,(list (child-spec args)))))

(defun status ()
  (supervisor:count_children (MODULE)))
  
;;; Setup functions

;; (defun sup-flags ()
;;   #M(strategy one_for_all 
;;      intensity 1
;;      period 3000))

(defun sup-flags ()
  #(one_for_all 
    1
    3000))

;; (defun child-spec (args)
;;   `#M(id logjamd
;;       start #(logjamd start_link ,args)
;;       restart permanent
;;       shutdown 5000
;;       type worker
;;       modules (logjamd)))

(defun child-spec (args)
  `#(logjamd
     #(logjamd start_link ,args)
     permanent
     5000
     worker
     (logjamd)))
