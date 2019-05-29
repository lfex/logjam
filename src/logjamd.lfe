(defmodule logjamd
  (behaviour gen_server)
  (export (start_link 1)
          (stop 0)
          (init 1)
          (terminate 2)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (code_change 3))
  (export (reload-config 0) (reload-config 1)
          (set-config 1) (set-config 2)
          (get-config 0) (get-config 1)))

;;; gen_server implementation

(defun start_link (args)
  (let* ((options '())
         (link (gen_server:start_link `#(local ,(MODULE))
                                      (MODULE)
                                      args
                                      options)))
    (logjam:debug "Linked server status: ~p" `(,link))
    link))

(defun stop ()
  (gen_server:cast (MODULE) 'stop))

;;; gen_server callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (logjam:debug 'logjamd
                 'handle_info/2
                 "Process ~p exited! (Reason: ~p)"
                 `(,pid ,reason))
   `#(noreply ,state-data))
  ((msg state-data)
   (logjam:debug 'logjamd
                 'handle_info/2
                 "Unhandled info message: ~p"
                 `(,msg))
   `#(noreply ,state-data)))

(defun handle_cast
  ((`#(level ,level) state-data)
    )
  (('reload _state-data)
    `#(noreply ,(logjam-cfg:read-config)))
  ((`#(reload ,key) state-data)
    (let* ((disk-config (logjam-cfg:read-config))
           (reset-config (proplists:lookup key disk-config)))
      `#(noreply ,(lists:merge `(,reset-config) state-data))))
  ((`#(set ,config-data) _state-data)
    `#(noreply ,config-data))
  ((`#(set ,key ,value) state-data)
    `#(noreply ,(lists:merge `(,(tuple key value)) state-data)))
  (('stop state-data)
    `#(stop normal ,state-data))
  ((msg state-data)
   (logjam:debug 'logjamd
                 'handle_cast/2
                 "Unhandled cast message: ~p"
                 `(,msg))
   `#(noreply ,state-data)))

(defun handle_call
  (('get _caller state-data)
    `#(reply ,state-data ,state-data))
  ((`#(get ,keys) _caller state-data)
    `#(reply
       ,(logjam-util:get-in state-data keys)
       ,state-data))
  ((msg _caller state-data)
   (logjam:debug 'logjamd
                 'handle_call3/
                 "Unhandled call message: ~p"
                 `(,msg))
    `#(reply #(error "Unknown command") ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; logjam server imlpementation

(defun reload-config ()
  (gen_server:cast (MODULE) 'reload))

(defun reload-config (key)
  (gen_server:cast (MODULE) `#(reload ,key)))

(defun set-config (config-data)
  (gen_server:cast (MODULE) `#(set ,config-data)))

(defun set-config (key value)
  (gen_server:cast (MODULE) `#(set ,key ,value)))

(defun get-config ()
  (gen_server:call (MODULE) 'get))

(defun get-config (keys)
  (gen_server:call (MODULE) `#(get ,keys)))
