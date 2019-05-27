(defmodule logjam-srv
  (behaviour gen_server)
  (export (start_link 0)
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

(defun start_link ()
  (gen_server:start `#(local ,(MODULE))
                    (MODULE)
                    (read-server-data)
                    '()))

(defun stop ()
  (gen_server:cast (MODULE) 'stop))

;;; gen_server callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun handle_cast
  (('reload _state-data)
    `#(noreply ,(read-server-data)))
  ((`#(reload ,key) state-data)
    (let* ((disk-config (read-server-data))
           (reset-config (proplists:lookup key disk-config)))
      `#(noreply ,(lists:merge (list reset-config) state-data))))
  ((`#(set ,config-data) _state-data)
    `#(noreply ,config-data))
  ((`#(set ,key ,value) state-data)
    `#(noreply ,(lists:merge (list (tuple key value)) state-data)))
  (('stop state-data)
    `#(stop normal ,state-data)))

(defun handle_call
  (('get _caller state-data)
    `#(reply ,state-data ,state-data))
  ((`#(get ,keys) _caller state-data)
    `#(reply 
       ,(lists:foldl #'proplists:get_value/2 state-data keys) 
       ,state-data))
  ((message _caller state-data)
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

;;; utility functions

(defun read-server-data ()
  (lists:sort (logjam-cfg:config)))
