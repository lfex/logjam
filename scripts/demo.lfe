#! /usr/bin/env lfescript

(include-lib "lfe/include/clj.lfe")

(defun main
  ((`(,config-file))
    (let ((`#(ok (,cfg)) (file:consult config-file)))
      (logger:set_primary_config #m(level all))
      (logger:set_handler_config
       'default
       (->> cfg
            (proplists:get_value 'kernel)
            (proplists:get_value 'logger)
            (car)
            (element 4)))
      (timer:sleep 500)
      (logjam-demo:run)
      (timer:sleep 1000)
      (io:format "~n")))
  ((args)
   (io:format "One arguement is required for the demo: the path to a config file.~n")
   (io:format "(Got ~p)~n" `(,args))))