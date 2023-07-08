#! /usr/bin/env lfescript

(include-lib "lfe/include/clj.lfe")

(defun main
  ((`(,config-file))
    (logjam:set-config `#(path ,config-file))
    (timer:sleep 500)
    (logjam-demo:run)
    (timer:sleep 1000)
    (io:format "~n"))
  ((args)
   (io:format "One arguement is required for the demo: the path to a config file.~n")
   (io:format "(Got ~p)~n" `(,args))))
