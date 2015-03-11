(defmodule integration-logjam-tests
  (behaviour ltest-integration)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "logjam/include/logjam.lfe")

(defun set-up ()
  (logjam:setup))

(defun tear-down (setup-result)
  (application:stop 'logjam))

(deftestcase from-include (setup-result)
  (is (try (progn (info "Running test case mod-func ...") 'true)
           (catch (err (error err))))))

(deftestcase from-mod-func (setup-result)
  (is (try (progn (logjam:info "Running test case mod-func ...") 'true)
           (catch (err (error err))))))

(deftestgen test-generator
  (tuple
    'foreach
    (defsetup set-up)
    (defteardown tear-down)
    (deftestcases
      from-include
      from-mod-func)))
