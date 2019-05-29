(defmodule integration-logjam-tests
  (behaviour ltest-integration)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "logjam/include/lager.lfe")

(defun set-up ()
  (logjam:start))

(defun tear-down (setup-result)
  (logjam:stop))

(deftestcase from-include (setup-result)
  (is (try (progn (logjam:info "Running test case mod-func ...") 'true)
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
