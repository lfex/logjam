(defmodule unit-ljack-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest my-adder
  (is-equal 4 (ljack:my-adder 2 2)))
