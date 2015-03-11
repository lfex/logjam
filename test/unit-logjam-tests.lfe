(defmodule unit-logjam-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest placeholder
  (is-equal 1 1))
