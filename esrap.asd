;;;;  Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;;  Copyright (c) 2012-2015 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :esrap-system
  (:use :cl :asdf))

(in-package :esrap-system)

(defsystem :esrap
  :version "0.12"
  :description "A Packrat / Parsing Grammar / TDPL parser for Common Lisp."
  :author "Nikodemus Siivola <nikodemus@random-state.net>"
  :maintainer "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :licence "MIT"
  :depends-on (:alexandria)
  :components ((:file "esrap")
               (:static-file "example-sexp.lisp")
               (:static-file "example-symbol-table.lisp")
               (:static-file "example-left-recursion.lisp")
               (:static-file "example-function-terminals.lisp")
               (:static-file "README.org")))

(defmethod perform :after ((op load-op) (sys (eql (find-system :esrap))))
  ;; Since version 0.13
  (pushnew :esrap.expression-start-terminals *features*)
  ;; Since version 0.12
  (pushnew :esrap.function-terminals *features*)
  ;; Since version 0.11
  (pushnew :esrap.multiple-transforms *features*)
  ;; Since version 0.10
  (pushnew :esrap.can-handle-left-recursion *features*)

  ;; For consistency with examples which contain (require :esrap).
  (provide :esrap))

(defsystem :esrap-tests
  :description "Tests for ESRAP."
  :licence "MIT"
  :depends-on (:esrap :fiveam)
  :serial     t
  :components ((:file "example-left-recursion")
               (:file "example-function-terminals")
               (:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :esrap))))
  (load-system :esrap-tests)
  (funcall (intern "RUN-TESTS" :esrap-tests)))
