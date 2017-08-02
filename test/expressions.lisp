;;;; Copyright (c) 2017 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package #:esrap-tests)

(in-suite esrap)

(test %direct-expression-dependencies.smoke
  "Smoke test for the %EXPRESSION-DEPENDENCIES function."

  (mapc (destructuring-lambda ((expression expected-dependencies))
          (let ((result (%expression-direct-dependencies expression)))
            (is (equal result (remove-duplicates result)))
            (is (set-equal expected-dependencies result))))
        '((character                  ())
          ((character-ranges #\a #\z) ())
          ((string 5)                 ())
          ((and #\a #\b)              ())
          ((or #\a #\b)               ())
          ((not #\a)                  ())
          ((* #\a)                    ())
          ((+ #\a)                    ())
          ((? #\a)                    ())
          ((& #\a)                    ())
          ((! #\a)                    ())
          ((< 1 #\a)                  ())
          ((> 1 #\a)                  ())
          (#\c                        ())
          ("foo"                      ())
          ((~ "foo")                  ())

          (foo                        (foo))
          ((and foo foo)              (foo))
          ((and foo bar)              (foo bar))
          ((consp foo)                (foo))

          ((foo #\a)                  ())
          (#'foo                      ()))))

(defrule expression-dependencies.1
    (and #\c (? expression-dependencies.undefined)
         expression-dependencies.1
         expression-dependencies.2))

(defrule expression-dependencies.2
    expression-dependencies.3)

(defrule expression-dependencies.3
    #\d)

(test %expression-dependencies.smoke
  "Smoke test for the %EXPRESSION-DEPENDENCIES function."

  (mapc (destructuring-lambda ((expression expected-dependencies))
          (let ((result (%expression-dependencies expression)))
            (is (equal result (remove-duplicates result)))
            (is (set-equal expected-dependencies result))))
        '((foo                       (foo))
          ((and foo foo)             (foo))
          ((and foo bar)             (foo bar))
          (expression-dependencies.1 (expression-dependencies.undefined
                                      expression-dependencies.3
                                      expression-dependencies.2
                                      expression-dependencies.1))
          (expression-dependencies.2 (expression-dependencies.3
                                      expression-dependencies.2))
          (expression-dependencies.3 (expression-dependencies.3)))))
