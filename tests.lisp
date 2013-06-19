;;;;  Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net.net>
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

(in-package :cl-user)

(defpackage :esrap-tests
  (:use :alexandria :cl :esrap :eos)
  (:shadowing-import-from :esrap "!")
  (:export #:run-tests))

(in-package :esrap-tests)

(def-suite esrap)
(in-suite esrap)

(test defrule.check-expression
  "Test expression checking in DEFRULE."
  (macrolet ((is-invalid-expr (&body body)
               `(signals invalid-expression-error ,@body)))
    (is-invalid-expr (defrule foo '(~ 1)))
    (is-invalid-expr (defrule foo '(string)))
    (is-invalid-expr (defrule foo '(character-ranges 1)))
    (is-invalid-expr (defrule foo '(character-ranges (#\a))))
    (is-invalid-expr (defrule foo '(character-ranges (#\a #\b #\c))))
    (is-invalid-expr (defrule foo '(and (string))))
    (is-invalid-expr (defrule foo '(not)))
    (is-invalid-expr (defrule foo '(foo)))))

;;;; A few semantic predicates

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-digit (char)
  (when (find-if-not #'digit-char-p char)
    t))

(defun not-newline (char)
  (not (eql #\newline char)))

(defun not-space (char)
  (not (eql #\space char)))

;;;; Utility rules

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:text t))

(defrule empty-line #\newline
  (:constant ""))

(defrule non-empty-line (and (+ (not-newline character)) (? #\newline))
  (:destructure (text newline)
    (declare (ignore newline))
    (text text)))

(defrule line (or empty-line non-empty-line)
  (:identity t))

(defrule trimmed-line line
  (:lambda (line)
    (string-trim '(#\space #\tab) line)))

(defrule trimmed-lines (* trimmed-line)
  (:identity t))

(defrule digits (+ (digit-char-p character))
  (:text t))

(defrule integer (and (? whitespace)
                      digits
                      (and (? whitespace) (or (& #\,) (! character))))
  (:destructure (whitespace digits tail)
    (declare (ignore whitespace tail))
    (parse-integer digits)))

(defrule list-of-integers (+ (or (and integer #\, list-of-integers)
                                 integer))
  (:destructure (match)
    (if (integerp match)
        (list match)
        (destructuring-bind (int comma list) match
          (declare (ignore comma))
          (cons int list)))))

(test smoke
  (is (equal '("1," "2," "" "3," "4.")
             (parse 'trimmed-lines "1,
                                    2,

                                    3,
                                    4.")))
  (is (eql 123 (parse 'integer "  123")))
  (is (eql 123 (parse 'integer "  123  ")))
  (is (eql 123 (parse 'integer "123  ")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "123, 45  ,   6789, 0")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "  123 ,45,6789, 0  "))))

(defrule single-token/bounds.1 (+ (not-space character))
  (:lambda (result &bounds start end)
    (format nil "~A[~S-~S]" (text result) start end)))

(defrule single-token/bounds.2 (and (not-space character) (* (not-space character)))
  (:destructure (first &rest rest &bounds start end)
    (format nil "~C~A(~S-~S)" first (text rest) start end)))

(defrule tokens/bounds.1 (and (? whitespace)
                              (or (and single-token/bounds.1 whitespace tokens/bounds.1)
                                  single-token/bounds.1))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule tokens/bounds.2 (and (? whitespace)
                              (or (and single-token/bounds.2 whitespace tokens/bounds.2)
                                  single-token/bounds.2))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(test bounds.1
  (is (equal '("foo[0-3]")
             (parse 'tokens/bounds.1 "foo")))
  (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
             (parse 'tokens/bounds.1 "foo bar    quux"))))

(test bounds.2
  (is (equal '("foo(0-3)")
             (parse 'tokens/bounds.2 "foo")))
  (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
             (parse 'tokens/bounds.2 "foo bar    quux"))))

;;; Left recursion tests

(defun make-input-and-expected-result (size)
  (labels ((make-expected (size)
             (if (plusp size)
                 (list (make-expected (1- size)) "l")
                 "r")))
    (let ((expected (make-expected size)))
      (values (apply #'concatenate 'string (flatten expected)) expected))))

(defrule left-recursion.direct
    (or (and left-recursion.direct #\l) #\r))

(test left-recursion.direct.success
  "Test parsing with one left recursive rule for different inputs."
  (dotimes (i 20)
    (multiple-value-bind (input expected)
        (make-input-and-expected-result i)
      (is (equal expected (parse 'left-recursion.direct input))))))

(test left-recursion.direct.condition
  "Test signaling of `left-recursion' condition if requested."
  (let ((*error-on-left-recursion* t))
    (signals (left-recursion)
      (parse 'left-recursion.direct "l"))
    (handler-case (parse 'left-recursion.direct "l")
      (left-recursion (condition)
        (is (string= (esrap-error-text condition) "l"))
        (is (= (esrap-error-position condition) 0))
        (is (eq (left-recursion-nonterminal condition)
                'left-recursion.direct))
        (is (equal (left-recursion-path condition)
                   '(left-recursion.direct
                     left-recursion.direct)))))))

(defrule left-recursion.indirect.1 left-recursion.indirect.2)

(defrule left-recursion.indirect.2 (or (and left-recursion.indirect.1 "l") "r"))

(test left-recursion.indirect.success
  "Test parsing with mutually left recursive rules for different
   inputs."
  (dotimes (i 20)
    (multiple-value-bind (input expected)
        (make-input-and-expected-result i)
      (is (equal expected (parse 'left-recursion.indirect.1 input)))
      (is (equal expected (parse 'left-recursion.indirect.2 input))))))

(test left-recursion.indirect.condition
  "Test signaling of `left-recursion' condition if requested."
  (let ((*error-on-left-recursion* t))
    (signals (left-recursion)
      (parse 'left-recursion.indirect.1 "l"))
    (handler-case (parse 'left-recursion.indirect.1 "l")
      (left-recursion (condition)
        (is (string= (esrap-error-text condition) "l"))
        (is (= (esrap-error-position condition) 0))
        (is (eq (left-recursion-nonterminal condition)
                'left-recursion.indirect.1))
        (is (equal (left-recursion-path condition)
                   '(left-recursion.indirect.1
                     left-recursion.indirect.2
                     left-recursion.indirect.1)))))))

;;; Test conditions

(test condition.1
  "Test signaling of `esrap-simple-parse-error' conditions for failed
   parses."
  (macrolet
      ((signals-esrap-error ((input position &optional messages) &body body)
         `(progn
            (signals (esrap-error)
              ,@body)
            (handler-case (progn ,@body)
              (esrap-error (condition)
                (is (string= (esrap-error-text condition) ,input))
                (is (= (esrap-error-position condition) ,position))
                ,@(when messages
                    `((let ((report (princ-to-string condition)))
                        ,@(mapcar (lambda (message)
                                    `(is (search ,message report)))
                                  (ensure-list messages))))))))))
    (signals-esrap-error ("" 0 ("Could not parse subexpression"
                                "Encountered at"))
      (parse 'integer ""))
    (signals-esrap-error ("123foo" 3 ("Could not parse subexpression"
                                      "Encountered at"))
      (parse 'integer "123foo"))
    (signals-esrap-error ("1, " 1 ("Incomplete parse."
                                   "Encountered at"))
      (parse 'list-of-integers "1, "))))

(test parse.string
  "Test parsing an arbitrary string of a given length."
  (is (equal "" (parse '(string 0) "")))
  (is (equal "aa" (parse '(string 2) "aa")))
  (signals esrap-error (parse '(string 0) "a"))
  (signals esrap-error (parse '(string 2) "a"))
  (signals esrap-error (parse '(string 2) "aaa")))

(test parse.case-insensitive
  "Test parsing an arbitrary string of a given length."
  (dolist (input '("aabb" "AABB" "aAbB" "aaBB" "AAbb"))
    (is (equal "aabb" (text (parse '(* (or (~ #\a) (~ #\b))) input))))
    (is (equal "AABB" (text (parse '(* (or (~ #\A) (~ #\B))) input))))
    (is (equal "aaBB" (text (parse '(* (or (~ #\a) (~ #\B))) input))))))

(test parse.negation
  "Test negation in rules."
  (let* ((text "FooBazBar")
         (t1c (text (parse '(+ (not "Baz")) text :junk-allowed t)))
         (t1e (text (parse (identity '(+ (not "Baz"))) text :junk-allowed t)))
         (t2c (text (parse '(+ (not "Bar")) text :junk-allowed t)))
         (t2e (text (parse (identity '(+ (not "Bar"))) text :junk-allowed t)))
         (t3c (text (parse '(+ (not (or "Bar" "Baz"))) text :junk-allowed t)))
         (t3e (text (parse (identity '(+ (not (or "Bar" "Baz")))) text :junk-allowed t))))
    (is (equal "Foo" t1c))
    (is (equal "Foo" t1e))
    (is (equal "FooBaz" t2c))
    (is (equal "FooBaz" t2e))
    (is (equal "Foo" t3c))
    (is (equal "Foo" t3e))))

;;; Test around

(declaim (special *depth*))
(defvar *depth* nil)

(defrule around/inner
    (+ (alpha-char-p character))
  (:text t))

(defrule around.1
    (or around/inner
        (and #\{ around.1 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around ()
    (let ((*depth* (if *depth*
                       (cons (1+ (first *depth*)) *depth*)
                       (list 0))))
      (call-transform))))

(defrule around.2
    (or around/inner
        (and #\{ around.2 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around (&bounds start end)
    (let ((*depth* (if *depth*
                       (cons (cons (1+ (car (first *depth*))) (cons start end))
                             *depth*)
                       (list (cons 0 (cons start end))))))
      (call-transform))))

(test around.1
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.1 ,input) ,expected))))
    (test-case "foo"     '((0) . "foo"))
    (test-case "{bar}"   '((1 0) . "bar"))
    (test-case "{{baz}}" '((2 1 0) . "baz"))))

(test around.2
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.2 ,input) ,expected))))
    (test-case "foo"     '(((0 . (0 . 3)))
                           . "foo"))
    (test-case "{bar}"   '(((1 . (1 . 4))
                            (0 . (0 . 5)))
                           . "bar"))
    (test-case "{{baz}}" '(((2 . (2 . 5))
                            (1 . (1 . 6))
                            (0 . (0 . 7)))
                           . "baz"))))

;;; Test character ranges

(defrule character-range (character-ranges (#\a #\b) #\-))

(test character-range
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab" :junk-allowed t)))
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab1" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* (character-ranges (#\a #\z) #\-)) "ab-" :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "AB-" :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "ZY-" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* character-range) "ab-cd" :junk-allowed t))))

;;; Test multiple transforms

(defrule multiple-transforms.1
    (and #\a #\1 #\c)
  (:function second)
  (:text t)
  (:function parse-integer))

(test multiple-transforms.1
  "Apply composed transforms to parse result."
  (is (eql (parse 'multiple-transforms.1 "a1c") 1)))

(test multiple-transforms.invalid
  "Test DEFRULE's behavior for invalid transforms."
  (dolist (form '((defrule multiple-transforms.2 #\1
                    (:text t)
                    (:lambda (x &bounds start end)
                      (parse-integer x)))
                  (defrule multiple-transforms.3 #\1
                    (:text t)
                    (:lambda (x &bounds start)
                      (parse-integer x)))))
    (signals simple-error (macroexpand-1 form))))

;; Test README examples

(test examples-from-readme.foo
  "README examples related to \"foo+\" rule."
  (is (equal '("foo" nil)
             (multiple-value-list (parse '(or "foo" "bar") "foo"))))
  (is (eq 'foo+ (add-rule 'foo+
                          (make-instance 'rule :expression '(+ "foo")))))
  (is (equal '(("foo" "foo" "foo") nil)
             (multiple-value-list (parse 'foo+ "foofoofoo")))))

(test examples-from-readme.decimal
  "README examples related to \"decimal\" rule."
  (is (eq 'decimal
          (add-rule
           'decimal
           (make-instance
            'rule
            :expression `(+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
            :transform (lambda (list start end)
                         (declare (ignore start end))
                         (parse-integer (format nil "~{~A~}" list)))))))
  (is (eql 123 (parse '(oddp decimal) "123")))
  (is (equal '(nil 0) (multiple-value-list
                       (parse '(evenp decimal) "123" :junk-allowed t)))))

;;; Examples in separate files

(test example-left-recursion.left-associative
  "Left associate grammar from example-left-recursion.lisp."
  ;; Should be parsable without
  (let ((*error-on-left-recursion* t))
    (is (equal (parse 'left-recursive-grammars:la-expr "1*2+3*4+5")
               '(+ (* 1 2) (+ (* 3 4) 5))))))

(test example-left-recursion.right-associative
  "Right associate grammar from example-left-recursion.lisp."
  ;; This grammar should
  (let ((*error-on-left-recursion* t))
    (signals left-recursion
      (parse 'left-recursive-grammars:ra-expr "1*2+3*4+5")))

  (is (equal (parse 'left-recursive-grammars:ra-expr "1*2+3*4+5")
             '(+ (+ (* 1 2) (* 3 4)) 5))))

(test example-left-recursion.warth
  "Warth's Java expression example from example-left-recursion.lisp."
 (mapc
  (curry #'apply
         (lambda (input expected)
           (is (equal expected
                      (parse 'left-recursive-grammars:primary input)))))
  '(("this"       "this")
    ("this.x"     (:field-access "this" "x"))
    ("this.x.y"   (:field-access (:field-access "this" "x") "y"))
    ("this.x.m()" (:method-invocation (:field-access "this" "x") "m"))
    ("x[i][j].y"  (:field-access (:array-access (:array-access "x" "i") "j") "y")))))

;;; Test runner

(defun run-tests ()
  (let ((results (run 'esrap)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))
