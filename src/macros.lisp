;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(cl:in-package #:esrap)

;;; Miscellany

(defun text (&rest arguments)
  "Arguments must be strings, or lists whose leaves are strings.
Catenates all the strings in arguments into a single string."
  (with-output-to-string (s)
    (labels ((cat-list (list)
               (dolist (elt list)
                 (etypecase elt
                   (string (write-string elt s))
                   (character (write-char elt s))
                   (list (cat-list elt))))))
      (cat-list arguments))))

(defun text/bounds (strings start end)
  (declare (ignore start end))
  (text strings))

(defun lambda/bounds (function)
  (lambda (result start end)
    (declare (ignore start end))
    (funcall function result)))

(defun identity/bounds (identity start end)
  (declare (ignore start end))
  identity)

;;; DEFRULE support functions

(defun parse-lambda-list-maybe-containing-&bounds (lambda-list)
  "Parse &BOUNDS section in LAMBDA-LIST and return three values:

1. The standard lambda list sublist of LAMBDA-LIST
2. A symbol that should be bound to the start of a matching substring
3. A symbol that should be bound to the end of a matching substring
4. A list containing symbols that were GENSYM'ed.

The second and/or third values are GENSYMS if LAMBDA-LIST contains a
partial or no &BOUNDS section, in which case fourth value contains them
for use with IGNORE."
  (let ((length (length lambda-list)))
    (multiple-value-bind (lambda-list start end gensyms)
        (cond
          ;; Look for &BOUNDS START END.
          ((and (>= length 3)
                (eq (nth (- length 3) lambda-list) '&bounds))
           (values (subseq lambda-list 0 (- length 3))
                   (nth (- length 2) lambda-list)
                   (nth (- length 1) lambda-list)
                   nil))
          ;; Look for &BOUNDS START.
          ((and (>= length 2)
                (eq (nth (- length 2) lambda-list) '&bounds))
           (let ((end (gensym "END")))
             (values (subseq lambda-list 0 (- length 2))
                     (nth (- length 1) lambda-list)
                     end
                     (list end))))
          ;; No &BOUNDS section.
          (t
           (let ((start (gensym "START"))
                 (end (gensym "END")))
             (values lambda-list
                     start
                     end
                     (list start end)))))
      (check-type start symbol)
      (check-type end symbol)
      (values lambda-list start end gensyms))))

(defun check-lambda-list (lambda-list spec
                          &key
                          (report-lambda-list lambda-list))
  (multiple-value-bind
        (required* optional* rest* keyword* allow-other-keys-p auxp keyp)
      (parse-ordinary-lambda-list lambda-list)
    (labels ((fail (expected actual)
               (let ((expected (ensure-list expected))
                     (actual   (ensure-list actual)))
                 (error "~@<Expected a lambda-list ~?, but ~S ~?.~@:>"
                        (first expected) (rest expected)
                        report-lambda-list
                        (first actual) (rest actual))))
             (check-section (section expected actual)
               (typecase expected
                 ((eql nil)
                  (when actual
                    (fail (list "without ~A parameters" section)
                          (list "has ~A parameters" section))))
                 ((eql t)
                  (unless actual
                    (fail (list "with ~A parameters" section)
                          (list "has no ~A parameters" section))))
                 (integer
                  (unless (length= expected actual)
                    (fail (list "with ~D ~A parameter~:*~:P" expected section)
                          (list "has ~D ~A parameter~:*~:P"
                                (length actual) section))))))
             (check-binary (name expected actual)
               (when (member expected '(t nil))
                 (unless (eq expected (when actual t))
                   (fail (list "~:[without~;with~] ~A" expected name)
                         (list "~:[has no~;has~] ~A" actual name)))))
             (check-simple-spec (&key required optional rest
                                      keyword allow-other-keys aux key)
               (check-section "required"         required         required*)
               (check-section "optional"         optional         optional*)
               (check-binary  '&rest             rest             rest*)
               (check-section "keyword"          keyword          keyword*)
               (check-binary  '&allow-other-keys allow-other-keys allow-other-keys-p)
               (check-section "aux"              aux              auxp)
               (check-binary  '&key              key              keyp))
             (check-spec (spec)
               (typecase spec
                 ((cons (eql or))
                  (loop :with errors = ()
                     :for sub-spec :in (rest spec)
                     :do (handler-case
                             (progn
                               (check-spec sub-spec)
                               (return))
                           (error (condition)
                             (push condition errors)))
                     :finally (error "~@<~{~A~^~@:_~}~@:>" errors)))
                 (list
                  (apply #'check-simple-spec spec)))))
      (check-spec spec))))

(defun parse-defrule-options (options form)
  (let ((transform nil)
        (around nil)
        (guard t)
        (condition t)
        (guard-seen nil))
    (dolist (option options)
      (flet ((set-transform (trans/bounds trans/no-bounds
                                          &optional use-start-end? start-end-symbols)
               (setf transform
                     (cond
                       ((not transform)
                        trans/bounds)
                       (use-start-end?
                        (error "~@<Trying to use ~{~S~^, ~} in composed ~
                                ~S transformation.~@:>"
                               start-end-symbols use-start-end?))
                       (t
                        `(compose ,trans/no-bounds ,transform)))))
             (set-guard (expr test)
               (if guard-seen
                   (error "~@<Multiple guards in ~S:~@:_~2@T~S~@:>"
                          'defrule form)
                   (setf guard-seen t
                         guard expr
                         condition test))))
        (destructuring-ecase option
          ((:when expr &rest rest)
           (when rest
             (error "~@<Multiple expressions in a ~S:~@:_~2@T~S~@:>"
                    :when form))
           (set-guard expr (cond
                             ((not (constantp expr)) `(lambda () ,expr))
                             ((eval expr) t))))
          ((:constant value)
           (set-transform `(constantly ,value) `(constantly ,value)))
          ((:text value)
           (when value
             (set-transform '#'text/bounds '#'text)))
          ((:identity value)
           (when value
             (set-transform '#'identity/bounds '#'identity)))
          ((:lambda lambda-list &body forms)
           (multiple-value-bind (lambda-list* start end ignore)
               (parse-lambda-list-maybe-containing-&bounds lambda-list)
             (declare (type list ignore))
             (check-lambda-list lambda-list*
                                '(or (:required 1) (:optional 1))
                                :report-lambda-list lambda-list)
             (apply #'set-transform
                    `(lambda (,@lambda-list* ,start ,end)
                       (declare (ignore ,@ignore))
                       ,@forms)
                    `(lambda ,lambda-list* ,@forms)
                    (unless (length= 2 ignore)
                      (list option
                            (set-difference (list start end) ignore))))))
          ((:function designator)
           (set-transform `(lambda/bounds
                            (resolve-function ',designator '(production) ',option))
                          `(resolve-function ',designator '(production) ',option)))
          ((:destructure lambda-list &body forms)
           (multiple-value-bind (lambda-list start end ignore)
               (parse-lambda-list-maybe-containing-&bounds lambda-list)
             (set-transform
              (with-gensyms (production)
                `(lambda (,production ,start ,end)
                   (declare (ignore ,@ignore))
                   (destructuring-bind ,lambda-list ,production
                     ,@forms)))
              (with-gensyms (production)
                `(lambda (,production)
                   (destructuring-bind ,lambda-list ,production
                     ,@forms))))))
          ((:around lambda-list &body forms)
           (multiple-value-bind (lambda-list* start end ignore)
               (parse-lambda-list-maybe-containing-&bounds lambda-list)
             (check-lambda-list
              lambda-list* '() :report-lambda-list lambda-list)
             (setf around `(lambda (,start ,end transform)
                             (declare (ignore ,@ignore)
                                      (function transform))
                             (flet ((call-transform ()
                                      (funcall transform)))
                               ,@forms))))))))
    (values transform around guard condition)))
