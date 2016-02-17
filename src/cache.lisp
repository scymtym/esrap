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

(in-package :esrap)

;;; MEMOIZATION CACHE
;;;
;;; Because each [rule, position] tuple has an unambiguous
;;; result per source text, we can cache this result -- this is what
;;; makes packrat parsing O(N).
;;;
;;; For now we just use EQUAL hash-tables, but a specialized
;;; representation would probably pay off.

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position cache)
  (gethash (cons symbol position) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash (cons symbol position) cache) result))

;; In case of left recursion, this stores
(defstruct (head (:predicate nil) (:copier nil))
  ;; the rule at which the left recursion started
  (rule (required-argument :rule) :type symbol)
  ;; the set of involved rules
  (involved-set '() :type list)
  ;; and the set of rules which rules which can still be applied in
  ;; the current round of "seed parse" growing
  (eval-set '() :type list))

(defvar *heads*)

(defun make-heads ()
  (make-hash-table :test #'equal))

(defun get-head (position heads)
  (gethash position heads))

(defun (setf get-head) (head position heads)
  (setf (gethash position heads) head))

(defun recall (rule position cache heads thunk)
  (let ((result (get-cached rule position cache))
        (head (get-head position heads)))
    (cond
      ;; If not growing a seed parse, just return what is stored in
      ;; the cache.
      ((not head)
       result)
      ;; Do not evaluate any rule that is not involved in this left
      ;; recursion.
      ((and (not result) (not (or (eq rule (head-rule head))
                                  (member rule (head-involved-set head)))))
       (make-failed-parse nil position nil))
      ;; Allow involved rules to be evaluated, but only once, during a
      ;; seed-growing iteration. Subsequent requests just return what
      ;; is stored in the cache.
      (t
       (when (member rule (head-eval-set head))
         (removef (head-eval-set head) rule :count 1)
         (setf result (funcall thunk position)
               (get-cached rule position cache) result))
       result))))

(defvar *nonterminal-stack* nil)

;;; SYMBOL and POSITION must all lexical variables!
(defmacro with-cached-result ((symbol position &optional (text nil)) &body forms)
  (with-gensyms (cache heads result)
    `(flet ((do-it (position) ,@forms))
       (let* ((,cache *cache*)
              (,heads *heads*)
              (,result (recall ,symbol ,position ,cache ,heads #'do-it)))
         (cond
           ;; Found left-recursion marker in the cache. Depending on
           ;; *ERROR-ON-LEFT-RECURSION*, we either signal an error or
           ;; prepare recovery from this situation (which is performed
           ;; by one of the "cache miss" cases (see below) up the
           ;; call-stack).
           ((left-recursion-result-p ,result)
            ;; If error on left-recursion has been requested, do that.
            (when (eq *on-left-recursion* :error)
              (left-recursion ,text ,position ,symbol
                              (reverse (mapcar #'left-recursion-result-rule
                                               *nonterminal-stack*))))
            ;; Otherwise, mark left recursion and fail this partial
            ;; parse.
            (let ((head (or (left-recursion-result-head ,result)
                            (setf (left-recursion-result-head ,result)
                                  (make-head :rule ,symbol)))))
              ;; Put this head into left recursion markers on the
              ;; stack. Add rules on the stack to the "involved set".
              (dolist (item *nonterminal-stack*)
                (when (eq (left-recursion-result-head item) head)
                  (return))
                (setf (left-recursion-result-head item) head)
                (pushnew (left-recursion-result-rule item)
                         (head-involved-set head))))
            (make-failed-parse ,symbol ,position nil))
           ;; Cache hit without left-recursion.
           (,result
            ,result)
           ;; Cache miss.
           (t
            ;; First add a left recursion marker for this pair, then
            ;; compute the result, potentially recovering from left
            ;; recursion and cache that.
            (let* ((result (make-left-recursion-result ,symbol))
                   (result1
                    (let ((*nonterminal-stack* (cons result *nonterminal-stack*)))
                      (setf (get-cached ,symbol ,position ,cache)
                            result
                            (get-cached ,symbol ,position ,cache)
                            (do-it position)))))
              ;; If we detect left recursion, handle it.
              (when (and (not (error-result-p result1))
                         (left-recursion-result-head result))
                (let ((head (left-recursion-result-head result)))
                  ;; Grow "seed parse" (grow-lr in the paper):
                  ;; repeatedly apply rules involved in left-recursion
                  ;; until no progress can be made.
                  (setf (get-head ,position ,heads) head)
                  (loop
                     (setf (head-eval-set head)
                           (copy-list (head-involved-set head)))
                     (let ((result2 (do-it ,position)))
                       (when (or (error-result-p result2)
                                 (<= (result-position result2)
                                     (result-position result1))) ; no progress
                         (return))
                       (setf (get-cached ,symbol ,position ,cache)
                             (%make-successful-parse
                              ,symbol (result-position result2)
                              result2 #'successful-parse-production)
                             result1 result2)))
                  (setf (get-head ,position ,heads) nil)))
              result1)))))))
