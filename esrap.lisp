;;;; ESRAP -- a packrat parser for Common Lisp
;;;;
;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2015 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Homepage and documentation:
;;;;
;;;;   http://scymtym.github.com/esrap/
;;;;
;;;; References:
;;;;
;;;;   * Bryan Ford, 2002, "Packrat Parsing: a Practical Linear Time
;;;;     Algorithm with Backtracking".
;;;;     http://pdos.csail.mit.edu/~baford/packrat/thesis/
;;;;
;;;;   * Alessandro Warth, James R. Douglass, Todd Millstein, 2008,
;;;;     "Packrat Parsers Can Support Left Recursion".
;;;;     http://www.vpri.org/pdf/tr2007002_packrat.pdf
;;;;
;;;; Licence:
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

(defpackage :esrap
  (:use :cl :alexandria)
  #+sbcl
  (:lock t)
  (:export
   #:&bounds

   #:! #:? #:+ #:* #:& #:~
   #:character-ranges

   #:*on-left-recursion*

   #:add-rule
   #:call-transform
   #:change-rule
   #:defrule
   #:describe-grammar
   #:describe-terminal
   #:esrap-error
   #:esrap-error-position
   #:esrap-error-text
   #:expression-start-terminals
   #:find-rule
   #:invalid-expression-error
   #:invalid-expression-error-expression
   #:left-recursion
   #:left-recursion-nonterminal
   #:left-recursion-path
   #:parse
   #:remove-rule
   #:rule
   #:rule-dependencies
   #:rule-expression
   #:rule-symbol
   #:text
   #:trace-rule
   #:untrace-rule
   #:undefined-rule-error
   #:undefined-rule-symbol
   ))

(in-package :esrap)

;;; Conditions

(define-condition invalid-expression-error (error)
  ((expression :initarg :expression :reader invalid-expression-error-expression))
  (:default-initargs
   :expression (required-argument :expression))
  (:documentation
   "Signaled when an invalid expression is encountered."))

(defmethod print-object ((condition invalid-expression-error) stream)
  (format stream "Invalid expression: ~S"
          (invalid-expression-error-expression condition)))

(defun invalid-expression-error (expression)
  (error 'invalid-expression-error :expression expression))

(define-condition esrap-error (parse-error)
  ((text :initarg :text :initform nil :reader esrap-error-text)
   (position :initarg :position :initform nil :reader esrap-error-position))
  (:documentation
   "Signaled when an Esrap parse fails. Use ESRAP-ERROR-TEXT to obtain the
string that was being parsed, and ESRAP-ERROR-POSITION the position at which
the error occurred."))

(defmethod print-object :before ((condition esrap-error) stream)
  (when (or *print-escape*
            *print-readably*
            (and *print-lines* (<= *print-lines* 5)))
    (return-from print-object))

  ;; FIXME: this looks like it won't do the right thing when used as
  ;; part of a logical block.
  (if-let ((text (esrap-error-text condition))
           (position (esrap-error-position condition)))
    (labels ((safe-index (index)
               (min (max index 0) (length text)))
             (find-newline (&key (start 0) (end (length text)) (from-end t))
               (let ((start (safe-index start))
                     (end   (safe-index end)))
                (cond
                  ((when-let ((position (position #\Newline text
                                                  :start start :end end
                                                  :from-end from-end)))
                     (1+ position)))
                  ((and from-end (zerop start))
                   start)
                  ((and (not from-end) (= end (length text)))
                   end)))))
      ;; FIXME: magic numbers
      (let* ((line       (count #\Newline text :end position))
             (column     (- position (or (find-newline :end position) 0) 1))
             (min-start  (- position 160))
             (max-end    (+ position 24))
             (line-start (or (find-newline :start min-start
                                           :end   position)
                             (safe-index min-start)))
             (start      (cond
                           ((= (safe-index min-start) line-start)
                            line-start)
                           ((find-newline :start min-start
                                          :end   (1- line-start)))
                           (t

                            line-start)))
             (end        (or (find-newline :start    position
                                           :end      max-end
                                           :from-end nil)
                             (safe-index max-end)))
             (*print-circle* nil))
        (format stream "At~:[~; end of input~]~2%~
                        ~2@T~<~@;~A~:>~%~
                        ~2@T~V@T^ (Line ~D, Column ~D, Position ~D)~2%"
                (= position (length text))
                (list (subseq text start end))
                (- position line-start)
                (1+ line) (1+ column) position)))

    (format stream "~2&<text and position not available>~2%")))

(define-condition simple-esrap-error (esrap-error simple-condition) ())

(defmethod print-object ((condition simple-esrap-error) stream)
  (apply #'format stream
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(declaim (ftype (function (t t t &rest t) (values &optional nil))
                simple-esrap-error))
(defun simple-esrap-error (text position format-control &rest format-arguments)
  (error 'simple-esrap-error
         :text text
         :position position
         :format-control format-control
         :format-arguments format-arguments))

(define-condition left-recursion (esrap-error)
  ((nonterminal :initarg :nonterminal :initform nil :reader left-recursion-nonterminal)
   (path :initarg :path :initform nil :reader left-recursion-path))
  (:documentation
   "May be signaled when left recursion is detected during Esrap parsing.

LEFT-RECURSION-NONTERMINAL names the symbol for which left recursion
was detected, and LEFT-RECURSION-PATH lists nonterminals of which the
left recursion cycle consists.

Note: This error is only signaled if *ON-LEFT-RECURSION* is bound
to :ERROR."))

(defmethod print-object ((condition left-recursion) stream)
  (format stream "Left recursion in nonterminal ~S. ~_Path: ~
                  ~{~S~^ -> ~}"
          (left-recursion-nonterminal condition)
          (left-recursion-path condition)))

(defun left-recursion (text position nonterminal path-butlast)
  (error 'left-recursion
         :text text
         :position position
         :nonterminal nonterminal
         :path (append path-butlast (list nonterminal))))

(define-condition undefined-rule (condition)
  ((symbol :initarg :symbol
           :type symbol
           :reader undefined-rule-symbol)))

(defmethod print-object ((condition undefined-rule) stream)
  (format stream "~@<The rule ~S is undefined.~@:>"
          (undefined-rule-symbol condition)))

(define-condition undefined-rule-error (undefined-rule error)
  ()
  (:documentation
   "Signaled when an undefined rule is encountered."))

(defun undefined-rule (symbol)
  (error 'undefined-rule-error :symbol symbol))

;;; Miscellany

(deftype left-recursion-policy ()
  '(or null (eql :error)))

(declaim (type left-recursion-policy *on-left-recursion*))

(defvar *on-left-recursion* nil
  "This special variable controls Esrap's behavior with respect to
allowing left recursion.

When :ERROR, PARSE signals a LEFT-RECURSION error when it encounters a
left recursive rule. Otherwise the rule is processed.

Note: when processing left recursive rules, linear-time guarantees
generally no longer hold.")

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

(deftype nonterminal ()
  "Any symbol except CHARACTER and NIL can be used as a nonterminal symbol."
  '(and symbol (not (member character nil))))

(deftype terminal ()
  "Literal strings and characters are used as case-sensitive terminal symbols,
and expressions of the form \(~ <literal>) denote case-insensitive terminals."
  '(or string character
       (cons (eql ~) (cons (or string character) null))))

(deftype character-range ()
  "A character range is either a single character or a list of two
characters."
  '(or character
       (cons character (cons character null))))

(deftype predicate-name ()
  '(and symbol
        (not (member character-ranges string and or not * + ? & ! ~
                     function))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *expression-kinds*
    `((character        . (eql character))
      (character-ranges . (cons (eql character-ranges)))
      (string           . (cons (eql string) (cons array-length null)))
      (and              . (cons (eql and)))
      (or               . (cons (eql or)))
      ,@(mapcar (lambda (symbol)
                  `(,symbol . (cons (eql ,symbol) (cons t null))))
                '(not * + ? & !))
      (terminal         . terminal)
      (nonterminal      . nonterminal)
      (predicate        . (cons predicate-name (cons (not null) null)))
      (function         . (cons (eql function) (cons symbol null)))
      (t                . t))
    "Names and corresponding types of acceptable expression
constructors."))

;;; RULE REPRESENTATION AND STORAGE
;;;
;;; For each rule, there is a RULE-CELL in *RULES*, whose %INFO slot has the
;;; function that implements the rule in car, and the rule object in CDR. A
;;; RULE object can be attached to only one non-terminal at a time, which is
;;; accessible via RULE-SYMBOL.

(defvar *rules* (make-hash-table))

(defun clear-rules ()
  (clrhash *rules*)
  nil)

(defstruct (rule-cell (:constructor
                       make-rule-cell
                       (symbol &aux (%info (cons (undefined-rule-function symbol) nil))))
                      (:conc-name cell-))
  ;; A cons
  ;;
  ;;   (FUNCTION . RULE)
  ;;
  ;; where
  ;;
  ;; FUNCTION is a function with lambda-list (text position end) which
  ;; is called to do the actual parsing work (or immediately signal an
  ;; error in case of referenced but undefined rules).
  ;;
  ;; RULE is a RULE instance associated to the cell or nil for
  ;; referenced but undefined rules.
  (%info (required-argument :%info) :type (cons function t))
  (trace-info nil)
  (referents nil :type list))

(declaim (inline cell-function))
(defun cell-function (cell)
  (car (cell-%info cell)))

(defun cell-rule (cell)
  (cdr (cell-%info cell)))

(defun set-cell-info (cell function rule)
  ;; Atomic update
  (setf (cell-%info cell) (cons function rule))
  cell)

(defun undefined-rule-function (symbol)
  (lambda (&rest args)
    (declare (ignore args))
    (undefined-rule symbol)))

(defun ensure-rule-cell (symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock *RULES*.
  (or (gethash symbol *rules*)
      (setf (gethash symbol *rules*)
            (make-rule-cell symbol))))

(defun delete-rule-cell (symbol)
  (remhash symbol *rules*))

(defun reference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (when referent
      (pushnew referent (cell-referents cell)))
    cell))

(defun dereference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (symbol)
  (check-type symbol nonterminal)
  (gethash symbol *rules*))

(defclass rule ()
  ((%symbol
    :initform nil)
   (%expression
    :initarg :expression
    :initform (required-argument :expression))
   (%guard-expression
    :initarg :guard-expression
    :initform t
    :reader rule-guard-expression)
   ;; Either T for rules that are always active (the common case),
   ;; NIL for rules that are never active, or a function to call
   ;; to find out if the rule is active or not.
   (%condition
    :initarg :condition
    :initform t
    :reader rule-condition)
   (%transform
    :initarg :transform
    :initform nil
    :reader rule-transform)
   (%around
    :initarg :around
    :initform nil
    :reader rule-around)))

(defun rule-symbol (rule)
  "Returns the nonterminal associated with the RULE, or NIL of the rule
is not attached to any nonterminal."
  (slot-value rule '%symbol))

(defun detach-rule (rule)
  (dolist (dep (%rule-direct-dependencies rule))
    (dereference-rule-cell dep (rule-symbol rule)))
  (setf (slot-value rule '%symbol) nil))

(defmethod shared-initialize :after ((rule rule) slots &key)
  (declare (ignore slots))
  (check-expression (rule-expression rule)))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type t :identity nil)
    (let ((symbol (rule-symbol rule)))
      (if symbol
          (format stream "~S <- " symbol)
          (format stream "(detached) ")))
    (write (rule-expression rule) :stream stream)))

(defun sort-dependencies (symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies
   (rule-symbol rule) (%expression-dependencies (rule-expression rule) nil)))

(defun rule-direct-dependencies (rule)
  (sort-dependencies
   (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

(defun %rule-direct-dependencies (rule)
  (delete (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

;;; Expression destructuring and validation

(defmacro with-expression ((expr lambda-list) &body body)
  (let* ((type (car lambda-list))
         (car-var (gensym "CAR"))
         (fixed-list (cons car-var (cdr lambda-list))))
    (once-only (expr)
      `(destructuring-bind ,fixed-list ,expr
         ,(if (eq t type)
              `(declare (ignore ,car-var))
              `(unless (eq ',type ,car-var)
                 (error "~S-expression expected, got: ~S" ',type ,expr)))
         (locally ,@body)))))

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
(defstruct head
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
       (make-failed-parse :position position))
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
              (left-recursion ,text,position ,symbol
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
            (make-failed-parse :expression ,symbol
                               :position ,position))
           ;; Cache hit without left-recursion.
           (,result
            ,result)
           ;; Cache miss.
           (t
            ;; First add a left recursion marker for this pair, then
            ;; compute the result, potentially recovering from left
            ;; recursion and cache that.
            (let* ((result (make-left-recursion-result :rule ,symbol))
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
                            (%make-result :position (result-position result2)
                                          :%production (result-%production result2))
                            result1 result2)))
                  (setf (get-head ,position ,heads) nil)))
              result1)))))))

;;; RESULT REPRESENTATION
;;;
;;; We always return a result -- ERROR-RESULT for failed parses, and
;;; RESULT for successes.
;;;
;;; We implement a simple lazy evaluation for the productions. This is
;;; used to perform semantic actions only when necessary -- either
;;; when we call a semantic predicate or once parse has finished.

(defstruct (error-result (:copier nil)))

(defstruct (inactive-rule (:include error-result) (:copier nil))
  ;; Name of the rule that was inactive.
  (rule (required-argument :rule) :type symbol :read-only t))

(defstruct (failed-parse (:include error-result) (:copier nil))
  ;; Expression that failed to match.
  (expression nil :read-only t) ; TODO required?
  ;; Position at which match was attempted.
  (position (required-argument) :type array-index :read-only t)
  ;; A nested error, closer to actual failure site.
  (detail nil :type (or null string condition error-result) :read-only t))

;; This is placed in the cache as a place in which information
;; regarding left recursion can be stored temporarily.
(defstruct (left-recursion-result (:include error-result) (:copier nil))
  (rule (required-argument :rule) :type symbol :read-only t)
  (head nil :type (or null head)))

(defstruct (result (:constructor %make-result) (:copier nil))
  ;; Either a list of results, whose first element is the production, or a
  ;; function to call that will return the production.
  (%production nil :type (or list function))
  ;; Position after the match.
  (position (required-argument :position) :type array-index :read-only t))

(defmacro make-result (&rest arguments &key production &allow-other-keys)
  (if production
      (let ((args (copy-list arguments)))
        (remf args :production)
        `(%make-result ,@args
                       :%production ,(if (symbolp production)
                                         `(list ,production)
                                         `(lambda () ,production))))
      `(%make-result ,@arguments)))

(defun result-production (result)
  (let ((thunk (result-%production result)))
    (if (functionp thunk)
        (let ((value (funcall thunk)))
          (setf (result-%production result) (list value))
          value)
        (car thunk))))

;;; MAIN INTERFACE

(defun parse (expression text &key (start 0) end junk-allowed raw)
  "Parses TEXT using EXPRESSION from START to END.

Incomplete parses, that is not consuming the entirety of TEXT, are
allowed only if JUNK-ALLOWED is true.

Returns three values:

1) A production, if the parse succeeded, NIL otherwise.
2) The position up to which TEXT has been consumed or NIL if the
   entirety of TEXT has been consumed.
3) If the parse succeeded, even if it did not consume any input, T is
   returned as a third value.

The third return value is necessary to distinguish successful and
failed parses for cases like

  (parse '(! #\\a) \"a\" :junk-allowed t)
  (parse '(! #\\a) \"b\" :junk-allowed t)

in which the first two return values cannot indicate failures.

RAW controls whether the parse result is interpreted and translated
into the return values described above. If RAW is true, a parse result
of type RESULT or ERROR-RESULT is returned as a single value.

Note that the combination of arguments :junk-allowed t :raw t does not
make sense since the JUNK-ALLOWED parameter is used when parse results
are interpreted and translated into return values which does not
happen when :raw t."
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (when (and junk-allowed raw)
    (error "~@<The combination of arguments ~{~S~^ ~} does not make ~
            sense.~@:>"
           (list :junk-allowed junk-allowed :raw raw)))
  (let* ((end (or end (length text)))
         (*cache* (make-cache))
         (*heads* (make-heads))
         (result (eval-expression expression text start end)))
    (if raw
        result
        (process-parse-result result text start end junk-allowed))))

(define-compiler-macro parse (&whole form expression text
                              &rest arguments &key &allow-other-keys
                              &environment env)
  (flet ((make-expansion (result-var rawp junk-allowed-p body)
           ;; This inline-lambda provides keyword defaults and
           ;; parsing, so the compiler-macro doesn't have to worry
           ;; about evaluation order.
           (with-gensyms (expr-fun)
             `(let ((,expr-fun (load-time-value (compile-expression ,expression))))
                ((lambda (text &key (start 0) end
                                    ,@(if rawp '(raw))
                                    ,@(if junk-allowed-p '(junk-allowed)))
                   (let* ((end (or end (length text)))
                          (*cache* (make-cache))
                          (*heads* (make-heads))
                          (,result-var (funcall ,expr-fun text start end)))
                     ,body))
                 ,text ,@(remove-from-plist arguments :raw))))))
   (cond
     ((not (constantp expression env))
      form)
     ((let ((raw (getf arguments :raw 'missing)))
        (when (and (not (eq raw 'missing))
                   (constantp raw env))
          (let ((rawp (eval raw)))
            (make-expansion 'result nil (not rawp)
                            (if rawp
                                'result
                                '(process-parse-result
                                  result text start end junk-allowed)))))))
     (t
      (make-expansion 'result t t
                      '(if raw
                           result
                           (process-parse-result
                            result text start end junk-allowed)))))))

(defun process-parse-result (result text start end junk-allowed)
  (cond
    ;; Successfully parsed something.
    ((not (error-result-p result))
     (let ((position (result-position result)))
       (values
        (result-production result)
        (cond
          ((= position end) nil) ; Consumed all input.
          (junk-allowed position) ; Did not consume all input; junk is OK.
          (t (simple-esrap-error text position "Incomplete parse.")))
        t)))
    ;; Did not parse anything, but junk is allowed.
    (junk-allowed
     (values nil start))
    ;; Did not parse anything and junk is not allowed.
    ((failed-parse-p result)
     (labels ((expressions (e)
                (etypecase e
                  (null
                   '())
                  (inactive-rule
                   (list (list (inactive-rule-rule e) "(not active)")))
                  (failed-parse
                   ;; The detail slot may contain nil, a condition or
                   ;; string, or a nested parse error result.
                   (let ((expression (failed-parse-expression e))
                         (detail (failed-parse-detail e)))
                     (if (typep detail '(or string condition))
                         (list (list expression
                                     (format nil "~%~6@T(~A)" detail)))
                         (cons (list expression)
                               (expressions detail))))))))
       (let ((expressions (expressions result)))
         (simple-esrap-error text (failed-parse-position result)
                             "Could not parse subexpression ~S when ~
                              parsing~2&~< Expression ~{~S~^ ~A~}~@{~&    ~
                              Subexpression ~{~S~^ ~A~}~}~:>"
                             (first (lastcar expressions))
                             expressions))))
    ;; Parse failed because of an inactive rule.
    (t
     (simple-esrap-error text nil "Rule ~S not active"
                         (inactive-rule-rule result)))))

(defmacro defrule (&whole form symbol expression &body options)
  "Define SYMBOL as a nonterminal, using EXPRESSION as associated the parsing expression.

Multiple OPTIONS specifying transforms are composed in the order of
appearance:

  (:text t)
  (:function parse-integer)
  =>
  (alexandria:compose #'parse-integer #'text)

Following OPTIONS can be specified:

  * (:WHEN TEST)

    The rule is active only when TEST evaluates to true. This can be used
    to specify optional extensions to a grammar.

    This option can only be supplied once.

  * (:CONSTANT CONSTANT)

    No matter what input is consumed or what EXPRESSION produces, the production
    of the rule is always CONSTANT.

  * (:FUNCTION FUNCTION)

    If provided the production of the expression is transformed using
    FUNCTION. FUNCTION can be a function name or a lambda-expression.

  * (:IDENTITY BOOLEAN)

    If true, the production of expression is used as-is, as if (:FUNCTION IDENTITY)
    has been specified. If no production option is specified, this is the default.

  * (:TEXT BOOLEAN)

    If true, the production of expression is flattened and concatenated into a string
    as if by (:FUNCTION TEXT) has been specified.

  * (:LAMBDA LAMBDA-LIST &BODY BODY)

    If provided, same as using the corresponding lambda-expression with :FUNCTION.

    As an extension of the standard lambda list syntax, LAMBDA-LIST accepts
    the optional pseudo lambda-list keyword ESRAP:&BOUNDS, which (1) must appear
    after all standard lambda list keywords. (2) can be followed by one or two
    variables to which bounding indexes of the matching substring are bound.

    Therefore:

      LAMBDA-LIST ::= (STANDARD-LAMBDA-LIST-ELEMENTS [&BOUNDS START [END]])

  * (:DESTRUCTURE DESTRUCTURING-LAMBDA-LIST &BODY BODY)

    If provided, same as using a lambda-expression that destructures its argument
    using DESTRUCTURING-BIND and the provided lambda-list with :FUNCTION.

    DESTRUCTURING-LAMBDA-LIST can use ESRAP:&BOUNDS in the same way
    as described for :LAMBDA.

  * (:AROUND ([&BOUNDS START [END]]) &BODY BODY)

    If provided, execute BODY around the construction of the production of the
    rule. BODY has to call ESRAP:CALL-TRANSFORM to trigger the computation of
    the production. Any transformation provided via :LAMBDA, :FUNCTION
    or :DESTRUCTURE is executed inside the call to ESRAP:CALL-TRANSFORM. As a
    result, modification to the dynamic state are visible within the
    transform.

    ESRAP:&BOUNDS can be used in the same way as described for :LAMBDA
    and :DESTRUCTURE.

    This option can be used to safely track nesting depth, manage symbol
    tables or for other stack-like operations.
"
  (let ((transform nil)
        (around nil)
        (guard t)
        (condition t)
        (guard-seen nil))
    (when options
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
             (multiple-value-bind (lambda-list start end ignore)
                 (parse-lambda-list-maybe-containing-&bounds lambda-list)
               (declare (type list ignore))
               (apply #'set-transform
                      `(lambda (,@lambda-list ,start ,end)
                         (declare (ignore ,@ignore))
                         ,@forms)
                      `(lambda (,@lambda-list) ,@forms)
                      (unless (length= 2 ignore)
                        (list option
                              (set-difference (list start end) ignore))))))
            ((:function designator)
             (set-transform `(lambda/bounds (function ,designator))
                            `(function ,designator)))
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
             (multiple-value-bind (lambda-list start end ignore)
                 (parse-lambda-list-maybe-containing-&bounds lambda-list)
               (assert (null lambda-list))
               (setf around `(lambda (,start ,end transform)
                               (declare (ignore ,@ignore)
                                        (function transform))
                               (flet ((call-transform ()
                                        (funcall transform)))
                                 ,@forms)))))))))
    `(eval-when (:load-toplevel :execute)
       (add-rule ',symbol (make-instance 'rule
                                         :expression ',expression
                                         :guard-expression ',guard
                                         :transform ,(or transform '#'identity/bounds)
                                         :around ,around
                                         :condition ,condition)))))

(defun add-rule (symbol rule)
  "Associates RULE with the nonterminal SYMBOL. Signals an error if the
rule is already associated with a nonterminal. If the symbol is already
associated with a rule, the old rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type symbol nonterminal)
  (when (rule-symbol rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))
  (let* ((cell (ensure-rule-cell symbol))
         (function (compile-rule symbol
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)
                                 (rule-around rule)))
         (trace-info (cell-trace-info cell)))
    (set-cell-info cell function rule)
    (setf (cell-trace-info cell)     nil
          (slot-value rule '%symbol) symbol)
    (when trace-info
      (destructuring-bind (break condition) (rest trace-info)
        (trace-rule symbol :break break :condition condition)))
    symbol))

(defun find-rule (symbol)
  "Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal
symbol."
  (check-type symbol nonterminal)
  (when-let ((cell (find-rule-cell symbol)))
    (cell-rule cell)))

(defun remove-rule (symbol &key force)
  "Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an
already referred to by other rules, an error is signalled unless :FORCE is
true."
  (check-type symbol nonterminal)
  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((cell (find-rule-cell symbol))
         (rule (cell-rule cell))
         (trace-info (cell-trace-info cell)))
    (when cell
      (flet ((frob ()
               (set-cell-info cell (undefined-rule-function symbol) nil)
               (when trace-info
                 (setf (cell-trace-info cell) (list (cell-%info cell) (second trace-info))))
               (when rule
                 (detach-rule rule))))
        (cond ((and rule (cell-referents cell))
               (unless force
                 (error "Nonterminal ~S is used by other nonterminal~P:~% ~{~S~^, ~}"
                        symbol (length (cell-referents cell)) (cell-referents cell)))
               (frob))
              ((not (cell-referents cell))
               (frob)
               ;; There are no references to the rule at all, so
               ;; we can remove the cell.
               (unless trace-info
                 (delete-rule-cell symbol)))))
      rule)))

(defvar *trace-level* 0)

(defun trace-rule (symbol &key recursive break condition)
  "Turn on tracing of nonterminal SYMBOL. If RECURSIVE is true, turn
on tracing for the whole grammar rooted at SYMBOL.

If BREAK is true, break is entered when the rule is invoked.

If supplied, CONDITION has to be a function whose lambda-list is
compatible to (symbol text position end). This function is called to
determine whether trace actions should be executed for the traced
rule.

  SYMBOL is the name of the rule being executed.

  TEXT is the whole text being parsed.

  POSITION is the position within TEXT at which the rule is executed.

  END is the end position of the portion of TEXT being parsed."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((traced (symbol break fun text position end)
               (when break
                 (break "rule ~S" symbol))
               (format *trace-output* "~&~V@T~D: ~S ~S?~%"
                       *trace-level* (1+ *trace-level*) symbol position)
               (finish-output *trace-output*)
               (let* ((*trace-level* (1+ *trace-level*))
                      (result (funcall fun text position end)))
                 (format *trace-output* "~&~V@T~D: ~S "
                         (1- *trace-level*) *trace-level* symbol)
                 (if (error-result-p result)
                     (format *trace-output* "-|~%")
                     (format *trace-output* "~S-~S -> ~S~%"
                             position (result-position result)
                             (result-production result)))
                 (finish-output *trace-output*)
                 result))
             (traced/condition (condition symbol break fun text position end)
               (if (funcall condition symbol text position end)
                   (traced symbol break fun text position end)
                   (funcall fun text position end)))
             (trace-one (symbol cell)
               ;; Avoid infinite recursion and processing sub-trees
               ;; multiple times.
               (if (gethash cell seen)
                   (return-from trace-one)
                   (setf (gethash cell seen) t))
               ;; If there is old trace information, removed it first.
               (when (cell-trace-info cell)
                 (untrace-rule symbol))
               ;; Wrap the cell function in a tracing function. Store
               ;; old info in trace-info slot of CELL.
               (let ((fun (cell-function cell))
                     (rule (cell-rule cell))
                     (info (cell-%info cell)))
                 (set-cell-info
                  cell (if condition
                           (curry #'traced/condition condition symbol break fun)
                           (curry #'traced symbol break fun))
                  rule)
                 (setf (cell-trace-info cell) (list info break condition))
                 ;; If requested, trace dependencies
                 ;; recursively. Checking RULE avoids recursing into
                 ;; referenced but undefined rules.
                 (when (and recursive rule)
                   (dolist (dep (%rule-direct-dependencies rule))
                     (trace-one dep (find-rule-cell dep)))))
               t))
      (trace-one symbol (or (find-rule-cell symbol)
                            (undefined-rule symbol))))))

(defun untrace-rule (symbol &key recursive break condition)
  "Turn off tracing of nonterminal SYMBOL.

If RECURSIVE is true, untraces the whole grammar rooted at SYMBOL.

BREAK and CONDITION are ignored, and are provided only for symmetry
with TRACE-RULE."
  (declare (ignore break condition))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((untrace-one (cell)
               ;; Avoid infinite recursion and processing sub-trees
               ;; multiple times.
               (if (gethash cell seen)
                   (return-from untrace-one)
                   (setf (gethash cell seen) t))
               ;; Restore info from trace-info slot of CELL.
               (let ((rule (cell-rule cell))
                     (trace-info (cell-trace-info cell)))
                 (when trace-info
                   (setf (cell-%info cell) (car trace-info)
                         (cell-trace-info cell) nil))
                 ;; If requested, trace dependencies
                 ;; recursively. Checking RULE avoids recursing into
                 ;; referenced but undefined rules.
                 (when (and recursive rule)
                   (dolist (dep (%rule-direct-dependencies rule))
                     (untrace-one (find-rule-cell dep)))))
               nil))
      (untrace-one (or (find-rule-cell symbol)
                       (undefined-rule symbol))))))

(defun rule-expression (rule)
  "Return the parsing expression associated with the RULE."
  (slot-value rule '%expression))

(defun (setf rule-expression) (expression rule)
  "Modify RULE to use EXPRESSION as the parsing expression. The rule must be
detached beforehand."
  (let ((name (rule-symbol rule)))
    (when name
      (error "~@<Cannot change the expression of an active rule, ~
              remove ~S first, or use CHANGE-RULE.~:@>"
             name))
    (setf (slot-value rule '%expression) expression)))

(defun change-rule (symbol expression)
  "Modifies the nonterminal SYMBOL to use EXPRESSION instead. Temporarily
removes the rule while it is being modified."
  (let ((rule (remove-rule symbol :force t)))
    (unless rule
      (undefined-rule symbol))
    (setf (rule-expression rule) expression)
    (add-rule symbol rule)))

(defun symbol-length (x)
  (length (symbol-name x)))

(defun describe-grammar (symbol &optional (stream *standard-output*))
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human
inspection."
  (check-type symbol nonterminal)
  (let ((rule (find-rule symbol)))
    (cond ((not rule)
           (format stream "Symbol ~S is not a defined nonterminal." symbol))
          (t
           (format stream "~&Grammar ~S:~%" symbol)
           (multiple-value-bind (defined undefined) (rule-dependencies rule)
             (let ((length
                    (+ 4 (max (reduce #'max (mapcar #'symbol-length defined)
                                      :initial-value 0)
                              (reduce #'max (mapcar #'symbol-length undefined)
                                      :initial-value 0)))))
               (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                       symbol length (rule-expression rule)
                       (when (rule-condition rule)
                         (rule-guard-expression rule)))
               (when defined
                 (dolist (s defined)
                   (let ((dep (find-rule s)))
                     (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                            s length (rule-expression dep)
                            (when (rule-condition rule)
                              (rule-guard-expression rule))))))
               (when undefined
                 (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                         (length undefined) undefined))))))))

;;; COMPILING RULES

(defvar *current-rule* nil)

(defun compile-rule (symbol expression condition transform around)
  (declare (type (or boolean function) condition transform around))
  (let* ((*current-rule* symbol)
         ;; Must bind *CURRENT-RULE* before compiling the expression!
         (function (compile-expression expression))
         (rule-not-active (make-inactive-rule :rule symbol)))
    (cond ((not condition)
           (named-lambda inactive-rule (text position end)
             (declare (ignore text position end))
             rule-not-active))
          (transform
           (flet ((exec-rule/transform (text position end)
                    (let ((result (funcall function text position end)))
                      (if (error-result-p result)
                          (make-failed-parse
                           :expression symbol
                           :position (if (failed-parse-p result)
                                         (failed-parse-position result)
                                         position)
                           :detail result)
                          (if around
                              (make-result
                               :position (result-position result)
                               :production (flet ((call-rule ()
                                                    (funcall transform
                                                             (result-production result)
                                                             position
                                                             (result-position result))))
                                             (funcall around position (result-position result) #'call-rule)))
                              (make-result
                               :position (result-position result)
                               :production (funcall transform
                                                    (result-production result)
                                                    position
                                                    (result-position result))))))))
             (if (eq t condition)
                 (named-lambda rule/transform (text position end)
                   (with-cached-result (symbol position text)
                     (exec-rule/transform text position end)))
                 (named-lambda condition-rule/transform (text position end)
                   (with-cached-result (symbol position text)
                     (if (funcall condition)
                         (exec-rule/transform text position end)
                         rule-not-active))))))
          (t
           (if (eq t condition)
               (named-lambda rule (text position end)
                 (with-cached-result (symbol position text)
                   (funcall function text position end)))
               (named-lambda conditional-rule (text position end)
                 (with-cached-result (symbol position text)
                   (if (funcall condition)
                       (funcall function text position end)
                       rule-not-active))))))))

;;; EXPRESSION COMPILER & EVALUATOR

(eval-when (:compile-toplevel :execute)
  (defmacro expression-case (expression &body clauses)
    "Similar to

  (cl:typecase EXPRESSION CLAUSES)

but clause heads designate kinds of expressions instead of types. See
*EXPRESSION-KINDS*."
    (let ((available (copy-list *expression-kinds*)))
      (labels ((type-for-expression-kind (kind)
                 (if-let ((cell (assoc kind available)))
                   (progn
                     (removef available cell)
                     (cdr cell))
                   (error "Invalid or duplicate clause: ~S" kind)))
               (process-clause (clause)
                 (destructuring-bind (kind &body body) clause
                   (etypecase kind
                     (cons
                      `((or ,@(mapcar #'type-for-expression-kind kind))
                        ,@body))
                     (symbol
                      `(,(type-for-expression-kind kind)
                        ,@body))))))
        (let ((clauses (mapcar #'process-clause clauses)))
          ;; We did not provide clauses for all expression
          ;; constructors and did not specify a catch-all clause =>
          ;; error.
          (when (and (assoc t available) (> (length available) 1))
            (error "Unhandled expression kinds: ~{~S~^, ~}"
                   (remove t (mapcar #'car available))))
          ;; If we did not specify a catch-all clause, insert one
          ;; which signals INVALID-EXPRESSION-ERROR.
          (once-only (expression)
            `(typecase ,expression
               ,@clauses
               ,@(when (assoc t available)
                   `((t (invalid-expression-error ,expression)))))))))))

(defun check-expression (expression)
  (labels
      ((rec (expression)
         (expression-case expression
           ((character string function terminal nonterminal))
           (character-ranges
            (unless (every (of-type 'character-range) (rest expression))
              (invalid-expression-error expression)))
           ((and or not * + ? & ! predicate)
            (mapc #'rec (rest expression))))))
    (rec expression)))

(defun %expression-dependencies (expression seen)
  (expression-case expression
    ((character string character-ranges function terminal)
     seen)
    (nonterminal
     (if (member expression seen :test #'eq)
         seen
         (let ((rule (find-rule expression))
               (seen (cons expression seen)))
           (if rule
               (%expression-dependencies (rule-expression rule) seen)
               seen))))
    ((and or)
     (dolist (subexpr (cdr expression) seen)
       (setf seen (%expression-dependencies subexpr seen))))
    ((not * + ? & ! predicate)
     (%expression-dependencies (second expression) seen))))

(defun %expression-direct-dependencies (expression seen)
  (expression-case expression
    ((character string character-ranges function terminal)
     seen)
    (nonterminal
     (cons expression seen))
    ((and or)
     (dolist (subexpr (rest expression) seen)
       (setf seen (%expression-direct-dependencies subexpr seen))))
    ((not * + ? & ! predicate)
     (%expression-direct-dependencies (second expression) seen))))

(defun expression-start-terminals (expression)
  "Return a list of terminals or tree of expressions with which a text
   parsable by EXPRESSION can start.

   A tree instead of a list is returned when EXPRESSION contains
   semantic predicates, NOT or !. Elements in the returned list or
   tree are

   * case (in)sensitive characters, character ranges,
     case (in)sensitive strings, function terminals
   * semantic predicates represented as

       (PREDICATE-NAME NESTED-ELEMENTS)

     where NESTED-ELEMENTS is the list of start terminals of the
     expression to which PREDICATE-NAME is applied.
   * NOT and ! expressions are represented as

       ({not,!} NESTED-ELEMENTS)

     where NESTED-ELEMENTS is the list of start terminals of the
     negated expression.

   The (outermost) list is sorted likes this:

   1. string terminals
   2. character terminals
   3. the CHARACTER wildcard terminal
   4. semantic predicates
   5. everything else"
  (labels ((rec (expression seen)
             (expression-case expression
               ((character string character-ranges function terminal)
                (list expression))
               (predicate
                (when-let ((result (rec/sorted (second expression) seen)))
                  (list (list (first expression) result))))
               (nonterminal
                (unless (member expression seen :test #'equal)
                  (when-let ((rule (find-rule expression)))
                    (rec (rule-expression rule) (list* expression seen)))))
               ((not !)
                (when-let ((result (rec/sorted (second expression) seen)))
                  (list (list (first expression) result))))
               ((+ &)
                (rec (second expression) seen))
               ((? *)
                (values (rec (second expression) seen) t))
               (and
                (let ((result '()))
                  (dolist (sub-expression (rest expression) result)
                    (multiple-value-bind (sub-start-terminals optionalp)
                        (rec sub-expression seen)
                      (when sub-start-terminals
                        (appendf result sub-start-terminals)
                        (unless optionalp
                          (return result)))))))
               (or
                (mapcan (rcurry #'rec seen) (rest expression)))))
           (rec/without-duplicates (expression seen)
             (remove-duplicates (rec expression seen) :test #'equal))
           (rec/sorted (expression seen)
             (stable-sort (rec/without-duplicates expression seen)
                          #'expression<)))
    (rec/sorted expression '())))

(defun expression< (left right)
  (or (and (typep left  'string)
           (typep right '(not string)))
      (and (typep left  'string)
           (string-lessp left right))
      (and (typep left  'character)
           (typep right '(not (or string character))))
      (and (typep left  'character)
           (typep right 'character)
           (char-lessp left right))
      (and (typep left  '(eql character))
           (typep left  '(not (eql character))))
      (and (typep left  '(cons predicate-name))
           (typep right '(not (or string character (eql character)
                                  (cons predicate-name)))))
      (typep right '(not (or string character (eql character)
                             (cons predicate-name))))))

(defun describe-terminal (terminal &optional (stream *standard-output*))
  "Print a description of TERMINAL onto STREAM.

   In additional to actual terminals, TERMINAL can be of the forms

     (PREDICATE-NAME TERMINALS)
     ({not,!} TERMINALS)

   (i.e. as produced by EXPRESSION-START-TERMINALS)."
  (labels
      ((output (format-control &rest format-arguments)
         (apply #'format stream format-control format-arguments))
       (rec/sub-expression (sub-expression prefix separator)
         (output prefix (length sub-expression))
         (rec (first sub-expression))
         (loop :for terminal :in (rest sub-expression)
            :do (output separator) (rec terminal)))
       (rec (terminal)
         (expression-case terminal
           (character
            (output "any character"))
           (string
            (output "a string of length ~D" (second terminal)))
           (character-ranges
            (output "a character in ~{[~{~C-~C~}]~^ or ~}"
                    (rest terminal)))
           (function
            (output "a string that can be parsed by the function ~S"
                    (second terminal)))
           (terminal
            (labels ((rec (thing)
                       (etypecase thing
                         (character
                          ;; For non-graphic or whitespace characters,
                          ;; just print the name.
                          (output "the character ~:[~*~A~:;~A (~A)~]"
                                  (and (graphic-char-p thing)
                                       (not (member thing '(#\Space #\Tab #\Newline))))
                                  thing (char-name thing)))
                         (string
                          (if (length= 1 thing)
                              (rec (char thing 0))
                              (output "the string ~S" thing)))
                         ((cons (eql ~))
                          (rec (second thing))
                          (output ", disregarding case")))))
              (rec terminal)))
           ((not !)
            (let ((sub-expression (second terminal)))
              (typecase sub-expression
                ((cons (eql character) null)
                 (output "<end of input>"))
                (t
                 (output "anything but")
                 (pprint-logical-block (stream sub-expression)
                   (rec/sub-expression
                    sub-expression "~[~; ~:; ~5:T~]" "~@:_ and "))))))
           (predicate
            (let ((sub-expression (second terminal)))
              (pprint-logical-block (stream sub-expression)
                (rec/sub-expression
                 sub-expression "~[~;~;~:; ~4:T~]" "~@:_ or ")
                (output "~[~; ~;~:;~@:_~]satisfying ~A"
                        (length sub-expression) (first terminal)))))
           (t
            (error "~@<Not a terminal: ~S~@:>" terminal)))))
    (rec terminal)))

;; For use as ~/esrap:print-terminal/ in format control.
(defun print-terminal (stream terminal &optional colonp atp)
  (declare (ignore colonp atp))
  (describe-terminal terminal stream))

(defun eval-expression (expression text position end)
  (expression-case expression
    (character
     (eval-character text position end))
    (terminal
     (if (consp expression)
         (eval-terminal (string (second expression)) text position end nil)
         (eval-terminal (string expression) text position end t)))
    (nonterminal
     (eval-nonterminal expression text position end))
    (string
     (eval-string expression text position end))
    (and
     (eval-sequence expression text position end))
    (or
     (eval-ordered-choise expression text position end))
    (not
     (eval-negation expression text position end))
    (*
     (eval-greedy-repetition expression text position end))
    (+
     (eval-greedy-positive-repetition expression text position end))
    (?
     (eval-optional expression text position end))
    (&
     (eval-followed-by expression text position end))
    (!
     (eval-not-followed-by expression text position end))
    (character-ranges
     (eval-character-ranges expression text position end))
    (function
     (eval-terminal-function expression text position end))
    (predicate
     (eval-semantic-predicate expression text position end))))

(defun compile-expression (expression)
  (expression-case expression
    (character        (compile-character))
    (terminal         (if (consp expression)
                          (compile-terminal (string (second expression)) nil)
                          (compile-terminal (string expression) t)))
    (nonterminal      (compile-nonterminal expression))
    (string           (compile-string expression))
    (and              (compile-sequence expression))
    (or               (compile-ordered-choise expression))
    (not              (compile-negation expression))
    (*                (compile-greedy-repetition expression))
    (+                (compile-greedy-positive-repetition expression))
    (?                (compile-optional expression))
    (&                (compile-followed-by expression))
    (!                (compile-not-followed-by expression))
    (character-ranges (compile-character-ranges expression))
    (function         (compile-terminal-function expression))
    (predicate        (compile-semantic-predicate expression))))

;;; Characters and strings

(declaim (inline exec-string))
(defun exec-string (length text position end)
  (let ((limit (+ length position)))
    (if (<= limit end)
        (make-result
         :production (subseq text position limit)
         :position limit)
        (make-failed-parse
         :expression `(string ,length)
         :position position))))

(defun eval-character (text position end)
  (if (< position end)
      (make-result
       :production (char text position)
       :position (1+ position))
      (make-failed-parse
       :expression 'character
       :position position)))

(defun compile-character ()
  #'eval-character)

(defun eval-string (expression text position end)
  (with-expression (expression (string length))
    (exec-string length text position end)))

(defun compile-string (expression)
  (with-expression (expression (string length))
    (named-lambda compiled-string (text position end)
      (exec-string length text position end))))

;;; Terminals
;;;
;;; FIXME: It might be worth it to special-case terminals of length 1.

(declaim (inline match-terminal-p))
(defun match-terminal-p (string length text position end case-sensitive-p)
  (and (<= (+ length position) end)
       (if case-sensitive-p
           (string= string text :start2 position :end2 (+ position length))
           (string-equal string text :start2 position :end2 (+ position length)))))

(defun exec-terminal (string length text position end case-sensitive-p)
  (if (match-terminal-p string length text position end case-sensitive-p)
      (make-result
       :position (+ length position)
       :production string)
      (make-failed-parse
       :expression string
       :position position)))

(defun eval-terminal (string text position end case-sensitive-p)
  (exec-terminal string (length string) text position end case-sensitive-p))

(defun compile-terminal (string case-sensitive-p)
  (let ((length (length string)))
    (named-lambda compiled-terminal (text position end)
      (exec-terminal string length text position end case-sensitive-p))))

(defun exec-terminal-function (expression function text position end)
  (declare (type function function))
  ;; The protocol is as follows:
  ;;
  ;; FUNCTION succeeded if one of
  ;; 1) returns three values and RESULT is T
  ;; 2) returns two values and END-POSITION is NIL
  ;; 3) returns two values and (> END-POSITION POSITION)
  ;; 4) returns one value of type RESULT
  ;;
  ;; FUNCTION failed if one of
  ;; 1) returns at least two values and (= END-POSITION POSITION)
  ;;    (since no progress has been made), but only if RESULT is not T
  ;; 2) returns three values and RESULT is a string or a condition
  ;; 3) returns one value of type ERROR-RESULT
  ;;
  ;; When RESULT is a string or a condition, END-POSITION can indicate
  ;; the exact position of the failure but is also allowed to be NIL.
  ;;
  ;; RESULT can be T to indicate success even if (= END-POSITION
  ;; POSITION).
  (multiple-value-bind (production end-position result)
      (funcall function text position end)
    (declare (type (or null non-negative-integer) end-position)
             (type (or null string condition (eql t)) result))
    (cond
      ((or (result-p production) (error-result-p production))
       production)
      ((or (eq result t)
           (and (null result)
                (or (null end-position)
                    (> end-position position))))
       (make-result :position (or end-position end)
                    :production production))
      (t
       (make-failed-parse :expression expression
                          :position (or end-position position)
                          :detail result)))))

(defun eval-terminal-function (expression text position end)
  (with-expression (expression (function function))
    (let ((function (ensure-function function)))
      (exec-terminal-function expression function text position end))))

(defun compile-terminal-function (expression)
  (with-expression (expression (function function))
    (let ((function (ensure-function function)))
      (named-lambda compiled-terminal-function (text position end)
        (exec-terminal-function expression function text position end)))))

;;; Nonterminals

(defparameter *eval-nonterminals* nil)

(defun eval-nonterminal (symbol text position end)
  (if *eval-nonterminals*
      (eval-expression (rule-expression (find-rule symbol)) text position end)
      (funcall (cell-function (ensure-rule-cell symbol)) text position end)))

(defun compile-nonterminal (symbol)
  (let ((cell (reference-rule-cell symbol *current-rule*)))
    (declare (type rule-cell cell))
    (named-lambda compiled-nonterminal (text position end)
      (funcall (cell-function cell) text position end))))

;;; Sequences
;;;
;;; FIXME: It might be better if we actually chained the closures
;;; here, instead of looping over them -- benchmark first, though.

(defun eval-sequence (expression text position end)
  (with-expression (expression (and &rest subexprs))
    (let (results)
      (dolist (expr subexprs
               (make-result
                :position position
                :production (mapcar #'result-production (nreverse results))))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (return (make-failed-parse
                       :expression expression
                       :position position
                       :detail result))
              (setf position (result-position result)))
          (push result results))))))

(defun compile-sequence (expression)
  (with-expression (expression (and &rest subexprs))
    (let ((functions (mapcar #'compile-expression subexprs)))
      (named-lambda compiled-sequence (text position end)
        (let (results)
          (dolist (fun functions
                   (make-result
                    :position position
                    :production (mapcar #'result-production (nreverse results))))
            (let ((result (funcall fun text position end)))
              (if (error-result-p result)
                  (return (make-failed-parse
                           :expression expression
                           :position position
                           :detail result))
                  (setf position (result-position result)))
              (push result results))))))))

;;; Ordered choises

(defun eval-ordered-choise (expression text position end)
  (with-expression (expression (or &rest subexprs))
    (let (last-error)
      (dolist (expr subexprs
               (make-failed-parse
                :expression expression
                :position (if (failed-parse-p last-error)
                              (failed-parse-position last-error)
                              position)
                :detail last-error))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (when (or (and (not last-error)
                             (or (inactive-rule-p result)
                                 (< position (failed-parse-position result))))
                        (and last-error
                             (failed-parse-p result)
                             (or (inactive-rule-p last-error)
                                 (< (failed-parse-position last-error)
                                    (failed-parse-position result)))))
                (setf last-error result))
              (return result)))))))

(defun compile-ordered-choise (expression)
  (with-expression (expression (or &rest subexprs))
    (let ((type :characters)
          (canonized nil))
      (dolist (sub subexprs)
        (when (typep sub '(or character string))
          (let* ((this (string sub))
                 (len (length this)))
            (unless (some (lambda (seen)
                            (not
                             ;; Check for "FOO" followed by "FOOBAR" -- the
                             ;; latter would never match, but it's an easy mistake to make.
                             (or (mismatch this seen :end1 (min (length seen) len))
                                 (warn "Prefix ~S before ~S in an ESRAP OR expression."
                                       seen this))))
                        canonized)
              (push this canonized))))
        (case type
          (:general)
          (:strings
           (unless (typep sub '(or character string))
             (setf type :general)))
          (:characters
           (unless (typep sub '(or character (string 1)))
             (if (typep sub 'string)
                 (setf type :strings)
                 (setf type :general))))))
      ;; FIXME: Optimize case-insensitive terminals as well.
      (ecase type
        (:characters
         ;; If every subexpression is a length 1 string, we can represent the whole
         ;; choise with a single string.
         (let ((choises (apply #'concatenate 'string canonized)))
           (named-lambda compiled-character-choise (text position end)
             (let ((c (and (< position end) (find (char text position) choises))))
               (if c
                   (make-result :position (+ 1 position)
                                :production (string c))
                   (make-failed-parse
                    :expression expression
                    :position position))))))
        (:strings
         ;; If every subexpression is a string, we can represent the whole choise
         ;; with a list of strings.
         (let ((choises (nreverse canonized)))
           (named-lambda compiled-character-choise (text position end)
             (dolist (choise choises
                      (make-failed-parse
                       :expression expression
                       :position position))
               (let ((len (length choise)))
                 (when (match-terminal-p choise len text position end t)
                   (return
                     (make-result :position (+ len position)
                                  :production choise))))))))
        (:general
         ;; In the general case, compile subexpressions and call.
         (let ((functions (mapcar #'compile-expression subexprs)))
             (named-lambda compiled-ordered-choise (text position end)
               (let (last-error)
                 (dolist (fun functions
                          (make-failed-parse
                           :expression expression
                           :position (if (and last-error
                                              (failed-parse-p last-error))
                                         (failed-parse-position last-error)
                                         position)
                           :detail last-error))
                   (let ((result (funcall fun text position end)))
                     (if (error-result-p result)
                         (when (or (and (not last-error)
                                        (or (inactive-rule-p result)
                                            (< position (failed-parse-position result))))
                                   (and last-error
                                        (failed-parse-p result)
                                        (or (inactive-rule-p last-error)
                                            (< (failed-parse-position last-error)
                                               (failed-parse-position result)))))
                           (setf last-error result))
                         (return result))))))))))))

;;; Negations

(defun exec-negation (fun expr text position end)
  (if (and (< position end)
           (error-result-p (funcall fun text position end)))
      (make-result
       :position (1+ position)
       :production (char text position))
      (make-failed-parse
       :expression expr
       :position position)))

(defun eval-negation (expression text position end)
  (with-expression (expression (not subexpr))
    (flet ((eval-sub (text position end)
             (eval-expression subexpr text position end)))
      (declare (dynamic-extent #'eval-sub))
      (exec-negation #'eval-sub expression text position end))))

(defun compile-negation (expression)
  (with-expression (expression (not subexpr))
    (let ((sub (compile-expression subexpr)))
      (named-lambda compiled-negation (text position end)
        (exec-negation sub expression text position end)))))

;;; Greedy repetitions

(defun eval-greedy-repetition (expression text position end)
  (funcall (compile-greedy-repetition expression) text position end))

(defun compile-greedy-repetition (expression)
  (with-expression (expression (* subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-repetition (text position end)
        (let ((results
               (loop for result = (funcall function text position end)
                     until (error-result-p result)
                     do (setf position (result-position result))
                     collect result)))
          (make-result
           :position position
           :production (mapcar #'result-production results)))))))

;;; Greedy positive repetitions

(defun eval-greedy-positive-repetition (expression text position end)
  (funcall (compile-greedy-positive-repetition expression)
           text position end))

(defun compile-greedy-positive-repetition (expression)
  (with-expression (expression (+ subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-positive-repetition (text position end)
        (let* ((last nil)
               (results
                (loop for result = (funcall function text position end)
                     until (error-result-p (setf last result))
                     do (setf position (result-position result))
                     collect result)))
          (if results
              (make-result
               :position position
               :production (mapcar #'result-production results))
              (make-failed-parse
               :position position
               :expression expression
               :detail last)))))))

;;; Optionals

(defun eval-optional (expression text position end)
  (with-expression (expression (? subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result :position position)
          result))))

(defun compile-optional (expression)
  (with-expression (expression (? subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-optional (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result :position position)
              result))))))

;;; Followed-by's

(defun eval-followed-by (expression text position end)
  (with-expression (expression (& subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position
           :production (result-production result))))))

(defun compile-followed-by (expression)
  (with-expression (expression (& subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (make-result
               :position position
               :production (result-production result))))))))

;;; Not followed-by's

(defun eval-not-followed-by (expression text position end)
  (with-expression (expression (! subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result
           :position position)
          (make-failed-parse
           :expression expression
           :position position)))))

(defun compile-not-followed-by (expression)
  (with-expression (expression (! subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-not-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result
               :position position)
              (make-failed-parse
               :expression expression
               :position position)))))))

;;; Semantic predicates

(defun eval-semantic-predicate (expression text position end)
  (with-expression (expression (t subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (let ((production (result-production result)))
            (if (funcall (symbol-function (car expression)) production)
                result
                (make-failed-parse
                 :position position
                 :expression expression)))))))

(defun compile-semantic-predicate (expression)
  (with-expression (expression (t subexpr))
    (let* ((function (compile-expression subexpr))
           (predicate (car expression))
           ;; KLUDGE: Calling via a variable symbol can be slow, and if we
           ;; grab the SYMBOL-FUNCTION here we will not see redefinitions.
           (semantic-function
            (if (eq (symbol-package predicate) (load-time-value (find-package :cl)))
                (symbol-function predicate)
                (compile nil `(lambda (x) (,predicate x))))))
      (named-lambda compiled-semantic-predicate (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (let ((production (result-production result)))
                (if (funcall semantic-function production)
                    result
                    (make-failed-parse
                     :position position
                     :expression expression)))))))))

;;; Character ranges

(defun exec-character-ranges (expression ranges text position end)
  (flet ((oops ()
           (make-failed-parse
            :expression expression
            :position position)))
    (if (< position end)
        (let ((char (char text position)))
          (if (loop for range in ranges
                    do (if (characterp range)
                           (when (char= range char)
                             (return t))
                           (when (char<= (first range) char (second range))
                             (return t))))
              (make-result
               :production char
               :position (1+ position))
              (oops)))
        (oops))))

(defun eval-character-ranges (expression text position end)
  (with-expression (expression (character-ranges &rest ranges))
    (exec-character-ranges expression ranges text position end)))

(defun compile-character-ranges (expression)
  (with-expression (expression (character-ranges &rest ranges))
    (named-lambda compiled-character-ranges (text position end)
      (exec-character-ranges expression ranges text position end))))

(defvar *indentation-hint-table* nil)

(defun hint-slime-indentation ()
  ;; See https://github.com/nikodemus/esrap/issues/24.
  (unless (member "SWANK-INDENTATION" *modules* :test #'string=)
    (return-from hint-slime-indentation))
  (when-let* ((swank (find-package :swank))
              (tables (find-symbol (string '#:*application-hints-tables*) swank))
              (table (make-hash-table :test #'eq)))
    (setf (gethash 'defrule table)
          '(4 4 &rest (&whole 2 &lambda &body)))
    (set tables (cons table (remove *indentation-hint-table* (symbol-value tables))))
    (setf *indentation-hint-table* table)
    t))

(hint-slime-indentation)
