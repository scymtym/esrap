;;;; Copyright (c) 2017-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

;;; Packrat cache
;;;
;;; A cache mapping pairs (input position, rule name) to parse
;;; results. Its purpose is avoiding multiple identical applications
;;; of rules. This can improve performance and act as the foundation
;;; of a framework for handling left recursion.
;;;
;;; Since reads from and writes to this cache can be a performance
;;; bottleneck, the implementation tries to be as runtime and memory
;;; efficient as possible. A two-level scheme maps the pairs
;;; mentioned above to parse results:
;;; 1. an array maps the input position to a secondary structure
;;; 2. this structure maps the rule name to the cached parse results
;;;
;;; The interesting part about 1. is not allocating an array of the
;;; same size as the input upfront while keeping lookup performance
;;; reasonable. This trade-off is achieved using a "chunk cache".
;;;
;;; The difficulty with 2. is the variety of scenarios that have to be
;;; supported efficiently w.r.t. memory and runtime. To address this
;;; issue, the secondary structure uses one of multiple
;;; representations depending on the situation:
;;;
;;; + If only a mapping from a single rule to the associated parse
;;;   result has to be represented, a single cons cell is used.
;;;
;;; + For a small number of mapping entries, the number of entries and
;;;   an alist are stored to represent the mapping.
;;;
;;; + In the (uncommon) case that more than a few entries have to be
;;;   stored, a hash-table is used.
;;;
;;; Switches between representations happen when entries are added.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +packrat-hash-table-switch-point+ 16))

(declaim (ftype (function (symbol input-position chunk-cache) (values t &optional))
                cached))
(defun cached (symbol position cache)
  (declare (optimize speed))
  (let* ((chunk      (find-chunk position cache))
         (position-2 (ldb (byte +chunk-divisor+ 0) position))
         (cell       (when chunk
                       (aref chunk position-2))))
    (cond ((null cell)
           nil)
          ((not (consp cell))
           (values (gethash symbol cell)))
          ((not (consp (cdr cell)))
           (when (eq (car cell) symbol)
             (cdr cell)))
          (t
           (values (assoc-value (cdr cell) symbol :test #'eq))))))

(declaim (ftype (function (t symbol input-position chunk-cache) (values t &optional))
                (setf cached)))
(defun (setf cached) (result symbol position cache)
  (declare (optimize speed))
  (let* ((chunk      (ensure-chunk position cache))
         (position-2 (ldb (byte +chunk-divisor+ 0) position))
         (cell       (aref chunk position-2)))
    (cond

      ;; No entry => Create a singleton entry using one CONS.
      ((null cell)
       (setf (aref chunk position-2) (cons symbol result)))

      ;; Not a CONS => Has to be a hash-table. Store the result.
      ((not (consp cell))
       (setf (gethash symbol cell) result))

      ;; A singleton CONS => Maybe extend to a list of the form
      ;;
      ;;   (LENGTH . (KEY1 . RESULT1) (KEY2 . RESULT2) ...)
      ;;
      ;; where LENGTH is initially 2 after upgrading from a singleton
      ;; CONS.
      ((not (consp (cdr cell)))
       (if (eq (car cell) symbol)
           (setf (cdr cell) result)
           (setf (aref chunk position-2)
                 (cons 2 (acons symbol result (list cell))))))

      ;; A list with leading length as described above.
      (t
       (let ((count   (car cell)) ; note: faster than DESTRUCTURING-BIND
             (entries (cdr cell)))
         (declare (type (integer 0 #.+packrat-hash-table-switch-point+) count))
         (cond ;; When there is an entry for RESULT, update it.
               ((when-let ((entry (assoc symbol entries :test #'eq)))
                  (setf (cdr entry) result)
                  t))
               ;; When there are +PACKRAT-HASH-TABLE-SWITCH-POINT+
               ;; entries and we need another one, upgrade to
               ;; HASH-TABLE, then store the new entry.
               ((= count +packrat-hash-table-switch-point+)
                (let ((table (setf (aref chunk position-2)
                                   (alist-hash-table entries :test #'eq))))
                  (setf (gethash symbol table) result)))
               ;; When there are less than
               ;; +PACKRAT-HASH-TABLE-SWITCH-POINT+ entries and we
               ;; need another one, increase the counter and add an
               ;; entry.
               (t
                (setf (car cell) (1+ count))
                (setf (cdr cell) (acons symbol result entries))))))))
  result)
