;;; setq-trap.el --- debug on entry to setq when specific symbols are modified

;;; Copyright (C) 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions
;;; Status: unreleased, not yet usable in emacs 18
;;; Created: 1994-08-18

;;; $Id: setq-trap.el,v 1.2 2013/07/02 19:02:53 friedman Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; For this to work, all the code in which you want to debug setq calls
;;; must not be byte-compiled.  Even if you modified the byte-compiler not
;;; to generate code using the setq bytecode, any previously compiled code
;;; would still use the setq bytecode and bypass any redefinition in lisp.
;;; Anyway, isn't debugging byte-compiled code inherently more difficult?

;;; TODO:
;;;   * Right now this only invokes the debugger if the value list for a
;;;     symbol is empty or when the value you tried to set it to is in the
;;;     value list.  There should be a way to specify an exclusive list of
;;;     values for which the debugger *shouldn't* be invoked, but only be
;;;     invoked when trying to set the symbol to any other value.
;;;   * Clean up calls to `member' and `delete' so this works in emacs 18.
;;;   * Write commands to reset `setq' to subr and/or switch to macro
;;;     definition when desired.

;;; Code:

(require 'backquote)

;; TODO: document this variable
(defvar setq-trap-debug-alist '())


;; This needs to be a macro to prevent premature evaluation of arguments
(defmacro setq-trap (&rest args)
  (let ((args1 (make-symbol "args1"))
        (sym   (make-symbol "sym"))
        (val   (make-symbol "val"))
        (subr  (get 'setq 'subr-definition)))
    `(let ((,args1 ',args)
           ,sym
           ,val)
       (while ,args1
         (,subr ,sym (car ,args1))
         (,subr ,val (eval (car (cdr ,args1))))
         (,subr ,args1 (cdr (cdr ,args1)))

         (and (setq-trap-p ,sym ,val)
              (funcall debugger nil
                       (format "attempt to set `%s' to: %s"
                               ,sym ,val)))
         (set ,sym ,val))
       ,val)))

;; TODO: tests --- delete this when finished
;(setq setq-trap-debug-alist nil)
;(setq-trap-add-entry 'foo 'debug-foo)
;(setq-trap-add-entry 'bar 'debug-bar)
;(setq-trap foo (1+ 1))
;(setq-trap foo 'debug-foo)

(defun setq-trap-p (sym &optional value)
  (let* ((entry (assq sym setq-trap-debug-alist))
         (value-list (cdr entry)))
    (and entry
         (cond
          ((or (null value-list)))
          ((member value value-list))
          (t nil)))))


;; TODO: document this and make interactive
(defun setq-trap-add-entry (sym &rest values)
  (let ((entry (assq sym setq-trap-debug-alist)))
    (and (null entry)
         (progn
           (setq setq-trap-debug-alist (cons (cons sym '())
                                             setq-trap-debug-alist))
           (setq entry (car setq-trap-debug-alist))))
    (while values
      (or (member (car values) (cdr entry))
          (progn
            (setcdr entry
                    (cons (car values) (cdr entry)))))
      (setq values (cdr values))))
  setq-trap-debug-alist)

;; TODO: document this and make interactive
(defun setq-trap-remove-entry (sym &rest values)
  (interactive)
  (let ((entry (assq sym setq-trap-debug-alist)))
    (cond
     ((null values)
      (setq setq-trap-debug-alist
            (delq entry setq-trap-debug-alist)))
     (t
      (while values
        (setcdr entry (delete (car values) (cdr entry)))
        (setq values (cdr values))))))
  setq-trap-debug-alist)


(provide 'setq-trap)

;; Save setq subr on the `subr-definition' property of setq symbol.
(let ((def (get 'setq 'subr-definition)))
  (or (subrp def)
      (put 'setq 'subr-definition (symbol-function 'setq))))

(if (fboundp 'defalias)
    (defalias 'setq 'setq-trap)
  (fset 'setq 'setq-trap))

;;; setq-trap.el ends here
