;;; cme-k.el --- CME k parser combinator

;; Copyright (C) 2021 Consciencia

;; Author: Consciencia <consciencia@protonmail.com>
;; Version: 1.0.0
;; Keywords: c c++ cme cedet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; :(

;;; Code:

(defmacro kdefparser (name arglist &optional docstring &rest body)
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist
            ,(if (and docstring (stringp docstring))
                 docstring
               "")
            (eval (progn ,(if (not (stringp docstring))
                              docstring)
                         ,@body)))
          (let ((byte-compile-log-warning-function
                 (lambda (&rest args))))
            (byte-compile ',name))))

(defmacro kdeftest (parser input exp-output)
  (declare (indent 1) (debug t))
  `(let (status output)
     (setq output (k-run ,parser
                         ',input))
     (setq status
           (equal output
                  ',exp-output))
     (if (not status)
         (message "Expected '%s' got '%s'"
                  ',exp-output
                  output))
     (assert status)))

(defmacro k| (&rest clauses)
  (declare (indent 0) (debug t))
  (setq clauses
        (loop for clause in clauses
              collect `(funcall ,clause tokens)))
  `(lambda (tokens)
     (or ,@clauses)))

(defmacro k& (&rest clauses)
  (declare (indent 0) (debug t))
  (let ((curr-tokens (gensym))
        (can-continue (gensym))
        (result (gensym))
        (results (gensym)))
    (setq clauses
          (loop for clause in clauses
                collect `(when ,can-continue
                           (setq ,result
                                 (funcall ,clause ,curr-tokens))
                           (if ,result
                               (progn
                                 (push (car ,result) ,results)
                                 (setq ,curr-tokens (cdr ,result)))
                             (progn
                               (setq ,can-continue nil
                                     ,results nil))))))
    `(lambda (tokens)
       (let ((,curr-tokens tokens)
             (,can-continue t)
             (,result nil)
             (,results nil))
         ,@clauses
         (when ,results
           (setq ,results (nreverse ,results))
           (cons ,results ,curr-tokens))))))

(defmacro k? (parser)
  (declare (indent 0) (debug t))
  `(lambda (tokens)
     (let* ((result (funcall ,parser tokens)))
       (if result
           result
         (cons nil tokens)))))

(defmacro k$ (source &rest forms)
  (declare (indent 1) (debug t))
  `(lambda (tokens)
     (let* ((result (funcall ,source tokens))
            ($ (car result))
            (new-tokens (cdr result)))
       (when result
         ,@forms
         (cons $ new-tokens)))))

(defmacro k$-n (idx)
  (declare (indent 0) (debug t))
  `(setq $ (nth ,idx $)))

(defmacro k$-flatten ()
  (declare (indent 0) (debug t))
  '(setq $
         (append (list (car $))
                 (cadr $))))

(kdefparser k* (parser)
  `(k? (k$ (k& ,parser
               (k* ,parser))
           (k$-flatten))))

(defmacro k+ (parser)
  (declare (indent 0) (debug t))
  `(k$ (k& ,parser (k* ,parser))
       (k$-flatten)))

(defmacro k-sep-by (separator parser)
  (declare (indent 1) (debug t))
  `(k$ (k& ,parser
           (k* (k$ (k& ,separator
                       ,parser)
                 (k$-n 1))))
     (k$-flatten)))

(defmacro k-list (begin end separator parser)
  `(k$ (k& ,begin
           (k-sep-by ,separator ,parser)
           ,end)
       (k$-n 1)))

(defmacro k-is (kind &optional value)
  (declare (indent 0) (debug t))
  `(lambda (tokens)
     (when (and tokens
                (equal (caar tokens)
                       ',kind))
       ,(if value
            `(when (equal ,value
                          (buffer-substring-no-properties
                           (cadar tokens)
                           (cddar tokens)))
               (cons (cdar tokens)
                     (cdr tokens)))
          '(cons (cdar tokens)
                 (cdr tokens))))))

(defmacro k-is-end ()
  (declare (indent 0) (debug t))
  `(lambda (tokens)
     (when (not tokens)
       (cons t nil))))

(defmacro k-run (parser input)
  (declare (indent 1) (debug t))
  `(car-safe (funcall ,parser ,input)))

(eval-when-compile
  (kdeftest (k$ (k& (k-is foo)
                    (k-is bar)
                    (k| (k-is baz)
                        (k-is-end)))
                (k$-n 2))
            ((foo . "foo")
             (bar . "bar")
             (baz . "baz"))
            "baz")
  (kdeftest (k$ (k& (k-is foo)
                    (k-is bar)
                    (k| (k-is baz)
                        (k-is-end)))
                (k$-n 2))
            ((foo . "foo")
             (bar . "bar")
             (bar . "baz"))
            nil)
  (kdeftest (k* (k-is foo))
            ((foo . "foo")
             (foo . "bar")
             (foo . "baz"))
            ("foo" "bar" "baz"))
  (kdeftest (k* (k-is foo))
            ((foo . "foo")
             (foo . "bar")
             (bar . "baz"))
            ("foo" "bar"))
  (kdeftest (k* (k-is foo))
            ((baz . "foo")
             (bar . "bar")
             (foo . "baz"))
            nil)
  (kdeftest (k+ (k-is foo))
            ((foo . "foo")
             (foo . "bar")
             (foo . "baz"))
            ("foo" "bar" "baz"))
  (kdeftest (k+ (k-is foo))
            ((foo . "foo")
             (foo . "bar")
             (foo . "baz"))
            ("foo" "bar" "baz"))
  (kdeftest (k-sep-by (k-is bar)
                      (k-is foo))
            ((foo . "foo1")
             (bar . "bar1")
             (foo . "foo2")
             (bar . "bar2")
             (foo . "foo3"))
            ("foo1" "foo2" "foo3")))


(provide 'cme-k)
;;; cme-k.el ends here
