;;; cme-semanticdb-grep.el --- CME virtual DB based on grep and findstr

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

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt))


;;;###autoload
(defun cme-db-enable-grep (&optional noerror)
  "Enable the use of the grep back end for all files in C/C++.
This will add an instance of a grep database to each buffer in a
grep supported hierarchy."
  (dolist (mode '(c-mode c++-mode))
    (let ((ih (mode-local-value mode 'semantic-init-mode-hook)))
      (eval `(setq-mode-local
              ,mode semantic-init-mode-hook
              (cons 'cme-db-enable-grep-hook ih)))))
  t)

(defun cme-db-enable-grep-in-buffer (&optional dont-err-if-not-available)
  "Enable a grep database in the current buffer.
When grep is not available for this directory, display a message
if optional DONT-ERR-IF-NOT-AVAILABLE is non-nil; else throw an error."
  (interactive "P")
  (if (file-exists-p (semantic-symref-calculate-rootdir))
      (setq
       ;; Add to the system database list.
       semanticdb-project-system-databases
       (cons (cme-db-project-database-grep "Grep")
             semanticdb-project-system-databases)
       ;; Apply the throttle.
       semanticdb-find-default-throttle
       (append semanticdb-find-default-throttle '(omniscience)))
    (if dont-err-if-not-available
        nil ;; (message "No grep support in %s" default-directory)
      (error "No grep support in %s" default-directory))))

(defun cme-db-enable-grep-hook ()
  "Add support for grep in the current buffer via `semantic-init-hook'."
  (cme-db-enable-grep-in-buffer t))

(defclass cme-db-project-database-grep
  ;; @todo - convert to one DB per directory.
  (semanticdb-project-database eieio-instance-tracker) ()
  "Database representing a grep virtual tags file.")

;;; Classes:
(defclass cme-db-table-grep (semanticdb-search-results-table)
  ((major-mode :initform nil))
  "A table for returning search results from grep.")

(defmethod semanticdb-equivalent-mode ((table cme-db-table-grep)
                                       &optional buffer)
  "Return t, pretend that this table's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  ;; @todo - hack alert!
  t)

(defmethod object-print ((obj cme-db-table-grep) &rest strings)
  "Pretty printer extension for `semanticdb-table-cscope'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj (cons " (proxy)" strings)))

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables
  ((obj cme-db-project-database-grep))
  "For a grep database, there are no explicit tables.
For each file hit, get the traditional semantic table from that file."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (cme-db-table-grep "Grep Search Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)))
  (call-next-method))

(defmethod semanticdb-file-table ((obj cme-db-project-database-grep)
                                  filename)
  "From OBJ, return FILENAME's associated table object."
  ;; We pass in "don't load".  I wonder if we need to avoid that or not?
  (car (semanticdb-get-database-tables obj)))

;;; Search Overrides
;;
;; Only NAME based searches work with CSCOPE as that is all it tracks.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table cme-db-table-grep) name &optional tags)
  "Find all tags named NAME in TABLE. Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; Call out to grep for some results.
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-name name 'project)))
      (when result
        (semantic-symref-result-get-tags result t)))))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table cme-db-table-grep) regex &optional tags)
  "Find all tags with name matching REGEXP in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-regexp regex 'project)))
      (when result
        (semantic-symref-result-get-tags result t)))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table cme-db-table-grep) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'grep)
           (result (semantic-symref-find-tags-by-completion prefix 'project))
           (faketags nil))
      (when result
        (dolist (T (oref result :hit-text))
          ;; We should look up each tag one at a time, but I'm lazy!
          ;; Doing this may be good enough.
          (setq faketags (cons
                          (semantic-tag T 'function :faux t)
                          faketags)))
        faketags))))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table cme-db-table-grep) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table cme-db-table-grep) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table cme-db-table-grep) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for cscope."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;; Fix for too strict symref excludes for C and C++.
(setq semantic-symref-filepattern-alist
      (loop for entry in semantic-symref-filepattern-alist
            for label = (car entry)
            if (or (equal label 'c-mode)
                   (equal label 'c++-mode))
            collect `(,label "*.[chCH]" "*.[ch]pp" "*.cc" "*.hh")
            else
            collect entry))

(defun cme-gen-win-grep-emulation-cmd (searchfor)
  (when (eq system-type 'windows-nt)
    (cme-replace-all
     `(,(cons "$dirlist"
              (s-join ";"
                      (cons (semantic-symref-calculate-rootdir)
                            (let ((proj (ede-toplevel)))
                              (if (and proj
                                       (same-class-p proj
                                                     'cme-generic-proj))
                                  (ede-source-roots proj))))))
       ,(cons "$searchfor" searchfor)
       ,(cons "$filters" "*.c *.cpp *.cxx *.h *.hpp *.hxx"))
     "findstr /s /n /d:$dirlist \"$searchfor\" $filters")))

(defun cme-cleanse-findstr-output (buff)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (pcre-to-elisp/cached
                   (concat "^(?:"
                           "\\s+([\\w\\\\/\\._\\-:]+):"
                           "|"
                           "([^\\s][\\w\\\\/\\._\\-:]+)(:\\d+:)([^\n]*)"
                           ")\n")))
          (prefix nil))
      (while (search-forward-regexp regexp nil t)
        (let ((root-path (match-string 1))
              (hit-path (match-string 2))
              (hit-line (match-string 3))
              (hit-payload (match-string 4))
              )
          (if root-path
              (progn
                (setq prefix (s-replace-regexp "\\\\" "/" root-path))
                (replace-match ""))
            (progn
              (if (not prefix)
                  (error (concat "Error during normalization of "
                                 "findstr result at line %s "
                                 "(missing prefix)!")
                         (line-number-at-pos (match-beginning 0))))
              (replace-match (concat prefix
                                     (s-replace-regexp (regexp-quote "\\")
                                                       "/"
                                                       hit-path)
                                     hit-line
                                     (s-replace-regexp (regexp-quote "\\")
                                                       "/"
                                                       hit-payload)
                                     "\n")))))))))

(defun cme-filter-raw-grep-output (hits searchfor)
  (let ((deep-search #'cme-search-file-no-includes)
        (files (cme-map-create))
        (result nil))
    (loop for (line . path) in hits
          do (cme-map-set path t files))
    (cme-map-everything (lambda (filename dummy)
                          (setq result
                                (append result
                                        (loop for tag
                                              in (funcall deep-search
                                                          searchfor
                                                          filename)
                                              collect (cons tag filename)))))
                        files)
    (setq result
          (loop for (tag . file) in result
                collect (cons (with-current-buffer (find-file-noselect file)
                                (line-number-at-pos
                                 (semantic-tag-start tag)))
                              file)))
    result))

(setq *cme-grep-db-enabled* t)
(defmacro cme-with-disabled-grep-db (&rest forms)
  (declare (indent 99) (debug t))
  `(let ((*cme-grep-db-enabled* nil))
     ,@forms))

(defmacro cme-with-enabled-grep-db (&rest forms)
  (declare (indent 99) (debug t))
  `(let ((*cme-grep-db-enabled* t))
     ,@forms))

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  (when *cme-grep-db-enabled*
    ;; Grep doesn't support some types of searches.
    (when (not (memq (oref tool searchtype)
                     '(symbol regexp tagname)))
      (error "Symref impl GREP does not support searchtype of '%s' for '%s'!"
             (oref tool searchtype)
             (oref tool searchfor)))
    (message "Grepping (%s) for '%s'."
             (oref tool searchtype)
             (oref tool searchfor))
    ;; Find the root of the project, and do a find-grep...
    (let* (;; Find the file patterns to use.
           (rootdir (semantic-symref-calculate-rootdir))
           (filepatterns (semantic-symref-derive-find-filepatterns))
           (filepattern (mapconcat #'shell-quote-argument filepatterns " "))
           ;; Grep based flags.
           (grepflags (cond ((eq (oref tool resulttype) 'file)
                             "-l ")
                            ((eq (oref tool searchtype) 'regexp)
                             "-nE ")
                            ((eq (oref tool searchtype) 'symbol)
                             (if (> emacs-major-version 25)
                                 ;; Evil hack for workarounding strange
                                 ;; grep issue in older emacs.
                                 "-n "
                               "-nw "))
                            ((eq (oref tool searchtype) 'tagname)
                             ;; Evil hack for workarounding strange
                             ;; grep issue in older emacs.
                             (if (> emacs-major-version 25)
                                 "-n "
                               "-nw "))
                            (t "-n ")))
           (greppat (cond ((eq (oref tool searchtype) 'regexp)
                           (oref tool searchfor))
                          (t
                           ;; Can't use the word boundaries: Grep
                           ;; doesn't always agree with the language
                           ;; syntax on those.
                           ;;
                           ;; Some error in grep command
                           ;; preprocessing somewhere.
                           (if (> emacs-major-version 25)
                               (format "\\(^\\|\\W\\)%s\\(\\W\\|$\\)"
                                       (oref tool searchfor))
                             (oref tool searchfor)))))
           ;; Misc
           (b (get-buffer-create "*Semantic SymRef*"))
           (ans nil))
      (with-current-buffer b
        (erase-buffer)
        (setq default-directory rootdir)
        (let ((cmd (semantic-symref-grep-use-template rootdir
                                                      filepattern
                                                      grepflags
                                                      greppat))
              ;; Ugly hack for windows. Grep combined with find is
              ;; awfully slow on windows so we must resort to native
              ;; alternative which is fast.
              (wincmd (cme-gen-win-grep-emulation-cmd (oref tool
                                                            searchfor))))
          (if wincmd
              (progn
                (call-process semantic-symref-grep-shell nil b nil
                              shell-command-switch wincmd)
                (cme-cleanse-findstr-output b))
            (progn
              (call-process semantic-symref-grep-shell nil b nil
                            shell-command-switch cmd)))))
      (setq ans (semantic-symref-parse-tool-output tool b))
      ;; Special handling for search type tagname.
      ;; Semantic expect only positions of tags will be provided so we
      ;; must filter raw output from grep using semantic parsing
      ;; infrastructure.
      (message "Grep found %s hits."
               (length ans))
      (if (equal (oref tool searchtype) 'tagname)
          (cme-filter-raw-grep-output ans (oref tool searchfor))
        ans))))

;; Disable grepping in idle summary mode. We need just prototypes
;; there, no need to scan whole project by grep.
(advice-add 'semantic-idle-summary-idle-function
            :around (lambda (oldfn)
                      (cme-with-disabled-grep-db
                          (funcall oldfn))))


(provide 'cme-semanticdb-grep)
;;; cme-semanticdb-grep.el ends here
