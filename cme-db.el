;;; cme-db.el --- Overrides for semantic db

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

;; Redefined because default implementation refused to list databases
;; not attached to registered EDE projects or explicitly stated in
;; semanticdb-project-roots.
(defun semanticdb-current-database-list (&optional dir)
  "Return a list of databases associated with the current buffer.
If optional argument DIR is non-nil, then use DIR as the starting directory.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'.
Always append `semanticdb-project-system-databases' if
`semanticdb-search-system' is non-nil."
  (let ((root nil)			; found root directory
        (dbs nil)			; collected databases
        ;; All user roots + project dependencies.
        (roots (nconc semanticdb-project-roots
                      (let ((proj (ede-toplevel)))
                        (if (and proj
                                 (same-class-p proj
                                               'cme-generic-proj))
                            (ede-source-roots proj)))))
        (dir (file-truename (or dir default-directory))))
    ;; Find the root based on project functions.
    (setq root (run-hook-with-args-until-success
                'semanticdb-project-root-functions
                dir))
    (if root
        (setq root (file-truename root))
      ;; Else, Find roots based on strings
      (while roots
        (let ((r (file-truename (car roots))))
          (if (string-match (concat "^" (regexp-quote r)) dir)
              (setq root r)))
        (setq roots (cdr roots))))
    ;; If no roots are found, use this directory.
    (unless root (setq root dir))
    ;; Find databases based on the root directory.
    (when root
      ;; The rootlist allows the root functions to possibly
      ;; return several roots which are in different areas but
      ;; all apart of the same system.
      (let ((regexp (concat "^" (regexp-quote root)))
            (adb semanticdb-database-list)) ; all databases
        (while adb
          ;; I don't like this part, but close enough.
          (if (and (slot-boundp (car adb) 'reference-directory)
                   (string-match regexp (oref (car adb) reference-directory)))
              (setq dbs (cons (car adb) dbs)))
          (setq adb (cdr adb)))))
    ;; Add in system databases
    (when semanticdb-search-system-databases
      (setq dbs (nconc dbs semanticdb-project-system-databases)))
    ;; Return
    dbs))


(provide 'cme-db)
;;; cme-db.el ends here
