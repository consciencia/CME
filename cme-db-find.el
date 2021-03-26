;;; cme-db-find.el --- Overrides for semantic db find

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

;; Hack done in order to be able to complete enums. By default, no deep
;; search was done in contextless searches so I introduced partial deep
;; search only for enums.
(cl-defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-abstract-table)
                                                          prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix
                                     (cme-flatten-enum-tags-table
                                      (or tags
                                          (semanticdb-get-tags table)))))

;; Redefined because default implementation does not acquire databases
;; of project dependencies.
(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
         (cond ((null path)
                semanticdb-current-database)
               ((semanticdb-table-p path)
                (oref path parent-db))
               (t (let ((tt (semantic-something-to-tag-table path)))
                    (if tt
                        ;; @todo - What does this DO ??!?!
                        ;; NOTE: Looks like some esoteric hack.
                        (with-current-buffer (semantic-tag-buffer (car tt))
                          semanticdb-current-database)
                      semanticdb-current-database))))))
    (apply
     #'nconc
     (mapcar
      (lambda (db)
        (let ((tabs (semanticdb-get-database-tables db))
              (ret nil))
          ;; Only return tables of the same language (major-mode)
          ;; as the current search environment.
          (while tabs
            (semantic-throw-on-input 'translate-path-brutish)
            (if (semanticdb-equivalent-mode-for-search (car tabs)
                                                       (current-buffer))
                (setq ret (cons (car tabs) ret)))
            (setq tabs (cdr tabs)))
          ret))
      (cme-delete-dups-eq
       (cme-append-new-backbone
        (semanticdb-current-database-list
         (if basedb
             (oref basedb reference-directory)
           default-directory))
        (apply #'cme-append-new-backbone
               (loop for root in (let ((proj (ede-toplevel)))
                                   (if (and proj
                                            (same-class-p proj
                                                          'cme-generic-proj))
                                       (ede-source-roots proj)))
                     collect (semanticdb-current-database-list root)))))))))


(provide 'cme-db-find)
;;; cme-db-find.el ends here
