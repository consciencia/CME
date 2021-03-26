;;; cme-company.el --- Overrides for company

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

;; Speed hack, semanticdb-fast-strip-find-results is a lot faster than
;; semanticdb-strip-find-results which caused company to lag for large
;; number of symbols.
(defun company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (if (and (fboundp 'semanticdb-minor-mode-p)
                        (semanticdb-minor-mode-p))
                   ;; Search the database & concatenate all matches together.
                   (semanticdb-fast-strip-find-results
                    (semanticdb-find-tags-for-completion prefix))
                 ;; Search just this file because there is no DB available.
                 (semantic-find-tags-for-completion
                  prefix (current-buffer))))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))

(defun cme-get-cpp-include-roots ()
  (let* ((proj (ede-toplevel))
         (root (semantic-symref-calculate-rootdir))
         (sys-includes (if proj (ede-system-include-path proj)))
         (global-includes semantic-dependency-system-include-path)
         (local-includes (ignore-errors
                           (loop for inc in (ede-include-path proj)
                                 collect (s-replace "//" "/"
                                                    (s-concat root
                                                              inc))))))
    (append sys-includes global-includes local-includes)))

;; Disable grep db for all company queries. We need just prototypes for it.
(advice-add #'company-semantic
            :around
            (lambda (oldfn &rest args)
              (cme-with-disabled-grep-db
                  (apply oldfn args))))

(advice-add #'company-complete-common
            :around
            (lambda (oldfn &rest args)
              (cme-with-disabled-grep-db
                  (apply oldfn args))))

;; Getter for project include roots.
(setq company-c-headers-path-system 'cme-get-cpp-include-roots)


(provide 'cme-company)
;;; cme-company.el ends here
