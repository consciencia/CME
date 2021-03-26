;;; cme-index.el --- CME indexer module

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

(defun cme-index-directory (root &optional logger selection-regex)
  (interactive (list (ido-read-directory-name "Select directory: ")))
  (when (not selection-regex)
    (setq selection-regex
          (pcre-to-elisp/cached ".*\\.(?:c|cpp|h|hpp|cxx|hxx)$")))
  (when (not logger)
    (setq logger (lambda (_))))
  (let ((root (file-name-as-directory (file-truename root)))
        (files (directory-files root t)))
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (semanticdb-save-all-db)
    (loop for file in files
          do (if (not (file-accessible-directory-p file))
                 (when (string-match-p selection-regex file)
                   (if (ignore-errors
                         (semanticdb-file-table-object file))
                       (funcall logger (format "Parsing %s [OK]" file))
                     (funcall logger (format "Parsing %s [ERROR]" file))))
               (progn (semanticdb-save-all-db)
                      (cme-index-directory file
                                           logger
                                           selection-regex))))))

(defun cme-index-current-project ()
  (interactive)
  (let* ((proj (ede-toplevel))
         (roots (append (list (semantic-symref-calculate-rootdir))
                        (if (and proj
                                 (same-class-p proj 'cme-generic-proj))
                            (ede-source-roots proj))
                        (if proj
                            (ede-system-include-path proj))
                        (if (and semantic-dependency-system-include-path
                                 (yes-or-no-p (concat "Do you want to also "
                                                      "index global system "
                                                      "include paths? "
                                                      "(warning: lot of "
                                                      "additional files to be "
                                                      "parsed)")))
                            semantic-dependency-system-include-path))))
    (if roots
        (progn
          (message "Indexing: %s" roots)
          (let* ((buffname "*Semantic Indexer Log*")
                 (buff (or (cme-get-buffer buffname)
                           (generate-new-buffer buffname)))
                 (logger (lambda (msg)
                           (insert msg "\n")
                           (goto-char (point-max))
                           (redisplay t)))
                 duration)
            (switch-to-buffer buff)
            (local-set-key "q" `(lambda ()
                                  (interactive)
                                  (kill-buffer ,buffname)))
            (erase-buffer)
            (redisplay t)
            (setq duration
                  (cdr (cme-with-measure-time
                           (loop for root in roots
                                 do (cme-index-directory root logger))
                           (insert "\nDONE after "))))
            (insert (format "%.0f" (/ duration 60))
                    "min")
            (read-only-mode t)))
      (error "Failed to find roots, are you in project?"))))

(setq *cme-parse-table-queue* nil)
(defun cme-parser-executor ()
  (when *cme-parse-table-queue*
    (semanticdb-refresh-table (car *cme-parse-table-queue*) t)
    (setq *cme-parse-table-queue* (cdr *cme-parse-table-queue*))))

(defun cme-refresh-table-in-future (table)
  (push table *cme-parse-table-queue*))

(setq *cme-loaded-projects* (cme-map-create))
(defun cme-load-all-project-dbs ()
  (interactive)
  (when (not (cme-at *cme-loaded-projects*
                     (semantic-symref-calculate-rootdir)))
    (let* ((proj-roots (cons (semantic-symref-calculate-rootdir)
                             (let ((proj (ede-toplevel)))
                               (if (and proj
                                        (same-class-p proj
                                                      'cme-generic-proj))
                                   (ede-source-roots proj)))))
           (store-path (file-name-as-directory
                        semanticdb-default-save-directory))
           (check-regexp (concat "^\\(?:"
                                 (s-join "\\|"
                                         (loop for proj-root in proj-roots
                                               collect
                                               (regexp-quote proj-root)))
                                 "\\)"))

           (raw-cache-paths (directory-files store-path))
           (cache-paths (loop for raw-cache in raw-cache-paths
                              for cache = (cedet-file-name-to-directory-name
                                           raw-cache)
                              if (s-match check-regexp cache)
                              collect raw-cache)))
      (loop for cache in cache-paths
            for full-path = (expand-file-name (concat store-path cache))
            for decoded-cache = (cedet-file-name-to-directory-name cache)
            if (not (semanticdb-file-loaded-p full-path))
            do (progn
                 (message "Loading: %s" full-path)
                 (let* ((db (semanticdb-load-database full-path))
                        (tables (if db (semanticdb-get-database-tables db))))
                   (when db
                     (oset db
                           reference-directory
                           (s-replace-regexp "semantic.cache"
                                             ""
                                             decoded-cache)))
                   (loop for table in tables
                         if (and (file-exists-p
                                  (concat (oref db reference-directory)
                                          (oref table file)))
                                 (semanticdb-needs-refresh-p table))
                         do (cme-refresh-table-in-future table))))))
    (cme-map-set (semantic-symref-calculate-rootdir)
                 t
                 *cme-loaded-projects*)))

(defun cme-get-current-db ()
  (semanticdb-directory-loaded-p
   (file-name-directory buffer-file-name)))

(defun cme-get-current-table ()
  (let ((db (cme-get-current-db)))
    (when db
      (semanticdb-file-table db buffer-file-name))))

(defun cme-dump-db-refpaths ()
  (interactive)
  (cme-with-simple-pop-up "*Semantic Db Ref Paths*"
    (setq kill-on-quit t)
    (loop for db in semanticdb-database-list
          collect (insert (or (ignore-errors
                                (format " PATH: %s"
                                        (oref db
                                              reference-directory)))
                              (format "CLASS: %s"
                                      (eieio-object-class-name db)))
                          "\n"))))

(semantic-idle-scheduler-add #'cme-parser-executor)


(provide 'cme-index)
;;; cme-index.el ends here
