;;; cme-install.el --- CME installation script

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

;; By evaluating this file, you install CME.

;;; Code:
(require 'url)


(let* ((root (expand-file-name user-emacs-directory))
       (cme-root (file-name-as-directory (concat root "CME")))
       (db-root (file-name-as-directory (concat root "semanticdb")))
       (root-url "https://raw.githubusercontent.com/consciencia/.emacs.d/master/lisp/CME/")
       (urls (loop for name in '("cme-analyze.el" "cme-c.el" "cme-company.el"
                                 "cme-complete.el" "cme-cpp-root.el"
                                 "cme-db-find.el" "cme-db.el" "cme-doc.el"
                                 "cme-ede-generic-proj.el" "cme-find.el"
                                 "cme-ia.el" "cme-index.el" "cme-k.el"
                                 "cme-misc.el" "cme-refs.el" "cme-search.el"
                                 "cme-semanticdb-grep.el" "cme-senator.el"
                                 "cme-symref.el" "cme-utils.el" "cme.el")
                   collect (concat root-url name)))
       (already-installed (file-directory-p cme-root))
       (should-install (if already-installed
                           (y-or-n-p "CME already installed, reinstall?")
                         t)))
  (when should-install
    (if already-installed
        (delete-directory cme-root t t))
    (make-directory cme-root)
    (make-directory db-root t)
    (loop for url in urls
          for dest = (concat cme-root
                             (car
                              (last
                               (split-string url "/" t))))
          do (progn
               (message "Downloading %s" url)
               (url-copy-file url dest)))))

;;; cme-install.el ends here
