;;; cme-misc.el --- Miscellaneous CME helper functions

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

(defun cme-enable-decorations-locally ()
  (when (and (featurep 'semantic)
             (semantic-active-p)
             ;; Ugly way how to check if mode is
             ;; enabled, unfortunately, variable
             ;; semantic-decoration-mode
             ;; can't be trusted. No idea why.
             (not (loop for o
                        in (with-current-buffer (current-buffer)
                             (semantic-overlays-in (point-min)
                                                   (point-max)))
                        if (semantic-decoration-p o)
                        return t)))
    (semantic-decoration-mode 1)))

(defun cme-diagnostic-visualizations ()
  (interactive)
  (let ((decision (ido-completing-read "Select thing to visualize: "
                                       '("Scope analyzer overhead"
                                         "Database size"
                                         "Tag complexity"
                                         "Tags by class"))))
    (cond
     ((equal decision "Scope analyzer overhead")
      (semantic-chart-analyzer))
     ((equal decision "Database size")
      (semantic-chart-database-size))
     ((equal decision "Tag complexity")
      (semantic-chart-tag-complexity))
     ((equal decision "Tags by class")
      (semantic-chart-tags-by-class)))))

(defun cme-show-parser-errors ()
  (interactive)
  (semantic-show-unmatched-syntax-mode 1)
  (message "Parser errors are shown as red underlines"))

(defun cme-hide-parser-errors ()
  (interactive)
  (semantic-show-unmatched-syntax-mode -1)
  (message "Parser errors are hidden"))

;; By default, which function will trigger reparsing of current file
;; by semantic, that is very slow so we will use shortcut.
(advice-add #'which-function
            :around
            (lambda (oldfn &rest args)
              (if (or (equal major-mode 'c-mode)
                      (equal major-mode 'c++-mode))
                  (let ((fun-name (cme-get-current-function-name)))
                    (if fun-name
                        (concat fun-name "()")
                      nil))
                (apply oldfn args))))

;; Overriden because old implementation is unable to close window by
;; hitting q.
(defun data-debug-new-buffer (name)
  "Create a new data-debug buffer with NAME."
  (let ((b (get-buffer-create name)))
    (pop-to-buffer b)
    (set-buffer b)
    (setq buffer-read-only nil) ; disable read-only
    (erase-buffer)
    (data-debug-mode)
    (local-set-key "q" 'delete-window)
    b))

(defun cme-cleanse-imenu-node-name (name)
  (cme-chain-forms
      (s-replace-regexp (pcre-to-elisp/cached
                         "(.*?)\\s+\\(def\\)")
                        "\\1"
                        name)
      (s-replace-regexp (pcre-to-elisp/cached
                         "(.*?)\\s+\\(class\\)")
                        "\\1")
      (s-replace-regexp (pcre-to-elisp/cached
                         "From: (.*)")
                        "")
      (s-replace-regexp (pcre-to-elisp/cached
                         "[\\(\\{]([^\\{]+)[\\)\\}]")
                        "\\1")))

(defun cme-flatten-imenu-root (prefix root)
  (if (and (consp root)
           (not (consp (cdr root))))
      `(,(cons (concat prefix
                       (if (> (length prefix) 0) "." "")
                       (cme-cleanse-imenu-node-name
                        (car root)))
               (cdr root)))
    (loop for sub-root in (cdr root)
          for new-prefix = (concat prefix
                                   (if (> (length prefix) 0) "." "")
                                   (cme-cleanse-imenu-node-name
                                    (car root)))
          append (cme-flatten-imenu-root new-prefix
                                         sub-root))))

(defun cme-imenu-records ()
  (interactive)
  (let* ((tree (save-excursion
                 (funcall imenu-create-index-function)))
         (records (loop for root in tree
                        append (if (and (consp root)
                                        (not (consp (cdr root))))
                                   `(,(cons (cme-cleanse-imenu-node-name
                                             (car root))
                                            (cdr root)))
                                 (cme-flatten-imenu-root ""
                                                         root)))))
    (if (interactive-p)
        (cme-with-simple-pop-up "*Flat Imenu Dump*"
          (setq kill-on-quit t)
          (loop for (name . _) in records
                do (insert name "\n")))
      records)))

(defun cme-goto-imenu-record (record)
  (let ((target (cdr record)))
    (cme-push-mark)
    (cond ((overlayp target)
           (goto-char (overlay-start target)))
          ((markerp target)
           (goto-char (marker-position target)))
          (t (goto-char target)))
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(defun cme-browse-local-tags (&optional symbol-list)
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (when (or (equal major-mode 'c-mode)
            (equal major-mode 'c++-mode))
    (semantic-fetch-tags))
  (let* ((index (cme-map-create))
         (records (cme-imenu-records))
         labels
         label)
    (loop for record in records
          do (progn (cme-map-set (car record)
                                 record
                                 index)
                    (push (car record) labels)))
    (setq label (ido-completing-read "Symbol? " labels))
    (cme-goto-imenu-record (cme-map-get label index))))


(provide 'cme-misc)
;;; cme-misc.el ends here
