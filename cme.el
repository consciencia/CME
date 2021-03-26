;;; cme.el --- Main CME entry point

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

(setq *cme-packages-refreshed* nil)
(loop for package in '(company company-c-headers s pcre2el)
      if (not (package-installed-p package))
      do (progn
           (when (not *cme-packages-refreshed*)
             (package-refresh-contents)
             (setq *cme-packages-refreshed* t))
           (package-install package)))

(require 's)
(require 'cl)
(require 'json)
(require 'pcre2el)
(require 'cc-mode)
(require 'data-debug)
(require 'seq)
(require 'mode-local)
(require 'ido)
(require 'company)
(require 'company-semantic)
(require 'company-c-headers)
(require 'ede)
(require 'ede/cpp-root)
(require 'semantic/db)
(require 'semantic)
(require 'semantic/idle)
(require 'semantic/symref)
(require 'semantic/symref/grep)
(require 'semantic/symref/list)
(require 'semantic/symref/filter)
(require 'semantic/doc)
(require 'semantic/ia)
(require 'semantic/chart)
(require 'semantic/ctxt)
(require 'semantic/bovine/c)
(require 'semantic/analyze)
(require 'semantic/dep)
(require 'srecode)
(require 'srecode/fields)
(require 'cme-utils)
(require 'cme-semanticdb-grep)
(require 'cme-ede-generic-proj)
(require 'cme-cpp-root)
(require 'cme-misc)
(require 'cme-company)
(require 'cme-db-find)
(require 'cme-db)
(require 'cme-refs)
(require 'cme-find)
(require 'cme-ia)
(require 'cme-symref)
(require 'cme-search)
(require 'cme-index)
(require 'cme-doc)
(require 'cme-complete)
(require 'cme-analyze)
(require 'cme-senator)
(require 'cme-c)


(defun cme-init (&rest args)
  (interactive)
  (setq semantic-new-buffer-setup-functions
        (loop for e in semantic-new-buffer-setup-functions
              if (not (or (equal (car e) 'python-mode)
                          (equal (car e) 'js-mode)
                          (equal (car e) 'html-mode)))
              collect e))
  (setq-default semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-summarize
                semantic-idle-work-parse-neighboring-files-flag nil
                semantic-idle-work-update-headers-flag nil
                semantic-complete-inline-analyzer-displayer-class 'semantic-displayer-tooltip
                semantic-edits-verbose-flag t
                ;; Use (semantic-symref-detect-symref-tool) for autodetecting
                ;; symref tool. Currently, we dont want to use that. Grep is
                ;; by far the best option If you have modern CPU.
                ;; Anyway, global is kinda buggy and cscope is not very used.
                semantic-symref-tool 'grep
                senator-step-at-tag-classes nil
                senator-step-at-start-end-tag-classes '(function)
                speedbar-use-images nil
                speedbar-use-imenu-flag t)
  (global-semanticdb-minor-mode t)
  (global-semantic-idle-scheduler-mode t)
  (global-semantic-mru-bookmark-mode t)
  (global-semantic-highlight-edits-mode t)
  (global-semantic-idle-summary-mode t)
  (global-semantic-highlight-func-mode t)
  ;; Disabled because decoration mode awfully slows down parsing of
  ;; larger chunks of unparsed files.
  ;; Also, when turning on, it will iterate all buffers and turns itself on
  ;; in every buffer managed by semantic which is very slow when many
  ;; buffers are opened.
  ;; Because of that, its not good idea to switch it off and on when
  ;; parsing.
  ;; Below, it is enabled on per buffer basis using idle timer.
  (global-semantic-decoration-mode -1)
  (global-semantic-idle-breadcrumbs-mode t)
  ;; idle breadcrumbs are better, but are in conflict
  ;; with stickyfunc, so its disabled.
  (global-semantic-stickyfunc-mode -1)
  ;; We dont want this globally, it kinds of annoys in some situations.
  (global-semantic-show-unmatched-syntax-mode -1)
  ;; Activate decoration mode for current buffer only after some seconds
  ;; of idling. This way, no slow down due to decorations happen during
  ;; bulk file parsing.
  (run-with-idle-timer 2 t 'cme-enable-decorations-locally)
  (cme-db-enable-grep)
  (global-ede-mode 1)
  (semantic-mode 1)
  ;; Support for elisp slowes things down and I dont have time to find
  ;; out why. Another thing is that output from semantic is used only
  ;; for imenu so its not critical.
  ;;
  ;; (add-to-list 'semantic-new-buffer-setup-functions
  ;;              '(emacs-lisp-mode . semantic-default-elisp-setup))
  (add-to-list 'semantic-inhibit-functions
               (lambda ()
                 (not (or (equal major-mode 'c-mode)
                          (equal major-mode 'c++-mode)
                          ;; (equal major-mode 'emacs-lisp-mode)
                          ))))
  (advice-add 'save-buffer :after
              (lambda (&rest args)
                (if (or (equal major-mode 'c-mode)
                        (equal major-mode 'c++-mode)
                        ;; (equal major-mode 'emacs-lisp-mode)
                        )
                    (save-mark-and-excursion
                      (cme-reparse-buffer)))))
  ;; Overriden in order to make CEDET not crash due to invalid usage
  ;; of file-exists-p.
  (advice-add #'file-exists-p
              :around
              (lambda (oldfn filename)
                (if filename
                    (funcall oldfn filename))))
  (loop for mode in '(c-mode-hook c++-mode-hook)
        do (add-hook mode
                     (lambda ()
                       (set (make-local-variable 'company-backends)
                            '((company-c-headers
                               company-semantic
                               company-files)))
                       (local-set-key (kbd ".")
                                      (lambda ()
                                        (interactive)
                                        (if (cme-is-prefix-pointer-p)
                                            (insert "->")
                                          (insert ".")))))))
  (when (and (plist-member args :configure-keys)
             (plist-get args :configure-keys))
    (loop for mode in '(c-mode-hook c++-mode-hook)
          do (add-hook mode
                       (lambda ()
                         (local-set-key (kbd "M-.") 'cme-jump)
                         (local-set-key (kbd "C-.") 'cme-find-anything)
                         (local-set-key (kbd "C-r") 'cme-browse-local-tags)
                         (local-set-key (kbd "M--") 'cme-symref)
                         (local-set-key (kbd "M-,") 'cme-pop-mark)
                         (local-set-key (kbd "M-*") 'cme-doc)
                         (local-set-key (kbd "C-,") 'cme-proto-impl-toggle)
                         (local-set-key (kbd "C--") 'cme-rename-local-var)
                         (local-set-key (kbd "M-<next>") 'cme-next-tag)
                         (local-set-key (kbd "M-<prior>") 'cme-previous-tag)
                         (local-set-key (kbd "M-p") 'cme-follow-ref-up)
                         (local-set-key (kbd "M-c") 'cme-find-subclasses)
                         (local-set-key (kbd "M-f") 'cme-fold-tag-toggle)
                         (local-set-key (kbd "M-d") 'cme-mark-tag)
                         (local-set-key (kbd "M-g") 'cme-reparse-buffer)
                         (local-set-key (kbd "<tab>")
                                        'company-indent-or-complete-common))))))

(defalias 'cme-next-tag 'senator-next-tag)
(defalias 'cme-previous-tag 'senator-previous-tag)
(defalias 'cme-fold-tag-toggle 'senator-fold-tag-toggle)
(defalias 'cme-reparse-buffer 'semantic-force-refresh)
(defalias 'cme-compile-project 'ede-compile-project)
(defalias 'cme-list-includes 'semanticdb-find-adebug-scanned-includes)

;; In major version 27 and newer, symbols with displayor in name were
;; renamed so that word displayer is used instead.
(when (< emacs-major-version 27)
  (defalias 'semantic-displayer-current-focus
    'semantic-displayor-current-focus)
  (defalias 'semantic-displayer-focus-abstract-child-p
    'semantic-displayor-focus-abstract-child-p)
  (defalias 'semantic-displayer-cleanup
    'semantic-displayor-cleanup)
  (defalias 'semantic-displayer-traditional
    'semantic-displayor-traditional)
  (defalias 'semantic-displayer-tooltip
    'semantic-displayor-tooltip))

;; Byte compile all CME functions on startup.
(mapatoms (lambda (sym)
            (when (and (s-starts-with-p "cme-" (format "%s" sym))
                       (fboundp sym))
              (let ((byte-compile-log-warning-function
                     (lambda (&rest args))))
                (byte-compile sym)))))

;; Detect emacs version changes and remove semantic DB when it happens.
;; DB store tags as eieio object dumps and sometimes, new emacs version
;; breaks backward compatibility which CEDET fails to resolve properly.
;; Instead, all just became buggy.
(let* ((write-file (lambda (filename data)
                     (ignore-errors
                       (let ((coding-system-for-write 'binary)
                             (write-region-annotate-functions nil)
                             (write-region-post-annotation-function nil))
                         (write-region data nil filename nil :silent)
                         nil))))
       (read-file (lambda (filename)
                    (ignore-errors
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (setq buffer-file-coding-system 'binary)
                        (insert-file-contents-literally filename)
                        (buffer-substring-no-properties (point-min)
                                                        (point-max))))))
       (root (expand-file-name user-emacs-directory))
       (version-filename (concat root "cme-last-ver.txt"))
       (version (funcall read-file version-filename)))
  (when (not (or (equal version nil)
                 (equal version (emacs-version))))
    (delete-directory (concat root "semanticdb") t t))
  (funcall write-file version-filename (emacs-version)))


(provide 'cme)
;;; cme.el ends here
