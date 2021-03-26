;;; cme-symref.el --- Overrides for semantic symref

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

(defvar semantic-symref-current-query nil)

(defun semantic-symref-symbol (sym)
  "Find references to the symbol SYM.
  This command uses the currently configured references tool within the
  current project to find references to the input SYM.  The
  references are organized by file and the name of the function
  they are used in.
  Display the references in `semantic-symref-results-mode'."
  (interactive (list (cme-with-disabled-grep-db
                         (cme-semantic-complete-read-tag-project
                          "Find references for: "
                          nil
                          (thing-at-point 'symbol)))))
  (semantic-fetch-tags)
  ;; Gather results and tags
  (message "Gathering References...")
  (let ((res (semantic-symref-find-references-by-name sym)))
    (semantic-symref-produce-list-on-results res sym)))

(defalias 'cme-symref 'semantic-symref-symbol)

;; In comparison to semantic-symref-result-get-tags, it does not strip
;; out duplicates and populate hit property of tags.
(cl-defmethod semantic-symref-result-get-tags-as-is ((result semantic-symref-result)
                                                     &optional
                                                     open-buffers)
  (if (and (slot-boundp result 'hit-tags) (oref result hit-tags))
      (oref result hit-tags)
    ;; Calculate the tags.
    (let ((lines (oref result hit-lines))
          (txt (oref (oref result created-by) searchfor))
          (searchtype (oref (oref result created-by) searchtype))
          (ans nil)
          (out nil))
      (save-excursion
        (setq ans (mapcar
                   (lambda (hit)
                     (semantic-symref-hit-to-tag-via-buffer
                      hit txt searchtype open-buffers))
                   lines)))
      ;; Kill off dead buffers, unless we were requested to leave them open.
      (if (not open-buffers)
          (add-hook 'post-command-hook
                    'semantic-symref-cleanup-recent-buffers-fcn)
        ;; Else, just clear the saved buffers so they aren't deleted later.
        (setq semantic-symref-recently-opened-buffers nil))
      (loop for T in ans if T collect T))))

(defun semantic-symref-produce-list-on-results (res str)
  "Produce a symref list mode buffer on the results RES."
  (when (not res) (error "No references found"))
  (semantic-symref-result-get-tags res t)
  (message "Gathering References...done")
  ;; Build a references buffer.
  (let ((buff (get-buffer-create (format "*Symref %s" str))))
    (cme-push-mark)
    (switch-to-buffer buff)
    (set-buffer buff)
    (semantic-symref-results-mode)
    (set (make-local-variable 'semantic-symref-current-results) res)
    (set (make-local-variable 'semantic-symref-current-query) str)
    (semantic-symref-results-dump res)
    (goto-char (point-min))))

(defun semantic-symref-rb-goto-file (&optional button)
  (interactive)
  (let* ((tag (button-get button 'tag))
         (buff (semantic-tag-buffer tag)))
    (cme-push-mark)
    (switch-to-buffer buff)
    (pulse-momentary-highlight-one-line (point))))

(defun semantic-symref-rb-goto-tag (&optional button)
  (interactive)
  (let* ((tag (button-get button 'tag))
         (buff (semantic-tag-buffer tag)))
    (cme-push-mark)
    (switch-to-buffer buff)
    (semantic-go-to-tag tag)
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(defun semantic-symref-rb-goto-match (&optional button)
  "Go to the file specified in the symref results buffer.
  BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
         (line (button-get button 'line))
         (buff (semantic-tag-buffer tag))
         (query semantic-symref-current-query))
    (cme-push-mark)
    (switch-to-buffer buff)
    (goto-char (point-min))
    (forward-line (1- line))
    ;; We have just line information, but that's ok, we can
    ;; jump to concrete column using forward search.
    (when (re-search-forward (regexp-quote query)
                             (point-at-eol)
                             t)
      (goto-char (match-beginning 0)))
    (recenter)
    (pulse-momentary-highlight-one-line (point))))

(let ((map semantic-symref-results-mode-map))
  (define-key map (kbd "<C-right>") 'forward-button)
  (define-key map (kbd "<C-left>") 'backward-button)
  (define-key map (kbd "RET") 'push-button)
  (define-key map (kbd "+")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "-")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "=")  'semantic-symref-list-toggle-showing)
  (define-key map (kbd "SPC") 'semantic-symref-list-toggle-showing)
  (define-key map (kbd "C-+") 'semantic-symref-list-expand-all)
  (define-key map (kbd "C--") 'semantic-symref-list-contract-all)
  (define-key map (kbd "C-r") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "R") 'semantic-symref-list-rename-open-hits)
  (define-key map (kbd "q") 'cme-pop-mark)
  (define-key map (kbd "C-q") 'cme-pop-mark))

;; Machinery for renaming local variables is part of symref facility so
;; it ended here.
(define-key srecode-field-keymap (kbd "<C-return>") 'srecode-field-exit-ask)

;; Adds support for multi repository symref searches. Will affect grep
;; search backend too.
(defun semantic-symref-grep-use-template (rootdir filepattern flags pattern)
  "Use the grep template expand feature to create a grep command.
ROOTDIR is the root location to run the `find' from.
FILEPATTERN is a string representing find flags for searching file patterns.
FLAGS are flags passed to Grep, such as -n or -l.
PATTERN is the pattern used by Grep."
  ;; We have grep-compute-defaults.  Let's use it.
  (setq rootdir (s-join " "
                        (cons rootdir
                              (let ((proj (ede-toplevel)))
                                (if (and proj
                                         (same-class-p proj
                                                       'cme-generic-proj))
                                    (ede-source-roots proj))))))
  (grep-compute-defaults)
  (let* ((semantic-symref-grep-flags flags)
         (grep-expand-keywords semantic-symref-grep-expand-keywords)
	     (cmd (grep-expand-template
               (if (memq system-type '(windows-nt ms-dos))
                   ;; FIXME: Is this still needed?
                   ;; grep-find uses '--color=always' on MS-Windows
                   ;; because it wants the colorized output, to show
                   ;; it to the user.  By contrast, here we don't show
                   ;; the output, and the SGR escapes get in the way
                   ;; of parsing the output.
                   (replace-regexp-in-string "--color=always" ""
                                             grep-find-template t t)
                 grep-find-template)
               pattern
               filepattern
               rootdir)))
    cmd))

;; Overriden because default implementation completely ignored
;; function parameters in function signature.
(defun semantic-symref-hits-in-region (target hookfcn start end)
  "Find all occurrences of the symbol TARGET that match TARGET the tag.
For each match, call HOOKFCN.
HOOKFCN takes three arguments that match
`semantic-analyze-current-symbol's use of HOOKFCN.
  ( START END PREFIX )

Search occurs in the current buffer between START and END."
  (require 'semantic/idle)
  (save-excursion
    (goto-char start)
    (let* ((str (semantic-tag-name target))
           (case-fold-search semantic-case-fold)
           (regexp (concat "\\<" (regexp-quote str) "\\>")))
      (while (re-search-forward regexp end t)
        (when (semantic-idle-summary-useful-context-p)
          (semantic-analyze-current-symbol
           (lambda (start end prefix)
             (let ((tag (car (nreverse prefix)))
                   (parent-tag (semantic-current-tag-parent)))
               ;; check for semantic match on the text match.
               (when (or (and (semantic-tag-p tag)
                              (semantic-equivalent-tag-p target tag))
                         ;; Hack in order to get recognized functional
                         ;; arguments.
                         ;; When tag failed to be analyzed, it might
                         ;; actually be hit into function parameter
                         ;; enumeration.
                         ;; In such case, we must somehow decide if it
                         ;; is true.
                         ;; 1) We get parent tag.
                         ;; 2) If it is function, we get all its arguments.
                         ;; 3) We try to find all arguments which
                         ;;    match to unresolved prefix name.
                         ;; 4) After that, we iterate over result from
                         ;;    step 3 and try to find at least one similar
                         ;;    parameter tag to the passed target tag.
                         ;;
                         ;; If at least one hit was found, we can be
                         ;; pretty sure that we are looking at
                         ;; functional parameter.
                         (and (stringp tag)
                              (semantic-tag-p parent-tag)
                              (equal (semantic-tag-class parent-tag)
                                     'function)
                              (loop for candidate
                                    in (semantic-find-tags-by-name
                                        tag
                                        (semantic-tag-function-arguments
                                         parent-tag))
                                    if (semantic-equivalent-tag-p target
                                                                  candidate)
                                    collect candidate)))
                 (save-excursion (funcall hookfcn
                                          start
                                          end
                                          prefix)))))
           (point)))))))

;; Overriden because default implementation ignored function
;; parameters in function signatures.
(defun semantic-symref-rename-local-variable ()
  "Fancy way to rename the local variable under point.
Depends on the SRecode Field editing API."
  (interactive)
  ;; Do the replacement as needed.
  (let* ((ctxt (semantic-analyze-current-context))
         (target (car (reverse (oref ctxt prefix))))
         (tag (semantic-current-tag)))
    ;; Hack for the situation when we are trying to renamed function
    ;; parameter and point is positioned in function signature.
    ;; We don't get any analysis here, so we must try to do it ourselves.
    (when (and (stringp target)
               (semantic-tag-with-position-p tag)
               (equal (semantic-tag-name tag) target)
               (semantic-current-tag-parent)
               (equal (semantic-tag-class (semantic-current-tag-parent))
                      'function))
      (setq target tag
            tag (semantic-current-tag-parent)))
    (when (not tag)
      (error "Failed to find containing tag!"))
    (when (or (not target)
              (not (semantic-tag-with-position-p target)))
      (error "Cannot identify symbol under point"))
    (when (not (semantic-tag-of-class-p target 'variable))
      (error "Can only rename variables"))
    (when (or (< (semantic-tag-start target) (semantic-tag-start tag))
              (> (semantic-tag-end target) (semantic-tag-end tag)))
      (error "Can only rename variables declared in %s"
             (semantic-tag-name tag)))
    ;; I think we're good for this example.  Give it a go through
    ;; our fancy interface from SRecode.
    (require 'srecode/fields)
    ;; Make sure there is nothing active.
    (let ((ar (srecode-active-template-region)))
      (when ar (srecode-delete ar)))
    (let ((srecode-field-archive nil)
          (region nil))
      (semantic-symref-hits-in-region
       target (lambda (start end prefix)
                ;; For every valid hit, create one field.
                (srecode-field "LOCAL" :name "LOCAL" :start start :end end))
       (semantic-tag-start tag) (semantic-tag-end tag))
      ;; Now that the fields are setup, create the region.
      (setq region (srecode-template-inserted-region
                    "REGION" :start (semantic-tag-start tag)
                    :end (semantic-tag-end tag)))
      ;; Activate the region.
      (if (not (null (oref region fields)))
          (srecode-overlaid-activate region)
        (error "No reference in region was found!")))))

(defalias 'cme-rename-local-var 'semantic-symref-rename-local-variable)

;; Overriden because old implementation does not detected invalid
;; tagname hits. That way, users blamed CEDET instead of shitty tools
;; which CEDET uses (hello, GNU Global).
(defun semantic-symref-hit-to-tag-via-buffer
    (hit searchtxt searchtype &optional open-buffers)
  "Convert the symref HIT into a TAG by looking up the tag via a buffer.
Return the Semantic tag associated with HIT.
SEARCHTXT is the text that is being searched for.
Used to narrow the in-buffer search.
SEARCHTYPE is the type of search (such as 'symbol or 'tagname).
Optional OPEN-BUFFERS, when nil will use a faster version of
`find-file' when a file needs to be opened.  If non-nil, then
normal buffer initialization will be used.
This function will leave buffers loaded from a file open, but
will add buffers that must be opened to `semantic-symref-recently-opened-buffers'.
Any caller MUST deal with that variable, either clearing it, or deleting the
buffers that were opened."
  (let* ((line (car hit))
         (file (cdr hit))
         (buff (find-buffer-visiting file))
         (tag nil))
    (cond
     ;; We have a buffer already.  Check it out.
     (buff
      (set-buffer buff))

     ;; We have a table, but it needs a refresh.
     ;; This means we should load in that buffer.
     (t
      (let ((kbuff
             (if open-buffers
                 ;; Even if we keep the buffers open, don't
                 ;; let EDE ask lots of questions.
                 (let ((ede-auto-add-method 'never))
                   (find-file-noselect file t))
               ;; When not keeping the buffers open, then
               ;; don't setup all the fancy froo-froo features
               ;; either.
               (semantic-find-file-noselect file t))))
        (set-buffer kbuff)
        (push kbuff semantic-symref-recently-opened-buffers)
        (semantic-fetch-tags))))

    ;; Too much baggage in goto-line
    ;; (goto-line line)
    (goto-char (point-min))
    (forward-line (1- line))

    ;; Search forward for the matching text.
    ;; FIXME: This still fails if the regexp uses something specific
    ;; to the extended syntax, like grouping.
    (when (re-search-forward (if (memq searchtype '(regexp tagregexp))
                                 searchtxt
                               (regexp-quote searchtxt))
                             (point-at-eol)
                             t)
      (goto-char (match-beginning 0)))

    (setq tag (semantic-current-tag))

    ;; If we are searching for a tag, but bound the tag we are looking
    ;; for, see if it resides in some other parent tag.
    ;;
    ;; If there is no parent tag, then we still need to hang the originator
    ;; in our list.
    (when (and (eq searchtype 'symbol)
               (string= (semantic-tag-name tag) searchtxt))
      (setq tag (or (semantic-current-tag-parent) tag)))

    (when (and (eq searchtype 'tagname)
               (not (string= (semantic-tag-name tag)
                             searchtxt))
               (not (string= (semantic-tag-name
                              ;; Hack for potential nil return value.
                              ;; Don't want to create special var for
                              ;; it and don't want to call it twice.
                              (or (semantic-current-tag-parent)
                                  tag))
                             searchtxt)))
      (let ((msg (format (concat "Hit %s:%s does not match to %s "
                                 "(searched for %s), this is with high "
                                 "probability error of symref backend, not "
                                 "semantic!")
                         file
                         line
                         (semantic-tag-name tag)
                         searchtxt)))
        (message msg)
        (setq tag nil)))

    ;; Copy the tag, which adds a :filename property.
    (when tag
      (setq tag (semantic-tag-copy tag nil t))
      ;; Ad this hit to the tag.
      (semantic--tag-put-property tag :hit (list line)))
    tag))


(provide 'cme-symref)
;;; cme-symref.el ends here
