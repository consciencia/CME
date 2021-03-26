;;; cme-complete.el --- Overrides for semantic complete

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

(defclass cme-collector-project-chained
  (semantic-collector-project-abstract)
  ()
  "Context aware completion engine for tags in a project.")

(cl-defmethod semantic-collector-calculate-completions-raw
  ((obj cme-collector-project-chained) prefix completionlist)
  "Calculate the completions for prefix from completionlist."
  (with-current-buffer (oref obj buffer)
    (let* ((table semanticdb-current-table)
           (ctx (cme-with-disabled-grep-db
                    (cme-best-tag-user-assist-enabled type+user
                        (cme-analyze-current-context-detached prefix))))
           (result (semantic-analyze-possible-completions ctx
                                                          'no-unique)))
      (if result
          (list (cons table result))))))

;; Fixed invalid usage of semantic-analyze-current-context which
;; happened to be outside of semantic buffer.
;; Also it was wrapped in error catcher because abstract class has no
;; context slot.
;; Added check if slot 'first-pass-completions' exists before usage.
;; Reworked usage of 'try-completion' in order to support dot chains.
(cl-defmethod semantic-collector-calculate-completions
  ((obj semantic-collector-abstract) prefix partial)
  "Calculate completions for prefix as setup for other queries."
  (let* ((case-fold-search semantic-case-fold)
         (same-prefix-p (semantic-collector-last-prefix= obj prefix))
         (last-prefix (and (slot-boundp obj 'last-prefix)
                           (oref obj last-prefix)))
         (completionlist
          (cond ((or same-prefix-p
                     (and last-prefix (eq (compare-strings
                                           last-prefix 0 nil
                                           prefix 0 (length last-prefix)) t)))
                 ;; We have the same prefix, or last-prefix is a
                 ;; substring of the of new prefix, in which case we are
                 ;; refining our symbol so just re-use cache.
                 (oref obj last-all-completions))
                ((and last-prefix
                      (> (length prefix) 1)
                      (eq (compare-strings
                           prefix 0 nil
                           last-prefix 0 (length prefix)) t))
                 ;; The new prefix is a substring of the old
                 ;; prefix, and it's longer than one character.
                 ;; Perform a full search to pull in additional
                 ;; matches.
                 (with-current-buffer (oref obj buffer)
                   (cme-with-disabled-grep-db
                       (let ((context (semantic-analyze-current-context (point))))
                         ;; Set new context and make first-pass-completions
                         ;; unbound so that they are newly calculated.
                         (when (slot-exists-p obj context)
                           (oset obj context context))
                         (when (and (slot-exists-p obj 'first-pass-completions)
                                    (slot-boundp obj 'first-pass-completions))
                           (slot-makeunbound obj 'first-pass-completions)))))
                 nil)))
         ;; Get the result
         (answer (if same-prefix-p
                     completionlist
                   (semantic-collector-calculate-completions-raw
                    obj prefix completionlist)))
         (completion nil)
         (complete-not-uniq nil))
    ;;(semanticdb-find-result-test answer)
    (when (not same-prefix-p)
      ;; Save results if it is interesting and beneficial
      (oset obj last-prefix prefix)
      (oset obj last-all-completions answer))
    ;; Now calculate the completion.
    (let ((prefix-segments
           (car
            (with-current-buffer (oref obj buffer)
              (cme-ctxt-current-symbol-and-bounds-detached prefix))))
          (candidates (loop for tag in (semanticdb-strip-find-results answer)
                            collect (semantic-tag-name tag)))
          short-prefix)
      (when (> (length prefix-segments) 1)
        (setq short-prefix
              (s-join "." (cme-list-init prefix-segments)))
        (setq candidates
              (loop for c in candidates
                    collect (concat short-prefix "." c))))
      (setq completion (try-completion prefix candidates)))
    (oset obj last-whitespace-completion nil)
    (oset obj current-exact-match nil)
    ;; Only do this if a completion was found.  Letting a nil in
    ;; could cause a full semanticdb search by accident.
    (when completion
      (oset obj last-completion
            (cond
             ;; Unique match in AC.  Last completion is a match.
             ;; Also set the current-exact-match.
             ((eq completion t)
              (oset obj current-exact-match answer)
              prefix)
             ;; It may be complete (a symbol) but still not unique.
             ;; We can capture a match
             ((setq complete-not-uniq
                    (semanticdb-find-tags-by-name
                     prefix
                     answer))
              (oset obj current-exact-match
                    complete-not-uniq)
              prefix)
             ;; Non unique match, return the string that handles
             ;; completion
             (t (or completion prefix)))))))

;; Redefined to use semanticdb-find-result-nth instead of
;; semanticdb-find-result-nth-in-buffer which caused unwanted
;; buffer switching.
;; Added support for unknown input when cme-complete-accepts-unknown
;; is set to true.
(defun semantic-complete-current-match ()
  "Calculate a match from the current completion environment.
Save this in our completion variable.  Make sure that variable
is cleared if any other keypress is made.
Return value can be:
  tag - a single tag that has been matched.
  string - a message to show in the minibuffer."
  ;; Query the environment for an active completion.
  (let ((collector semantic-completion-collector-engine)
        (displayer semantic-completion-display-engine)
        (contents (semantic-completion-text))
        matchlist
        answer)
    (if (string= contents "")
        ;; The user wants the defaults!
        (setq answer semantic-complete-active-default)
      ;; This forces a full calculation of completion on CR.
      (when (not cme-complete-accepts-unknown)
        (save-excursion
          (semantic-collector-calculate-completions collector contents nil))
        (semantic-complete-try-completion))
      (cond
       (cme-complete-accepts-unknown
        (setq answer (semantic-completion-text)))
       ;; Input match displayer focus entry
       ((setq answer (semantic-displayer-current-focus displayer))
        ;; We have answer, continue
        )
       ;; One match from the collector
       ((setq matchlist (semantic-collector-current-exact-match collector))
        (if (= (semanticdb-find-result-length matchlist) 1)
            (setq answer (car (semanticdb-find-result-nth matchlist 0)))
          (if (semantic-displayer-focus-abstract-child-p displayer)
              ;; For focusing displayers, we can claim this is
              ;; not unique.  Multiple focuses can choose the correct
              ;; one.
              (setq answer "Not Unique")
            ;; If we don't have a focusing displayer, we need to do something
            ;; graceful.  First, see if all the matches have the same name.
            (let ((allsame t)
                  (firstname (semantic-tag-name
                              (car
                               (semanticdb-find-result-nth matchlist 0))))
                  (cnt 1)
                  (max (semanticdb-find-result-length matchlist)))
              (while (and allsame (< cnt max))
                (if (not (string=
                          firstname
                          (semantic-tag-name
                           (car
                            (semanticdb-find-result-nth matchlist cnt)))))
                    (setq allsame nil))
                (setq cnt (1+ cnt)))
              ;; Now we know if they are all the same.  If they are, just
              ;; accept the first, otherwise complain.
              (if allsame
                  (setq answer (car (semanticdb-find-result-nth matchlist 0)))
                (setq answer "Not Unique"))))))
       ;; No match
       (t (setq answer "No Match"))))
    ;; Set it into our completion target.
    (when (or (semantic-tag-p answer)
              cme-complete-accepts-unknown)
      (setq semantic-complete-current-matched-tag answer)
      ;; Make sure it is up to date by clearing it if the user dares
      ;; to touch the keyboard.
      (add-hook 'pre-command-hook
                (lambda () (setq semantic-complete-current-matched-tag nil))))
    ;; Return it
    answer))

;; Redefined to use semanticdb-find-result-nth instead of
;; semanticdb-find-result-nth-in-buffer which caused unwanted
;; buffer switching.
(defun semantic-complete-default-to-tag (default)
  "Convert a calculated or passed in DEFAULT into a tag."
  (if (semantic-tag-p default)
      ;; Just return what was passed in.
      (setq semantic-complete-active-default default)
    ;; If none was passed in, guess.
    (if (null default)
        (setq default (semantic-ctxt-current-thing)))
    (if (null default)
        ;; Do nothing
        nil
      ;; Turn default into something useful.
      (let ((str
             (cond
              ;; Semantic-ctxt-current-symbol will return a list of
              ;; strings.  Technically, we should use the analyzer to
              ;; fully extract what we need, but for now, just grab the
              ;; first string
              ((and (listp default) (stringp (car default)))
               (car default))
              ((stringp default)
               default)
              ((symbolp default)
               (symbol-name default))
              (t
               (signal 'wrong-type-argument
                       (list default 'semantic-tag-p)))))
            (tag nil))
        ;; Now that we have that symbol string, look it up using the active
        ;; collector.  If we get a match, use it.
        (save-excursion
          (semantic-collector-calculate-completions
           semantic-completion-collector-engine
           str nil))
        ;; Do we have the perfect match???
        (let ((ml (semantic-collector-current-exact-match
                   semantic-completion-collector-engine)))
          (when ml
            ;; We don't care about uniqueness.  Just guess for convenience
            (setq tag (car (semanticdb-find-result-nth ml 0)))))
        ;; save it
        (setq semantic-complete-active-default tag)
        ;; Return it.. .whatever it may be
        tag))))

;; Replaced semantic-complete-done for cme-semantic-complete-done
;; in order to support raw string outputs of names not known to semantic.
(defvar cme-semantic-complete-key-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " 'semantic-complete-complete-space)
    (define-key km "\t" 'semantic-complete-complete-tab)
    (define-key km "\C-m" 'cme-semantic-complete-done)
    (define-key km "\C-g" 'abort-recursive-edit)
    (define-key km "\M-n" 'next-history-element)
    (define-key km "\M-p" 'previous-history-element)
    (define-key km "\C-n" 'next-history-element)
    (define-key km "\C-p" 'previous-history-element)
    km)
  "Keymap used while completing across a list of tags.")

;; Accepts string result. Old behavior is to treat it as an error
;; message. Result from original implementation is always tag, not raw
;; unresolved name.
(setq cme-complete-accepts-unknown nil)
(defun cme-semantic-complete-done ()
  "Accept the current input."
  (interactive)
  (let* ((cme-complete-accepts-unknown t)
         (ans (semantic-complete-current-match)))
    (exit-minibuffer)))

;; Replaced semantic-complete-key-map for cme-semantic-complete-key-map
;; in order to support raw string outputs.
(defun cme-semantic-complete-read-tag-engine
    (collector displayer prompt default-tag initial-input history)
  "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argument DISPLAYER is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYER.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
  (let* ((semantic-completion-collector-engine collector)
         (semantic-completion-display-engine displayer)
         (semantic-complete-active-default nil)
         (semantic-complete-current-matched-tag nil)
         (default-as-tag (semantic-complete-default-to-tag default-tag))
         (default-as-string (when (semantic-tag-p default-as-tag)
                              (semantic-tag-name default-as-tag))))
    (when default-as-string
      ;; Add this to the prompt.
      ;;
      ;; I really want to add a lookup of the symbol in those
      ;; tags available to the collector and only add it if it
      ;; is available as a possibility, but I'm too lazy right
      ;; now.
      ;;
      ;; @todo - move from () to into the editable area
      (if (string-match ":" prompt)
          (setq prompt (concat
                        (substring prompt 0 (match-beginning 0))
                        " (default " default-as-string ")"
                        (substring prompt (match-beginning 0))))
        (setq prompt (concat prompt " (" default-as-string "): "))))
    ;; Perform the Completion
    (unwind-protect
        (read-from-minibuffer prompt
                              initial-input
                              cme-semantic-complete-key-map
                              nil
                              (or history
                                  'semantic-completion-default-history)
                              default-tag)
      (semantic-collector-cleanup semantic-completion-collector-engine)
      (semantic-displayer-cleanup semantic-completion-display-engine))
    ;; Extract the tag from the completion machinery.
    semantic-complete-current-matched-tag))

;; Redefined because original used more sophisticated displayer which
;; refused to select not unique tag name.
(defun cme-semantic-complete-read-tag-project (prompt
                                               &optional
                                               default-tag
                                               initial-input
                                               history)
  "Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in."
  ;; Load all existing databases for current project and its
  ;; dependencies into memory, otherwise this function will be unable
  ;; to provide autocompletion for all project symbols. Of course,
  ;; unparsed files which are logically missing in DB are not loaded
  ;; and symbols from them are invisible for autocompletion.
  (save-excursion
    (cme-load-all-project-dbs)
    (cme-semantic-complete-read-tag-engine
     ;; I used semantic-collector-project-brutish in past but my new
     ;; collector is able to complete even dot chains.
     (cme-collector-project-chained prompt
                                    :buffer (current-buffer)
                                    :path (current-buffer))
     (semantic-displayer-traditional)
     prompt
     default-tag
     initial-input
     history)))


(provide 'cme-complete)
;;; cme-complete.el ends here
