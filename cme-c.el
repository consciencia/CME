;;; cme-c.el --- Overrides for semantic c

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

;; Walks all for cycles in range and parses variables defined in them.
;;
;; WARNING: Does not respect for cycle scope nesting.
;;
;; NOTE: When reworking current hack to correct handling of cycle
;; scopes, don't forget that some for cycles may by without scope
;; braces.
;;
;; Following functions will be very handy when dealing with cycle
;; context analysis:
;;     semantic-beginning-of-context
;;     semantic-end-of-context
(defun cme-parse-vars-in-fors-in-c-cpp (context-begin context-end)
  (save-excursion
    (goto-char context-begin)
    (let ((vars nil))
      (while (re-search-forward (pcre-to-elisp/cached "^\\s*for\\s*\\(")
                                context-end
                                t)
        (goto-char (match-end 0))
        (push (semantic-parse-region (point)
                                     (save-excursion
                                       (semantic-end-of-context)
                                       (point))
                                     'bovine-inner-scope
                                     nil
                                     t)
              vars)
        (semantic-end-of-context))
      ;; Reverse the tags so that first tag is the most far away one.
      ;; This is important for handling variable shadowing.
      (setq vars (nreverse (apply #'append vars)))
      ;; And here is variable shadowing using hash map.
      (let ((map (cme-map-create)))
        (loop for var in vars
              for varname = (semantic-tag-name var)
              do (cme-map-set varname var map))
        (setq vars nil)
        (cme-map-everything (lambda (key val) (push val vars)) map))
      vars)))

;; Enhanced version of function semantic-get-local-variables-default.
;; In C/C++, local variables in for cycles are not parsed because
;; variable parser just refuses to parse stuff inside statements.
;; As an workaround, we walk all seen for cycles and release semantic
;; variable parser only on its internals which are parsed just fine.
;;
;; WARNING: This hack does not respect nesting of cycle contexts, it
;;          will just parse all variables in for cycles located in
;;          <start-of-toplevel-context, starting-point>. As an
;;          result, variables which are inaccessible from the
;;          starting-point are reported even though they should not.
;;
;; NOTE: It is not exactly true that variable parser refuses to parse
;;       variables in cycles. Actually it is able to parse them when
;;       lexer is configured with depth of 2 instead of nil which
;;       defaults to 0. Unfortunately it is a lot slower and it catches
;;       more false positives than when configured with lexer depth 0
;;       and it is even worse when it comes to respecting nesting of
;;       for cycles. At least, my hack knows that all for cycles after
;;       cursor are out of game.
(defun cme-semantic-get-local-variables-in-c-cpp ()
  "Get local values from a specific context.
Uses the bovinator with the special top-symbol `bovine-inner-scope'
to collect tags, such as local variables or prototypes."
  ;; This assumes a bovine parser.  Make sure we don't do
  ;; anything in that case.
  (when (and semantic--parse-table (not (eq semantic--parse-table t)))
    (let ((vars (semantic-get-cache-data 'get-local-variables)))
      (if vars
          (progn
            ;;(message "Found cached vars.")
            vars)
        (let ((vars2 nil)
              ;; We want nothing to do with funny syntaxing while doing this.
              (semantic-unmatched-syntax-hook nil)
              (start (point))
              (firstusefulstart nil)
              (last-context-begin nil))
          (while (not (semantic-up-context (point) 'function))
            (when (not vars)
              (setq firstusefulstart (point)))
            (setq last-context-begin (point))
            (save-excursion
              (forward-char 1)
              (setq vars
                    ;; Note to self: semantic-parse-region returns cooked
                    ;; but unlinked tags.  File information is lost here
                    ;; and is added next.
                    (append (semantic-parse-region (point)
                                                   (save-excursion
                                                     (semantic-end-of-context)
                                                     (point))
                                                   'bovine-inner-scope
                                                   nil
                                                   t)
                            vars))))
          ;; Get variables from all previous for cycles.
          (when last-context-begin
            (setq vars
                  (append
                   (cme-parse-vars-in-fors-in-c-cpp last-context-begin
                                                    start)
                   vars)))
          ;; Modify the tags in place.
          (setq vars2 vars)
          (while vars2
            (semantic--tag-put-property (car vars2)
                                        :filename (buffer-file-name))
            (setq vars2 (cdr vars2)))
          ;; Hash our value into the first context that produced useful results.
          (when (and vars firstusefulstart)
            (let ((end (save-excursion
                         (goto-char firstusefulstart)
                         (save-excursion
                           (unless (semantic-end-of-context)
                             (point))))))
              ;;(message "Caching values %d->%d." firstusefulstart end)
              (semantic-cache-data-to-buffer (current-buffer)
                                             firstusefulstart
                                             (or end
                                                 ;; If the end-of-context fails,
                                                 ;; just use our cursor starting
                                                 ;; position.
                                                 start)
                                             vars
                                             'get-local-variables
                                             'exit-cache-zone)))
          ;; Return our list.
          vars)))))

;; Override this function for c mode only. We dont want this logic
;; anywhere else.
(define-mode-local-override semantic-get-local-variables c-mode
  (&optional point)
  "Get all variables from cycles in c mode"
  (cme-semantic-get-local-variables-in-c-cpp))

;; Override this function for c++ mode only. We dont want this logic
;; anywhere else.
;;
;; Borrowed injection of "this" variable from old override.
(define-mode-local-override semantic-get-local-variables c++-mode
  (&optional point)
  "Get all variables from cycles in c++ mode"
  (let* ((origvar (cme-semantic-get-local-variables-in-c-cpp))
         (ct (semantic-current-tag))
         (p (when (semantic-tag-of-class-p ct 'function)
              (or (semantic-tag-function-parent ct)
                  (car-safe (semantic-find-tags-by-type
                             "class" (semantic-find-tag-by-overlay)))))))
    ;; If we have a function parent, then that implies we can
    (if p
        ;; Append a new tag THIS into our space.
        (cons (semantic-tag-new-variable "this" p nil :pointer 1)
              origvar)
      ;; No parent, just return the usual.
      origvar)))


(provide 'cme-c)
;;; cme-c.el ends here
