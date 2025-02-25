;;; bufferwizard.el --- Buffer wizard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/bufferwizard.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The **bufferwizard** Emacs package provides a collection of helper functions
;; and commands for managing buffers.

;;; Code:

(require 'cl-lib)
(require 'hi-lock)

(defgroup bufferwizard nil
  "Buffer wizard."
  :group 'bufferwizard
  :prefix "bufferwizard-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/bufferwizard.el"))

;;; Helper functions

(defun bufferwizard--message (&rest args)
  "Display a message with '[bufferwizard]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[bufferwizard] " (car args)) (cdr args)))

(defun bufferwizard--get-list-buffers (filename)
  "Return a list of buffers visiting the specified FILENAME.

FILENAME is the absolute path of the file to check for associated buffers.

Iterates through all buffers and performs the following check: If a buffer is
visiting the specified FILENAME (based on the true file path), it is added to
the resulting list.

Returns a list of buffers that are associated with FILENAME."
  (let ((filename (file-truename filename))
        (list-buffers nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let* ((base-buf (or (buffer-base-buffer buf) buf))
               (buf-filename (when base-buf (buffer-file-name base-buf))))
          (when (and buf-filename
                     (string-equal filename (file-truename buf-filename)))
            (push buf list-buffers)))))
    list-buffers))

;;; Indirect buffers

(defun bufferwizard-clone-indirect-buffer (&optional newname
                                                     display-flag
                                                     norecord)
  "Create an indirect buffer while preserving window state.

This function is an enhanced version of the built-in `clone-indirect-buffer'.

This function creates an indirect buffer with the same content as the current
buffer, ensuring that the point position, window start, and horizontal scroll
position remain unchanged. The original buffer remains intact.

- If NEWNAME is provided, it is used as the name of the new buffer. Otherwise, a
  default name is generated by appending <N> to the current buffer name, where
  N is an incrementing number.
- If DISPLAY-FLAG is non-nil, the new buffer is displayed using `pop-to-buffer'.
  This always happens when called interactively.
- If NORECORD is non-nil, the new buffer is not placed at the front of the
  recently selected buffer list.

This function does not allow cloning buffers whose major mode has a non-nil
`no-clone-indirect' property. If attempted, an error is signaled.

Returns the newly created indirect buffer."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
               (read-buffer "Name of indirect buffer: " (current-buffer)))
           t)))
  (let ((point (point))
        (window-start (window-start))
        (window-hscroll (window-hscroll))
        (indirect-buffer (clone-indirect-buffer newname display-flag norecord)))
    (when (buffer-live-p indirect-buffer)
      (switch-to-buffer indirect-buffer)
      (with-current-buffer indirect-buffer
        (goto-char point)
        (let ((selected-window (selected-window))
              (buffer-window (get-buffer-window indirect-buffer)))
          (when (and buffer-window
                     (eq selected-window buffer-window))
            (set-window-start selected-window window-start)
            (set-window-hscroll selected-window window-hscroll))))
      indirect-buffer)))

(defun bufferwizard-clone-and-switch-to-indirect-buffer (&optional newname
                                                                   norecord)
  "Create an indirect buffer and switch to it.

This function is a variant of `bufferwizard-clone-indirect-buffer', except it
also switches to the new indirect buffer.

This function creates a new indirect buffer with the same content as the current
buffer, preserving point position, window start, and horizontal scroll position.
The original buffer remains unchanged.

The optional argument NEWNAME specifies the indirect buffer name. If NEWNAME is
nil, the name defaults to the current buffer's name with a <N> suffix added,
or by incrementing N in an existing suffix. An error occurs when attempting to
clone a buffer whose major mode symbol has a non-nil `no-clone-indirect'
property.

The optional argument NORECORD, when non-nil, prevents putting this buffer at
the front of the list of recently selected ones.

Returns the newly created indirect buffer."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
               (read-buffer "Name of indirect buffer: " (current-buffer)))
           t)))
  (bufferwizard-clone-indirect-buffer newname nil norecord))

(defun bufferwizard-switch-to-base-buffer (&optional buffer)
  "Switch to the base buffer if BUFFER is indirect.
Preserve point, `window-start', and horizontal scrolling."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (base-buffer (buffer-base-buffer buffer)))
    (unless base-buffer
      (user-error "Buffer '%s' is not an indirect buffer" (buffer-name buffer)))
    (let ((point (with-current-buffer buffer (point)))
          (window-start (window-start))
          (window-hscroll (window-hscroll)))
      (switch-to-buffer base-buffer)
      (goto-char point)
      (when (eq (current-buffer) base-buffer)
        (set-window-start nil window-start)
        (set-window-hscroll nil window-hscroll)))))

;;; Symbol helpers

(defun bufferwizard--symbol-at-point-regexp ()
  "Return a regexp that matches the symbol at point."
  (let ((symbol (thing-at-point 'symbol t)))
    (when symbol
      ;; This returns
      (concat "\\_<" (regexp-quote symbol) "\\_>"))))

;;; Search and replace (string)

(defun bufferwizard-replace-regexp (from-regexp &optional to-string)
  "Replace occurrences of FROM-REGEXP with TO-STRING.
When TO-STRING is not specified, the user is prompted for input.
This function confirms each replacement."
  (when buffer-read-only
    (error "The buffer '%s' is read-only" (buffer-name)))
  (let ((orig-window-start (window-start))
        (scroll-conservatively 10))
    (save-excursion
      (let ((undo-handle (prepare-change-group))
            (start (point)))
        (unwind-protect
            (progn
              ;; Replace from the current position
              (query-replace-regexp from-regexp to-string nil start (point-max))

              ;; Replace from the beginning
              (when (> start (point-min))
                (query-replace-regexp
                 from-regexp to-string nil (point-min) (1- start))))
          (undo-amalgamate-change-group undo-handle)
          (set-window-start nil orig-window-start))))))

;;;###autoload
(defun bufferwizard-replace-symbol-at-point (&optional to-string)
  "Replace occurrences of a symbol at point with a specified string.
When TO-STRING is not specified, the user is prompted for input.
This function confirms each replacement."
  (interactive)
  (when buffer-read-only
    (error "The buffer '%s' is read-only" (buffer-name)))
  (let ((region nil)
        (from-string nil)
        (string-start nil)
        (string-regexp))
    (if (use-region-p)
        ;; Region
        (progn
          (setq region t)
          (setq from-string
                (buffer-substring-no-properties (region-beginning)
                                                (region-end)))
          (setq string-start (region-beginning))
          (when from-string
            (deactivate-mark))
          (setq string-regexp (regexp-quote from-string)))
      (setq from-string (thing-at-point 'symbol t))
      (setq string-start (car (bounds-of-thing-at-point 'symbol)))
      (setq string-regexp (bufferwizard--symbol-at-point-regexp)))
    ;; Ask the user
    (when from-string
      (unwind-protect
          (progn
            (highlight-regexp string-regexp 'lazy-highlight)
            (unless to-string
              (setq to-string
                    (read-string (format "Replace '%s' with: " from-string)
                                 from-string))
              ;; Update the regular expression because the user has just entered
              ;; a new one.
              (if region
                  (setq string-regexp (regexp-quote from-string))
                (setq string-regexp (bufferwizard--symbol-at-point-regexp)))))
        (unhighlight-regexp string-regexp))
      ;; Replace
      (goto-char string-start)
      (bufferwizard-replace-regexp string-regexp to-string))))

;;; Highlight symbols

(defun bufferwizard-highlight-p ()
  "Return non-nil the symbol at point is currently highlighted."
  (member (bufferwizard--symbol-at-point-regexp)
          (hi-lock--regexps-at-point)))

;;;###autoload
(defun bufferwizard-highlight-symbol-at-point ()
  "Highlight the symbol at point in the current buffer.

This function identifies the symbol at the current point, generates the
appropriate regular expression for it, and applies highlighting using the
built-in `hi-lock' package."
  (interactive)
  (when-let* ((regexp (find-tag-default-as-symbol-regexp)))
    (hi-lock-face-symbol-at-point)))

;;;###autoload
(defun bufferwizard-unhighlight-symbol-at-point ()
  "Remove highlighting for the symbol at point."
  (interactive)
  (when-let* ((regexp (find-tag-default-as-symbol-regexp)))
    (hi-lock-unface-buffer regexp)))

;;;###autoload
(defun bufferwizard-toggle-highlight-symbol-at-point ()
  "Toggle highlighting for the symbol at point.

This function checks if the symbol at point is currently highlighted.
If it is, it removes the highlight; otherwise, it applies the highlight."
  (interactive)
  (if (bufferwizard-highlight-p)
      (bufferwizard-unhighlight-symbol-at-point)
    (bufferwizard-highlight-symbol-at-point)))

;;;###autoload
(defun bufferwizard-unhighlight ()
  "Remove highlighting of each match to REGEXP.
Interactively, prompt for REGEXP, accepting only regexps previously inserted by
hi-lock interactive functions. If REGEXP is t (or if \\[universal-argument] was
specified interactively), then remove all hi-lock highlighting."
  (interactive)
  (call-interactively 'hi-lock-unface-buffer))

;;; Provide
(provide 'bufferwizard)
;;; bufferwizard.el ends here
