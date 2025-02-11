;;; bufferwizard.el --- Buffer wizard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/bufferwizard.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
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
;;
;; The current version includes the following functions:
;; - `bufferwizard-rename-file': Renames the file the current buffer is
;;   visiting. This command updates the file name on disk, adjusts the buffer
;;   name, and updates any indirect buffers or other buffers associated with the
;;   old file.
;; - `bufferwizard-delete-file': Delete the file associated with a buffer and
;;   kill all buffers visiting the file,including indirect buffers or clones.

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

(defcustom bufferwizard-use-vc t
  "If non-nil, enable using version control (VC) when available.
When this option is enabled and the file being deleted or renamed is under VC,
the renaming operation will be handled by the VC backend."
  :type 'boolean
  :group 'bufferwizard)

(defcustom bufferwizard-verbose nil
  "If non-nil, display messages during file renaming operations.
When this option is enabled, messages will indicate the progress
and outcome of the renaming process."
  :type 'boolean
  :group 'bufferwizard)

(defvar bufferwizard-before-rename-file-functions nil
  "List of functions to run before renaming a file.
Each function takes 3 argument: (list-buffers previous-path new-path).")

(defvar bufferwizard-after-rename-file-functions nil
  "List of functions to run after renaming a file.
Each function takes 3 argument: (list-buffers previous-path new-path).")

(defvar bufferwizard-before-delete-file-functions nil
  "List of functions to run before deleting a file.
Each function takes 2 argument: (list-buffers path).")

(defvar bufferwizard-after-delete-file-functions nil
  "List of functions to run after deleting a file.
Each function takes 2 argument: (list-buffers path).")

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

;;; Rename file

(defun bufferwizard--rename-all-buffer-names (old-filename new-filename)
  "Update buffer names to reflect the renaming of a file.
OLD-FILENAME and NEW-FILENAME are absolute paths as returned by `file-truename'.

For all buffers associated with OLD-FILENAME, update the buffer names to use
NEW-FILENAME.

This includes indirect buffers whose names are derived from the old filename."
  (let ((basename (file-name-nondirectory old-filename))
        (new-basename (file-name-nondirectory new-filename)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((base-buffer (buffer-base-buffer)))
            (when base-buffer
              (let* ((base-buffer-file-name
                      (let ((file-name (buffer-file-name base-buffer)))
                        (when file-name
                          (file-truename file-name)))))
                (when (and base-buffer-file-name
                           (string= base-buffer-file-name new-filename))
                  (let ((indirect-buffer-name (buffer-name)))
                    (let* ((new-buffer-name (concat new-basename
                                                    (substring
                                                     indirect-buffer-name
                                                     (length basename)))))
                      (when (string-prefix-p basename indirect-buffer-name)
                        (when new-buffer-name
                          (rename-buffer new-buffer-name))))))))))))))

;;;###autoload
(defun bufferwizard-rename-file (&optional buffer)
  "Rename the current file of that BUFFER is visiting.
This command updates:
- The file name on disk,
- the buffer name,
- all the indirect buffers or other buffers associated with the old file.

Hooks in `bufferwizard-before-rename-file-functions' and
`bufferwizard-after-rename-file-functions' are run before and after the renaming
process."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (let* ((filename (let ((file-name (buffer-file-name (buffer-base-buffer))))
                     (when file-name
                       (file-truename file-name))))
         (original-buffer (when filename
                            (get-file-buffer filename))))
    (unless filename
      (error "The buffer '%s' is not associated with a file"
             (buffer-name)))

    (unless (file-regular-p filename)
      (error "The file '%s' does not exist on disk" filename))

    (unless original-buffer
      (error "Could not locate the buffer for '%s'"
             filename))

    (with-current-buffer original-buffer
      (when (buffer-modified-p)
        (let ((save-silently t))
          (save-buffer)))

      (let* ((basename (if filename
                           (file-name-nondirectory filename)
                         ""))
             (new-basename (read-string "New name: " basename))
             (list-buffers (bufferwizard--get-list-buffers filename)))
        (unless (string= basename new-basename)
          (let ((new-filename (file-truename
                               (expand-file-name
                                new-basename (file-name-directory filename)))))
            (run-hook-with-args 'bufferwizard-before-rename-file-functions
                                list-buffers filename new-filename)

            (if (and bufferwizard-use-vc
                     (vc-backend filename))
                (progn
                  ;; Rename the file using VC
                  (vc-rename-file filename new-filename)
                  (when bufferwizard-verbose
                    (bufferwizard--message
                     "VC Rename: %s -> %s"
                     filename (file-name-nondirectory new-filename))))
              ;; Rename
              (rename-file filename new-filename 1)
              (when bufferwizard-verbose
                (bufferwizard--message "Rename: %s -> %s"
                                       filename
                                       (file-name-nondirectory new-filename))))

            (set-visited-file-name new-filename t t)

            ;; Update all buffers pointing to the old file Broken
            (bufferwizard--rename-all-buffer-names filename
                                                   new-filename)

            (run-hook-with-args 'bufferwizard-after-rename-file-functions
                                list-buffers filename new-filename)))))))

;;; Delete file

;;;###autoload
(defun bufferwizard-delete-file (&optional buffer)
  "Kill BUFFER and delete file associated with it.
Delete the file associated with a buffer and kill all buffers visiting the file,
including indirect buffers or clones.
If BUFFER is nil, operate on the current buffer.

Hooks in `bufferwizard-before-delete-file-functions' and
`bufferwizard-after-delete-file-functions' are run before and after the renaming
process."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (filename nil))
    (unless (buffer-live-p buffer)
      (error "The buffer '%s' is not alive" (buffer-name buffer)))

    (setq filename (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
    (unless filename
      (error "The buffer '%s' is not visiting a file" (buffer-name buffer)))
    (setq filename (file-truename filename))

    (when (y-or-n-p (format "Delete file '%s'?"
                            (file-name-nondirectory filename)))
      (let ((list-buffers (bufferwizard--get-list-buffers filename)))
        (dolist (buf list-buffers)
          (when (buffer-modified-p buf)
            (error "The buffer '%s' has not been saved yet" buf)))

        (run-hook-with-args 'bufferwizard-before-delete-file-functions
                            list-buffers filename)

        (dolist (buf list-buffers)
          (kill-buffer buf))

        (when (file-exists-p filename)
          (if (and bufferwizard-use-vc
                   (vc-backend filename))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _args) t)))
                (vc-delete-file filename))
            (delete-file filename)))

        (when bufferwizard-verbose
          (bufferwizard--message "Deleted: %s" filename))

        (run-hook-with-args 'bufferwizard-after-delete-file-functions
                            list-buffers filename)))))

;;; Clone indirect buffers

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

(defun bufferwizard-clone-indirect-buffer-current-window (&optional newname
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

;;; Helpers

(defun bufferwizard--symbol-at-point-regexp ()
  "Return a regexp that matches the symbol at point."
  (let ((symbol (thing-at-point 'symbol t)))
    (when symbol
      ;; This returns
      (concat "\\_<" (regexp-quote symbol) "\\_>"))))

;;; Replace occurences (string)

(defun bufferwizard-replace-regexp (from-regexp &optional to-string)
  "Replace occurrences of FROM-REGEXP with TO-STRING.
When TO-STRING is not specified, the user is prompted for input.
This function confirms each replacement."
  ;; Make sure it scrolls
  (let ((orig-window-start (window-start))
        (scroll-conservatively 10))
    (save-excursion
      (let ((undo-handle (prepare-change-group))
            (start (point))
            (case-replace t) ; Smart
            (case-fold-search t))
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
(defun bufferwizard-replace-string-at-point (&optional to-string)
  "Replace occurrences of a symbol at point with a specified string.
When TO-STRING is not specified, the user is prompted for input.
This function confirms each replacement."
  (interactive)
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
            (deactivate-mark)))
      ;; Not a region
      (setq from-string (thing-at-point 'symbol t))
      (setq string-start (car (bounds-of-thing-at-point 'symbol))))
    ;; Generate the regular expression
    (if region
        (setq string-regexp (regexp-quote from-string))
      (setq string-regexp (bufferwizard--symbol-at-point-regexp)))
    ;; Ask the user
    (when from-string
      (unwind-protect
          (progn
            (highlight-regexp string-regexp 'lazy-highlight)
            (unless to-string
              (setq to-string
                    (read-string (format "Replace '%s' with: " string-regexp)
                                 from-string))
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
  (let ((list-regexp-at-point
         (or (hi-lock--regexps-at-point)
             (mapcar (lambda (pattern)
                       (or (car (rassq pattern hi-lock-interactive-lighters))
                           (car pattern)))
                     hi-lock-interactive-patterns))))
    (member (bufferwizard--symbol-at-point-regexp)
            list-regexp-at-point)))

;;;###autoload
(defun bufferwizard-highlight-symbol-at-point ()
  "Highlight the symbol at point in the current buffer.

This function identifies the symbol at the current point, generates the
appropriate regular expression for it, and applies highlighting using the
built-in `hi-lock' package."
  (interactive)
  (cl-letf (((symbol-function 'find-tag-default-as-symbol-regexp)
             #'(lambda ()
                 (bufferwizard--symbol-at-point-regexp))))
    (hi-lock-face-symbol-at-point)))

;;;###autoload
(defun bufferwizard-unhighlight-symbol-at-point ()
  "Remove highlighting for the symbol at point."
  (interactive)
  (let ((regexp (bufferwizard--symbol-at-point-regexp)))
    (when regexp
      (hi-lock-unface-buffer regexp))))

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

(provide 'bufferwizard)
;;; bufferwizard.el ends here
