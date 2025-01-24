;;; bufferwizard.el --- Buffer wizard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/bufferwizard.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))
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
Each function takes 3 argument: (buffer previous-path new-path).")

(defvar bufferwizard-after-rename-file-functions nil
  "List of functions to run after renaming a file.
Each function takes 3 argument: (buffer previous-path new-path).")

(defvar bufferwizard-before-delete-file-functions nil
  "List of functions to run before deleting a file.
Each function takes 2 argument: (list-buffer path).")

(defvar bufferwizard-after-delete-file-functions nil
  "List of functions to run after deleting a file.
Each function takes 2 argument: (list-buffer path).")

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
        (let* ((buf (or (buffer-base-buffer buf) buf))
               (buf-filename (when buf (buffer-file-name buf))))
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
             (new-basename (read-string "New name: " basename)))
        (unless (string= basename new-basename)
          (let ((new-filename (file-truename
                               (expand-file-name
                                new-basename (file-name-directory filename)))))
            (run-hook-with-args 'bufferwizard-before-rename-file-functions
                                (current-buffer) filename new-filename)

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
                                (current-buffer) filename new-filename)))))))

;;; Delete file

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

        (dolist (buf list-buffers)
          (kill-buffer buf))

        (run-hook-with-args 'bufferwizard-before-delete-file-functions
                            list-buffers filename)

        (if (file-exists-p filename)
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

(provide 'bufferwizard)
;;; bufferwizard.el ends here
