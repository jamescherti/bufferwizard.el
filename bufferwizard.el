;;; bufferwizard.el --- Buffer wizard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/bufferwizard.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
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

(defgroup bufferwizard nil
  "Buffer wizard."
  :group 'bufferwizard
  :prefix "bufferwizard-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/bufferwizard.el"))

(defcustom bufferwizard-rename-file-enable-vc t
  "If non-nil, enable renaming files using version control (VC) when available.
When this option is enabled and the file being renamed is under VC, the renaming
operation will be handled by the VC backend."
  :type 'boolean
  :group 'bufferwizard)

(defcustom bufferwizard-verbose nil
  "If non-nil, display messages during file renaming operations.
When this option is enabled, messages will indicate the progress
and outcome of the renaming process."
  :type 'boolean
  :group 'bufferwizard)

;;; Helper functions

(defun bufferwizard--message (&rest args)
  "Display a message with '[bufferwizard]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[bufferwizard] " (car args)) (cdr args)))

;;; Rename file

(defvar bufferwizard-before-rename-file-functions nil
  "List of functions to run before renaming a file.
Each function takes 3 argument: (buffer previous-path new-path).")

(defvar bufferwizard-after-rename-file-functions nil
  "List of functions to run after renaming a file.
Each function takes 3 argument: (buffer previous-path new-path).")

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

            (if (and bufferwizard-rename-file-enable-vc
                     (vc-backend filename))
                (progn
                  ;; Rename the file using VC
                  (vc-rename-file filename new-filename)
                  (when bufferwizard-verbose
                    (bufferwizard--message
                     "[VC RENAME] %s -> %s"
                     filename (file-name-nondirectory new-filename))))
              ;; Rename
              (rename-file filename new-filename 1)
              (when bufferwizard-verbose
                (bufferwizard--message
                 "[RENAME] %s -> %s"
                 filename (file-name-nondirectory new-filename))))

            (set-visited-file-name new-filename t t)

            ;; Update all buffers pointing to the old file Broken
            (bufferwizard--rename-all-buffer-names filename
                                                   new-filename)

            (run-hook-with-args 'bufferwizard-after-rename-file-functions
                                (current-buffer) filename new-filename)))))))

(provide 'bufferwizard)

;;; Delete file

(defun bufferwizard-delete-file (&optional buffer)
  "Kill BUFFER and delete file associated with it.
Delete the file associated with a buffer and kill all buffers visiting the file,
including indirect buffers or clones.
If BUFFER is nil, operate on the current buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (filename nil))
    (unless (buffer-live-p buffer)
      (error "Buffer '%s' is not alive" (buffer-name buffer)))

    (setq filename (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
    (unless filename
      (error "Buffer '%s' is not visiting a file" (buffer-name buffer)))
    (setq filename (file-truename filename))

    (when (y-or-n-p (format "Delete file '%s'?"
                            (file-name-nondirectory filename)))
      (when (buffer-modified-p buffer)
        (let ((save-silently t))
          (save-buffer buffer)))

      ;; Delete the file associated with BUFFER (clones / indirect buffers)
      (dolist (buf (buffer-list))
        (when
            (and (buffer-file-name buf)
                 (string-equal filename
                               (file-truename
                                (buffer-file-name
                                 (or (buffer-base-buffer buf) buf)))))
          (kill-buffer buf)))

      (if (file-exists-p filename)
          (delete-file filename))

      (when bufferwizard-verbose
        (message "[Deleted] %s" filename)))))

;;; bufferwizard.el ends here
