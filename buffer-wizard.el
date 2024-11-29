;;; buffer-wizard.el --- Buffer wizard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/buffer-wizard.el
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
;; Buffer wizard

;;; Code:

(defgroup buffer-wizard nil
  "Buffer wizard"
  :group 'buffer-wizard
  :prefix "buffer-wizard-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/buffer-wizard.el"))

(defun buffer-wizard--message (&rest args)
  "Display a message with '[buffer-wizard]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-wizard] " (car args)) (cdr args)))

(defun buffer-wizard--warning (&rest args)
  "Display a warning message with '[buffer-wizard] Warning: ' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-wizard] Warning: " (car args)) (cdr args)))

;;;###autoload
(define-minor-mode buffer-wizard-mode
  "Toggle `buffer-wizard-mode'."
  :global t
  :lighter " buffer-wizard"
  :group 'buffer-wizard
  (if buffer-wizard-mode
      t
    t))

(provide 'buffer-wizard)
;;; buffer-wizard.el ends here
