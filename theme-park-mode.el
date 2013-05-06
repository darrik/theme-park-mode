;;; theme-park-mode.el --- So much to see!

;; Copyright (C) 2013 Rikard Glans

;; Author: Rikard Glans <rikard@ecx.se>
;; Version: 0.1.0
;; Keywords: colorthemes, themes
;; Created: 6th May 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(when (< emacs-major-version 24)
  (error "Theme Park mode only works with Emacs 24 or greater"))

;; Variables
(defvar tpm_next nil
  "Next theme in queue.")

(defvar tpm_prev nil
  "Prev theme in queue.")

(defvar tpm_rest nil
  "Rest of themes to try.")

(defvar tpm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'tpm--next-theme)
    (define-key map (kbd "C-c C-p") 'tpm--prev-theme)
    (define-key map (kbd "C-c C-r") 'tpm--start-over)
    (define-key map (kbd "C-c C-q") 'tpm--quit)

    (define-key map (kbd "<right>") 'tpm--next-theme)
    (define-key map (kbd "<left>")  'tpm--prev-theme)
    (define-key map (kbd "<up>")    'tpm--start-over)
    (define-key map (kbd "<down>")  'tpm--quit)
    map)
  "Keyboard commands for Theme Park mode.")

(defvar tpm_ok 0)

;; Internal functions
(defun tpm--initialize ()
  "Initialize variables."
  (setq tpm_rest (cdr (custom-available-themes)))
  (setq tpm_next (car (custom-available-themes)))
  (setq tpm_prev tpm_next))

(defun tpm--load-theme (thm)
  "Load theme."
  (tpm--reset-theme)
  (when (not (eq thm nil))
    ;; (load-theme thm t) ; This is unsafe if you have unvetted themes installed.
    (load-theme thm)
    (message "Theme: %s" thm)))

(defun tpm--next-theme ()
  "Next theme."
  (interactive)

  (when (eq tpm_ok 1)
    (setq tpm_prev (car custom-enabled-themes))
    (setq tpm_next (car tpm_rest))
    (setq tpm_rest (cdr tpm_rest))

    (if (eq tpm_next nil)
        (tpm--initialize))
    (if (and (not (eq tpm_next nil))
             (eq tpm_rest nil))
        (setq tpm_rest (custom-available-themes)))
    )

  (tpm--load-theme tpm_next)

  (setq tpm_ok 1))

(defun tpm--prev-theme ()
  "Previous theme."
  (interactive)
  (if (eq tpm_prev nil)
      (tpm--reset)
    (progn
      (setq tpm_ok 0)
      (tpm--load-theme tpm_prev))))

(defun tpm--reset-theme ()
  "Disable all loaded themes, effectively resetting to default colors."
  (interactive)
  (mapc 'disable-theme custom-enabled-themes)
  (unless (eq custom-enabled-themes nil) ; FIXME: Handle properly
    (theme-park-mode -1)
    (error "ABANDON SHIP!")))

(defun tpm--reset ()
  "Reset."
  (tpm--reset-theme)
  (tpm--initialize))

(defun tpm--quit ()
  "Leave Theme Park mode."
  (interactive)
  (theme-park-mode -1)
  (message "Theme Park: Bye bye!"))

(defun tpm--start-over ()
  "Start over."
  (interactive)
  (tpm--reset)
  (message "Theme Park: Starting over."))

;;;###autoload
(define-minor-mode theme-park-mode
  "Theme park mode."
  :lighter " TP"
  :keymap tpm-mode-map
  (if theme-park-mode
      (tpm--initialize)))

(provide 'theme-park-mode)

;;; theme-park-mode.el ends here
