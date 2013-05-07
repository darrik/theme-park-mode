;;; theme-park-mode.el --- Take your themes for a ride!

;; Copyright (C) 2013 Rikard Glans

;; Author: Rikard Glans <rikard@ecx.se>
;; URL: https://github.com/darrik/theme-park-mode
;; Version: 0.1.2
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

;;; Commentary:

;; M-x theme-park-mode to take your themes for a ride. Right arrow to go
;; forward, Left backward, Up to go again and Down to stop. Designed to be
;; deactivated once you're done deciding on a theme (C-c C-q or down arrow.)

;;; Code:

(when (< emacs-major-version 24)
  (error "Theme Park mode only works with Emacs 24 or greater"))

;; Variables
(defvar tpm-themes nil
  "Holds a list of themes")

(defvar tpm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'tpm--next-theme)
    (define-key map (kbd "C-c C-p") 'tpm--prev-theme)
    (define-key map (kbd "C-c C-r") 'tpm--start-over)
    (define-key map (kbd "C-c C-q") 'tpm--quit)
    (define-key map (kbd "C-c C-c") 'tpm--current-theme)

    (define-key map (kbd "<right>") 'tpm--next-theme)
    (define-key map (kbd "<left>")  'tpm--prev-theme)
    (define-key map (kbd "<up>")    'tpm--start-over)
    (define-key map (kbd "<down>")  'tpm--quit)
    map)
  "Keyboard commands for Theme Park mode.")

;; Functions
(defun tpm--initialize ()
  "Initialize variables."
  (setq tpm-themes (custom-available-themes)))

(defun tpm--load-theme (thm)
  "Load theme."
  (tpm--reset-theme)
  (when (not (eq thm nil))
    ;; (load-theme thm t) ; This is unsafe if you have unvetted themes installed.
    (load-theme thm)
    (message "Theme: %s" thm)))

;; TODO: This mess could be prettier.
(defun tpm--step (direction themes)
  (let ((current (car custom-enabled-themes)))
    (if (eq direction 'forward)
        (let ((next (car themes)))
          (setq tpm-themes (append (cdr themes) (list next)))
          (if (eq current next)
              (tpm--step direction tpm-themes)
            (tpm--load-theme next)))
      (let ((next (car (last themes))))
        (setq tpm-themes (append (list next) (butlast themes)))
        (if (eq current next)
            (tpm--step direction tpm-themes)
          (tpm--load-theme next))))))

(defun tpm--next-theme ()
  "Next theme."
  (interactive)
  (tpm--step 'forward tpm-themes))

(defun tpm--prev-theme ()
  "Previous theme."
  (interactive)
  (tpm--step 'backward tpm-themes))

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

(defun tpm--current-theme ()
  "Show currently loaded theme."
  (interactive)
  (message "Current theme: %s" (car custom-enabled-themes)))

;;;###autoload
(define-minor-mode theme-park-mode
  "Theme park mode."
  :lighter " TP"
  :keymap tpm-mode-map
  (if theme-park-mode
      (tpm--initialize)))

(provide 'theme-park-mode)

;;; theme-park-mode.el ends here

