;;; theme-park-mode.el --- Take your themes for a ride!

;; Copyright (C) 2013 Rikard Glans

;; Author: Rikard Glans <rikard@ecx.se>
;; URL: https://github.com/darrik/theme-park-mode
;; Version: 0.2.1
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

;; Configuration variables
(defcustom tpm-tagged nil
  "List of personal themes."
  :type 'sexp
  :tag "List of private themes"
  :group 'theme-park-mode)

(defcustom tpm-auto-save t
  "Auto save tagged themes."
  :type 'boolean
  :group 'theme-park-mode)

;; Internal variables
(defvar tpm-mode 'global
  "Mode of operation.
global: cycle between all available themes.
local: cycle between tagged themes.")

(defvar tpm-themes nil
  "Holds a list of themes")

(defvar tpm-themes-private nil
  "Holds a list of private themes")

(defvar tpm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'tpm--next-theme)
    (define-key map (kbd "C-c C-p") 'tpm--prev-theme)
    (define-key map (kbd "C-c C-r") 'tpm--start-over)
    (define-key map (kbd "C-c C-q") 'tpm--quit)
    (define-key map (kbd "C-c C-c") 'tpm--current-theme)
    (define-key map (kbd "C-c C-t") 'tpm--tag)
    (define-key map (kbd "C-c C-v") 'tpm--show-tagged)
    (define-key map (kbd "C-c C-g") 'tpm--toggle-mode)
    (define-key map (kbd "C-c C-l") 'tpm--toggle-mode)
    (define-key map (kbd "C-c C-s") 'tpm--save-tagged)
    (define-key map (kbd "C-c C-d") 'tpm--tag-pop)

    (define-key map (kbd "<right>") 'tpm--next-theme)
    (define-key map (kbd "<left>")  'tpm--prev-theme)
    (define-key map (kbd "<up>")    'tpm--start-over)
    (define-key map (kbd "<down>")  'tpm--quit)
    map)
  "Keyboard commands for Theme Park mode.")

;; Functions
(defun tpm--isglobal ()
  "Return t if in global mode."
  (eq tpm-mode 'global))

(defun tpm--islocal ()
  "Return t if in local mode."
  (eq tpm-mode 'local))

(defun tpm--initialize ()
  "Initialize variables."
  (setq tpm-themes (custom-available-themes))
  (setq tpm-themes-private tpm-tagged)
  (message "Theme Park Mode: Hi there! (%s mode.)" tpm-mode))

(defun tpm--load-theme (thm)
  "Load theme."
  (tpm--reset-theme)
  (when (not (eq thm nil))
    ;; (load-theme thm t) ; This is unsafe if you have unvetted themes installed.
    (load-theme thm)
    (message "Theme: %s" thm)))

(defun tpm--cthm ()
  "Returns currently active theme."
  (car custom-enabled-themes))

(defun tpm--current-theme ()
  "Show currently loaded theme."
  (interactive)
  (message "Current theme: %s" (tpm--cthm)))

;; TODO: This mess could be prettier.
(defun tpm--step (direction themes)
  "Advance to next or previous theme."
  (let ((--set (lambda (lst)
                 (if (tpm--isglobal)
                     (setq tpm-themes lst)
                   (setq tpm-themes-private lst))))
        (--re-step (lambda (direction)
                     (if (tpm--isglobal)
                         (tpm--step direction tpm-themes)
                       (tpm--step direction tpm-themes-private))))
        (current (tpm--cthm)))
    (if (eq direction 'forward)
        (let ((next (car themes)))
          (funcall --set (append (cdr themes) (list next)))
          (if (eq current next)
              (funcall --re-step direction)
            (tpm--load-theme next)))
      (let ((next (car (last themes))))
        (funcall --set (append (list next) (butlast themes)))
        (if (eq current next)
            (funcall --re-step direction)
          (tpm--load-theme next))))))

(defun tpm--next-theme ()
  "Next theme."
  (interactive)
  (tpm--step-theme 'forward))

(defun tpm--prev-theme ()
  "Previous theme."
  (interactive)
  (tpm--step-theme 'backward))

(defun tpm--step-theme (direction)
  "Switch theme."
  (interactive)
  (if (tpm--islocal)
      (progn
        (if (> (length tpm-themes-private) 1)
            (tpm--step direction tpm-themes-private)
          (message "%s" "Theme Park: You need to have at least two themes tagged.")))
    (tpm--step direction tpm-themes)))

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
  (if (eq tpm-auto-save t)
      (customize-save-variable 'tpm-tagged tpm-tagged))
  (theme-park-mode -1)
  (message "%s" "Theme Park: Bye bye!"))

(defun tpm--start-over ()
  "Start over."
  (interactive)
  (tpm--reset)
  (message "%s" "Theme Park: Starting over."))

(defun tpm--tag-add (thm)
  "Add theme to tagged list."
  (customize-set-variable 'tpm-tagged (add-to-list 'tpm-tagged thm))
  (message "Tagged \"%s\" for inclusion." thm))

(defun tpm--tag ()
  "Tag current theme for inclusion."
  (interactive)
  (let ((thm (tpm--cthm)))
    (if (tpm--islocal)
        (if (not thm)
            (message "%s" "Theme Park: Nothing to do.")
          (if (member thm tpm-tagged)
              (when (y-or-n-p (format "Do you want to remove %s from local list?" thm))
                (tpm--tag-pop))
            (tpm--tag-add thm)))
      (if (not thm)
          (message "%s" "Theme Park: You need to view a theme first.")
        (tpm--tag-add thm)))))

(defun tpm--tag-pop ()
  "Remove current theme from list of tagged themes."
  (interactive)
  (let ((current (tpm--cthm)))
    (if (not current)
        (message "%s" "Theme Park: Nothing to do.")
      (progn
        (customize-set-variable 'tpm-tagged (remove current tpm-tagged))
        (setq tpm-themes-private (remove current tpm-themes-private))
        (message "Removed \"%s\" from local list." current)))))

(defun tpm--show-tagged ()
  "Display list of tagged themes."
  (interactive)
  (if (not tpm-tagged)
      (message "%s" "Theme Park: No themes tagged yet.")
    (let ((lst nil)
          (thms tpm-tagged))
      (while thms
        (let ((thm (car thms)))
          (if (not lst)
              (setq lst (format "%s" thm))
            (setq lst (concat lst (format ", %s" thm)))))
        (setq thms (cdr thms)))
      (message "Tagged themes: %s" lst))))

(defun tpm--toggle-mode ()
  "Toggle between global / local mode."
  (interactive)
  (let ((--do-it (lambda ()
                   (tpm--reset)
                   (message "Theme Park: %s mode enabled." tpm-mode))))
    (if (tpm--isglobal)
        (if (> (length tpm-tagged) 1)
            (progn
              (setq tpm-mode 'local)
              (funcall --do-it))
          (message "%s" "Theme Park: You need to tag at least two themes."))
      (progn
        (setq tpm-mode 'global)
        (funcall --do-it)))))

(defun tpm--save-tagged ()
  "Save tpm-tagged customization variable."
  (interactive)
  (customize-save-variable 'tpm-tagged tpm-tagged)
  (message "%s" "Theme Park: Tagged themes saved!"))

;;;###autoload
(define-minor-mode theme-park-mode
  "Theme park mode."
  :lighter " TP"
  :keymap tpm-mode-map
  (if theme-park-mode
      (tpm--initialize)))

(provide 'theme-park-mode)

;;; theme-park-mode.el ends here

