
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

  '(package-selected-packages (quote (lua-mode typescript-mode go-playground))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load Path
(add-to-list 'load-path "~/code/dotfiles/emacs-plugins")

;; Always follow symlinks
(setq vc-follow-symlinks t)


;; Tabs
(setq default-tab-width 2)

;; Set files ending in ".p8" to open in lua-mode
(add-to-list 'auto-mode-alist '("\\.p8\\'" . lua-mode))

;; Xah Run Current File
(load "xah-run-current-file.el")
(global-set-key (kbd "<f8>") 'xah-run-current-file)

;; Auto reload file
(global-auto-revert-mode 1)

;; Web Development
(load "prettier-js.el")
(require 'prettier-js)

;; GUI Window Size
(if (display-graphic-p)
		(progn
			(setq initial-frame-alist
						'(
							(tool-bar-lines . 0)
							(width . 200) ; chars
							(height . 68) ; lines
							;;
							))
			(setq default-frame-alist
						'(
							(tool-bar-lines . 0)
							(width . 200)
							(height . 68)
							;;
							)))
	(progn
		(setq initial-frame-alist
					'(
						(tool-bar-lines . 0)))
		(setq default-frame-alist
					'(
						(tool-bar-lines . 0)))))

;; Load solarized
(load-theme 'solarized-light t)
