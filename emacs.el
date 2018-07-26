
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
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (wombat)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
	 (--map
		(solarized-color-blend it "#fdf6e3" 0.25)
		(quote
		 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
	 (quote
		(("#eee8d5" . 0)
		 ("#B4C342" . 20)
		 ("#69CABF" . 30)
		 ("#69B7F0" . 50)
		 ("#DEB542" . 60)
		 ("#F2804F" . 70)
		 ("#F771AC" . 85)
		 ("#eee8d5" . 100))))
 '(hl-bg-colors
	 (quote
		("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
	 (quote
		("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
	 (quote
		(org-projectile org-agenda-property yaml-mode exec-path-from-shell lua-mode typescript-mode go-playground)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#dc322f")
		 (40 . "#c85d17")
		 (60 . "#be730b")
		 (80 . "#b58900")
		 (100 . "#a58e00")
		 (120 . "#9d9100")
		 (140 . "#959300")
		 (160 . "#8d9600")
		 (180 . "#859900")
		 (200 . "#669b32")
		 (220 . "#579d4c")
		 (240 . "#489e65")
		 (260 . "#399f7e")
		 (280 . "#2aa198")
		 (300 . "#2898af")
		 (320 . "#2793ba")
		 (340 . "#268fc6")
		 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))

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
							(fullscreen . maximized)
							))
			(setq default-frame-alist
						'(
							(tool-bar-lines . 0)
							(fullscreen . maximized)
							)))
	(progn
		(setq initial-frame-alist
					'(
						(tool-bar-lines . 0)))
		(setq default-frame-alist
					'(
						(tool-bar-lines . 0)))))

;; Load solarized
;; (load-theme 'solarized-light t)

;; Initial Buffer
(setq initial-buffer-choice "~/code")

;; Exec path from shell
(exec-path-from-shell-initialize)

;; F1 for bookmark-list
(global-set-key (kbd "<f1>") 'list-bookmarks)
