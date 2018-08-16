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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(elfeed-feeds
   (quote
    (("https://www.reddit.com/r/fightsticks/.rss" gaming)
     ("https://www.reddit.com/r/tekken/.rss" gaming)
     ("https://www.reddit.com/r/StreetFighter/.rss" gaming)
     ("https://shoryuken.com/feed/" gaming)
     ("https://www.giantbomb.com/videos/feed/hd/?api_key=e7111fb512aa82c48cfe54f51b5a69029c93c57d" gaming)
     ("https://www.reddit.com/r/emulation/.rss" gaming)
     ("https://www.eurogamer.net/?format=rss&topic=digital_foundry" gaming)
     ("https://www.usgamer.net/rss" gaming)
     ("https://www.polygon.com/rss/index.xml" gaming)
     ("https://kotaku.com/rss" gaming)
     ("http://rss.cnn.com/rss/money_latest.rss" news)
     ("https://www.politico.com/rss/economy.xml" news)
     ("https://www.politico.com/rss/defense.xml" news)
     ("https://www.politico.com/rss/congress.xml" news)
     ("https://www.politico.com/rss/politics08.xml" news)
     ("https://www.theguardian.com/world/rss" news)
     ("https://www.npr.org/rss/rss.php?id=1001" news)
     ("https://www.phoronix.com/rss.php" tech)
     ("http://feeds.arstechnica.com/arstechnica/features" tech)
     ("http://feeds.arstechnica.com/arstechnica/index" tech)
     ("https://www.theverge.com/rss/index.xml" tech)
     ("https://www.reddit.com/r/golang/.rss" tech)
     ("https://www.reddit.com/r/linux/.rss" tech)
     ("https://www.reddit.com/r/emacs/.rss" tech)
     ("http://rss.cnn.com/rss/cnn_topstories.rss" news)
     ("https://www.slashfilm.com/feed/" film)
     ("https://www.reddit.com/r/pico8/.rss" tech)
     ("https://www.reddit.com/r/Louisville/.rss" news)
     ("https://www.reddit.com/r/politics/.rss" news)
     ("https://www.reddit.com/r/Angular2/.rss" tech)
     ("https://news.ycombinator.com/rss" tech))))
 
 '(fill-column 70)
 '(helm-follow-mode-persistent t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (elfeed-protocol elfeed dumb-jump helm-ag helm typescript-mode go-mode markdown-mode magit org runner lua-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Backup files and Autosave settings
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil)

;; Menubar
(menu-bar-mode -1)

;; Toolbar
(tool-bar-mode -1)

;; Tabs
(setq default-tab-width 2)
(setq-default tab-width 2)

;; Initial Buffer
(setq inhibit-startup-screen t
      initial-buffer-choice nil)
(setq initial-scratch-message "")

;; F1 for bookmark-list
(global-set-key (kbd "<f1>") 'helm-filtered-bookmarks)

;; Auto reload file
(global-auto-revert-mode -1)

;; Set files ending in ".p8" to open in lua-mode
(add-to-list 'auto-mode-alist '("\\.p8\\'" . lua-mode))

;(global-set-key (kbd "; Load Path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Xah Run Current File
(load "xah-run-current-file.el")
(global-set-key (kbd "<f8>") 'xah-run-current-file)

;; Show Parens
(show-paren-mode 1)

;; Turn off beeping
(setq bell-volume 0)

;; Increase font size
;;(set-face-attribute 'default nil :height 140)

;; Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/go/1.10.1/bin/"))

;; Go
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Indenting Tabs and Spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Helm / Searching
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-s") 'helm-ag-this-file)
(global-set-key (kbd "C-x C-b") 'helm-mini)

;; Line Numbers
(global-display-line-numbers-mode 1)

;; Dumb Jump
(dumb-jump-mode 1)
(global-set-key (kbd "C-o") 'dumb-jump-go)

;; Symlinks
(setq vc-follow-symlinks t)

;; Exec Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/adamrichardson/code/go/bin"))
(setenv "GOPATH" "/Users/adamrichardson/code/go")
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/adamrichardson/code/go/bin")))

