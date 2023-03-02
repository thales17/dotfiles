(setenv "LD_LIBRARY_PATH"
	(concat
	 (getenv "HOME")
	 "/.local/lib"))


(load "~/.emacs.d/ajr")

(setq x-alt-keysym 'meta)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alFh"))

(use-package simple
  :hook (text-mode . turn-on-visual-line-mode))

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package swiper
  :ensure t
  :init (ivy-mode)
  :bind (("C-M-s" . swiper)
	 ("C-c r " . ivy-resume)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x b" . 'counsel-switch-buffer)))

(use-package emms
  :ensure t
  :init (progn
	    (require 'emms-setup)
	    (emms-all)
	    (emms-default-players))
  :bind (("C-c m b" . emms-playlist-mode-go)
	 ("C-c m a" . ajr-play-album)
	 ("C-c m p" . emms-pause)
	 ("C-c m <right>" . emms-next)
	 ("C-c m <left>" . emms-previous)
	 ("C-c m i" . emms-show-all)
	 ("C-c m r" . ajr-play-random-album)))

(use-package company
  :ensure t
  :hook prog-mode)

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode
	 scheme-mode
	 lisp-mode
	 lisp-interaction-mode))

(use-package markdown-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package dart-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  :init (setq graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure t)

(use-package password-store
  :ensure t
  :bind ("C-c p c" . password-store-copy))

(use-package password-store-otp
  :ensure t
  :bind ("C-c p o" . password-store-otp-token-copy))

(use-package ledger-mode
  :ensure t)

(use-package notmuch
  :ensure t
  :bind (("C-c n n" . notmuch)
	 ("C-c n u" . ajr-sync-mail)))

(use-package eglot
  :init
  (unbind-key (kbd "C-c f"))
  :bind (("C-c e a" . eglot-code-actions)
	 ("C-c f n" . flymake-goto-next-error)
	 ("C-c f p" . flymake-goto-prev-error)
	 ("C-c f a" . flymake-show-project-diagnostics)))

(use-package ef-themes
  :ensure t)

(use-package slime
  :ensure t
  :demand t
  :bind (("C-x l" . slime-repl)))

(setq org-capture-templates
	  '(("t" "TODO")
	    ("ti" "Misc TODO" entry
	     (file+headline "todos.org" "Misc")
	     "* TODO %?\n"
	     :prepend t)))

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-hide-emphasis-markers nil)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'content)
  (org-todo-keyword-faces '(("IN PROGRESS" . "cyan")))
  (org-todo-keywords '((sequence "TODO" "IN PROGRESS" "DONE")))
  (org-ditaa-jar-path "~/.local/lib/ditaa0_9.jar")
  (org-directory "~/org")
  (org-agenda-files '("~/org"))
  (org-babel-load-languages
   '((latex . t)
     (shell . t)
     (ditaa . t)
     (C . t)
     (dot . t)
     (lisp . t)
     (emacs-lisp . t)
     (plantuml . t))))

(use-package project
  :custom
  (project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-find-dir "Find directory" nil)
     (project-shell "Shell" nil)
     (project-compile "Compile" nil)
     (magit-project-status "Magit" 109))))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-<return>" . hs-toggle-hiding)))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(defun ajr-disable-tabs ()
  (setq indent-tabs-mode nil))

(use-package artist
  :hook (artist-mode . ajr-disable-tabs))

(use-package js
 :hook (js-mode . ajr-disable-tabs))

(use-package gruvbox-theme
  :ensure t)

(use-package elfeed
  :ensure t
  :bind (("<f11>" . elfeed)
	 :map elfeed-search-mode-map
	 ("a" . ajr-elfeed-search-mark-read)
	 ("w" . ajr-elfeed-search-star)
	 ("W" . ajr-elfeed-search-star-filter)
	 :map elfeed-show-mode-map
	 ("w" . ajr-elfeed-show-star))
  :custom
  (elfeed-sort-order 'ascending))

(use-package elfeed-org
  :ensure t
  :init (progn
	  (elfeed-org)))

(defun ajr-before-save ()
  (whitespace-cleanup)
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(add-hook 'before-save-hook 'ajr-before-save)

(require 'epg)
(setq epg-pinentry-mode 'loopback)

(global-set-key (kbd "<f5>") 'redraw-display)
(global-set-key (kbd "<f6>") 'ajr-scratch)
(global-set-key (kbd "<f7>") 'shell)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'whitespace-mode)
(global-set-key (kbd "M-o") 'other-window)

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ajr-ask-before-closing))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
