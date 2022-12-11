(setenv "LD_LIBRARY_PATH"
	(concat
	 (getenv "HOME")
	 "/.local/lib"))

(require 'dired)
(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(setq x-alt-keysym 'meta)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

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
	 ("C-x C-f" . 'counsel-find-file)
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
	 ("C-c m <left>" . emms-previous)))

(use-package company
  :ensure t
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (lisp-mode . paredit-mode)
	 (lisp-interaction-mode . paredit-mode)))

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
  :bind (("C-c e a" . eglot-code-actions)))

(use-package ef-themes
  :ensure t)

(use-package slime
  :ensure t
  :bind (("C-x l" . slime-repl)))

(use-package flymake
  :bind (("C-c f n" . flymake-goto-next-error)
	 ("C-c f p" . flymake-goto-prev-error)
	 ("C-c f a" . flymake-show-project-diagnostics)))

(use-package org
  :init
  (progn
    (setq org-capture-templates
	  '(("t" "TODO")
	    ("ti" "Misc TODO" entry
	     (file+headline "todos.org" "Misc")
	     "* TODO %?\n"
	     :prepend t))))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

(defun ajr-before-save ()
  (whitespace-cleanup)
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(add-hook 'before-save-hook 'ajr-before-save)

(defun ajr-prog-hook ()
  (display-line-numbers-mode)
  (hs-minor-mode)
  (hl-line-mode))

(add-hook 'prog-mode-hook 'ajr-prog-hook)

(load "~/.emacs.d/ajr-1")

(global-set-key (kbd "<f6>") 'ajr-scratch)
(global-set-key (kbd "<f7>") 'shell)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'whitespace-mode)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-<return>") 'hs-toggle-hiding)

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ajr-ask-before-closing))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x w") 'webjump)
(global-set-key (kbd "C-c f") 'find-file-at-point)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(add-hook 'artist-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
