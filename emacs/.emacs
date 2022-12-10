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
  :ensure)

(use-package yasnippet
  :ensure
  :config (yas-global-mode))

(use-package which-key
  :ensure
  :config (which-key-mode 1))

(use-package swiper
  :ensure
  :init (ivy-mode)
  :bind (("C-M-s" . swiper)
	 ("C-c r " . ivy-resume)))

(use-package counsel
  :ensure
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . 'counsel-find-file)
	 ("C-x b" . 'counsel-switch-buffer)))

(use-package emms
  :ensure
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
  :ensure
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))

(use-package paredit
  :ensure
  :init
  (progn
    (autoload 'enable-paredit-mode "paredit"
      "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)))

(use-package markdown-mode
  :ensure)

(use-package lua-mode
  :ensure)

(use-package yaml-mode
  :ensure)

(use-package go-mode
  :ensure)

(use-package dart-mode
  :ensure)

(use-package kotlin-mode
  :ensure)

(use-package plantuml-mode
  :ensure)

(use-package graphviz-dot-mode
  :ensure
  :init (setq graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure)

(use-package password-store
  :ensure
  :bind ("C-c p c" . password-store-copy))

(use-package password-store-otp
  :ensure
  :bind ("C-c p o" . password-store-otp-token-copy))

(use-package ledger-mode
  :ensure)

(use-package notmuch
  :ensure
  :bind (("C-c n n" . notmuch)
	 ("C-c n u" . ajr-sync-mail)))

(use-package eglot
  :init
  :bind (("C-c e a" . eglot-code-actions)))

(use-package ef-themes
  :ensure)

(use-package slime
  :ensure
  :bind (("C-x l" . slime-repl)))

(use-package flymake
  :bind (("C-c f n" . flymake-goto-next-error)
	 ("C-c f p" . flymake-goto-prev-error)
	 ("C-c f a" . flymake-show-project-diagnostics)))

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

;; (straight-use-package 'geiser)
;; (straight-use-package 'geiser-guile)
;; (straight-use-package 'ob-go)
;; (straight-use-package 'restclient)

(load "~/.emacs.d/ajr-1")

(global-set-key (kbd "<f6>") 'ajr-scratch)
(global-set-key (kbd "<f7>") 'shell)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'whitespace-mode)
(global-set-key (kbd "<f10>") 'whitespace-cleanup)
(global-set-key (kbd "<f12>") 'comment-dwim)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-<return>") 'hs-toggle-hiding)

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ajr-ask-before-closing))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x w") 'webjump)
(global-set-key (kbd "C-c f") 'find-file-at-point)


(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "TODO")
	("ti" "Misc TODO" entry
	 (file+headline "todos.org" "Misc")
	 "* TODO %?\n"
	 :prepend t)))

(add-hook 'artist-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
