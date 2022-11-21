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

(let ((map dired-mode-map))
  (define-key map (kbd "C-b") #'bongo-dired-append-enqueue-lines))

(setq x-alt-keysym 'meta)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'magit)
(straight-use-package 'counsel)
(straight-use-package 'hydra)
(straight-use-package 'ivy-hydra)
(straight-use-package 'ace-window)
(straight-use-package 'avy)
(straight-use-package 'markdown-mode)
(straight-use-package 'geiser)
(straight-use-package 'geiser-guile)
(straight-use-package 'bongo)
(straight-use-package 'speed-type)
(straight-use-package 'paredit)
(straight-use-package 'password-store)
(straight-use-package 'password-store-otp)
(straight-use-package 'lua-mode)
(straight-use-package 'notmuch)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'yafolding)
(straight-use-package 'coterm)
(straight-use-package 'pulseaudio-control)
(straight-use-package 'yaml-mode)
(straight-use-package 'ledger-mode)
(straight-use-package 'slime)
(straight-use-package 'sunshine)
(straight-use-package 'gnuplot)
(straight-use-package 'graphviz-dot-mode)
(straight-use-package 'go-mode)
(straight-use-package 'ob-go)
(straight-use-package 'olivetti)
(straight-use-package 'auctex)
(straight-use-package 'lsp-mode)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'lsp-ui)
(straight-use-package 'sudoku)
(straight-use-package 'plantuml-mode)
(straight-use-package 'kotlin-mode)
(straight-use-package 'ef-themes)
(straight-use-package 'restclient)
(straight-use-package 'which-key)
(straight-use-package 'dart-mode)
(straight-use-package 'eglot)

(load "~/.emacs.d/ajr-1")

(yas-global-mode)
(yafolding-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'dart-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(coterm-mode)
(which-key-mode 1)

(require 'flymake)

(ivy-mode)

(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key (kbd "C-M-j") 'avy-goto-char)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(global-set-key (kbd "<f6>") 'ajr-scratch)
(global-set-key (kbd "<f7>") 'shell)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "<f9>") 'whitespace-mode)
(global-set-key (kbd "<f10>") 'whitespace-cleanup)
(global-set-key (kbd "<f12>") 'comment-dwim)

(global-set-key (kbd "C-c n n") 'notmuch)
(global-set-key (kbd "C-c n u") 'ajr-sync-mail)

(global-set-key (kbd "C-c p c") 'password-store-copy)
(global-set-key (kbd "C-c p o") 'password-store-otp-token-copy)

(define-key global-map (kbd "C-c m") (make-sparse-keymap))
(global-set-key (kbd "C-c m b") 'bongo-playlist)
(global-set-key (kbd "C-c m a") 'ajr-bongo-play-album)
(global-set-key (kbd "C-c m p") 'bongo-pause/resume)
(global-set-key (kbd "C-c m <right>") 'bongo-next)
(global-set-key (kbd "C-c m <left>") 'bongo-previous)
(global-set-key (kbd "C-c m c") 'ajr-podcast-dired)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x o") 'ace-window)

(global-set-key (kbd "C-<return>") 'yafolding-toggle-element)

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'ajr-ask-before-closing))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x w") 'webjump)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-x l") 'slime-repl)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(define-key flymake-mode-map (kbd "C-c f") (make-sparse-keymap))
(define-key flymake-mode-map (kbd "C-c f n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c f p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c f a") 'flymake-show-project-diagnostics)

(require 'eglot)
(define-key eglot-mode-map (kbd "C-c e") (make-sparse-keymap))
(define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "TODO")
	("ti" "Misc TODO" entry
	 (file+headline "todos.org" "Misc")
	 "* TODO %?\n"
	 :prepend t)))

(defun ajr-eglot-format-before-save ()
  (whitespace-cleanup)
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(add-hook 'before-save-hook 'ajr-eglot-format-before-save)

(require 'bongo)

(add-hook 'artist-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq common-lisp-hyperspec-root
  (concat "file://" (expand-file-name "~/docs/HyperSpec/")))

(require 'hydra)

(require 'ivy-hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(straight-use-package '(asm-blox :host github :repo "zkry/asm-blox"))

(setq graphviz-dot-indent-width 4)

(defun ajr-eww-setup ()
  (olivetti-mode)
  (text-scale-mode)
  (text-scale-increase 3))

(add-hook 'eww-mode-hook #'ajr-eww-setup)

(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
