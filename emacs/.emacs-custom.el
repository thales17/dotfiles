(setenv "SDL_VIDEODRIVER" "x11")

(setq inferior-lisp-program "/usr/local/bin/sbcl")


(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/docs/HyperSpec/")))

(load (expand-file-name "~/.emacs.d/ajr.el/ajr"))

(ajr--make-cl-lifecycle "chaos-dimension"
			"~/code/chaos_dimension/"
			"server.lisp"
			"setup.lisp"
			"chaos-dimension")

(ajr--make-cl-lifecycle "lisp-games"
			"~/code/lisp-games/lisp/"
			"example.lisp"
			"setup.lisp"
			"lisp-games")

(setq org-capture-templates
      '(("t" "TODO")
	("tp" "Personal TODO" entry
	 (file+headline "todos.org" "Personal")
	 "* TODO %?\n"
	 :prepend t)
	("tg" "Games TODO" entry
	 (file+headline "todos.org" "Games")
	 "* TODO %?\n"
	 :prepend t)
	("ts" "Setup TODO" entry
	 (file+headline "todos.org" "Setup")
	 "* TODO %?\n"
	 :prepend t)
	("tc" "Coding TODO" entry
	 (file+headline "todos.org" "Coding")
	 "* TODO %?\n"
	 :prepend t)
	("tm" "Music TODO" entry
	 (file+headline "todos.org" "Music")
	 "* TODO %?\n"
	 :prepend t)
	("tr" "Reading TODO" entry
	 (file+headline "todos.org" "Reading")
	 "* TODO %?\n"
	 :prepend t)
	("tw" "Writing TODO" entry
	 (file+headline "todos.org" "Writing")
	 "* TODO %?\n"
	 :prepend t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajr-emu-snes-program '("retroarch" "-L" "bsnes_mercury_accuracy_libretro.so"))
 '(ajr-music-dir "/home/adam/music")
 '(ajr-sync-mail-program "/home/adam/.local/bin/syncmail")
 '(ajr-video-dir "/home/adam/Videos")
 '(bongo-enabled-backends '(vlc))
 '(bongo-header-line-mode nil)
 '(bongo-prefer-library-buffers nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("25f79eb20039ac795a4b2b499c392934f424e59d75b1dd12680b340d962f4679" "9ae0e81ced7c8d587cb1db9fcb528856315d352822082518884e8726fe681d1d" "b330e75c3717166db3a8a41eb180c705b46b97c584dffdb917b310f86025c811" "d983956f195a1a5e920c78216e6c164f4fd73a4fdce3f532d527182409f96515" "0657bbbdf081bafc0b56d4d40cb333829eb576c8c519c8028f29afbf9559eb35" "3eb4031719479655814b5db031492570cdc7c82e37f34d7707515590c926980f" "92cfd42cedb42fdd3ea0d84d518825a94f29b30be20f65978dab7d7c8fa30c6a" "b69d8a1a142cde4bbde2f940ac59d2148e987cd235d13d6c4f412934978da8ab" "126d30c137a7e345193d7f77f5b2af92d9669ebf60ed81346c897dbe16f40376" "81006de2b57ea81ebf278277c61f8bdadbac4894f52f15220d932befea6e9839" "a8e9953f429517bd62a0bf136b081b436fd429ee1d445bc311d7eee83679d151" "f21756050d9a6cd931517b54356ffbce5a51e0cd15454199bf408254d6364963" "dc2790247fb4102399f17b6226bef2682ada45a9b0020661f168e4708964d3de" "e375c943dbc6cac4242684b7507ef97d30c4b4725614660c15b101cb50c66277" "616a43bd873b09e966e837c7138e5b2561b3442b92723d21b8c80166f3ecd9f3" "a068a281383f92b622a058ec29755c6ae9f226c5e444ed126c05f71ba17570e5" default))
 '(default-frame-alist '((fullscreen . maximized)))
 '(dired-listing-switches "-alFh")
 '(display-line-numbers-width nil)
 '(doc-view-resolution 200)
 '(elfeed-sort-order 'ascending)
 '(initial-buffer-choice t)
 '(large-file-warning-threshold nil)
 '(ledger-post-amount-alignment-column 58)
 '(ledger-reports
   '(("budget" "%(binary) --empty --no-total -S -T -f %(ledger-file) bal ^assets:budget")
     ("budget accounts" "%(binary) -f %(ledger-file) bal ^assets:checking ^assets:savings ^equity:budget ^liabilities:credit card ^assets:a money ^assets:k money")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(menu-bar-mode nil)
 '(modus-themes-bold-constructs t)
 '(modus-themes-headings '((1 1.4) (2 1.3) (3 nil 1.2) (4 semilight 1.1)))
 '(modus-themes-italic-constructs t)
 '(modus-themes-mode-line '(nil accented borderless 5))
 '(modus-themes-org-blocks 'gray-background)
 '(modus-themes-paren-match '(bold intense underline))
 '(modus-themes-subtle-line-numbers t)
 '(notmuch-archive-tags '("-unread" "-inbox"))
 '(notmuch-saved-searches
   '((:name "flagged" :query "tag:flagged" :key "f")
     (:name "emacs-devel" :query "emacs-devel@gnu.org and tag:unread" :key "e")
     (:name "misc openbsd" :query "misc@openbsd.org and tag:unread")
     (:name "guile-user" :query "guile-user@gnu.org and tag:unread")
     (:name "patreon" :query "patreon.com and tag:unread")
     (:name "announce openbsd" :query "announce@openbsd.org and tag:unread")
     (:name "jcps" :query "jefferson.kyschools.us and tag:unread")
     (:name "library" :query "lfpl and tag:unread")
     (:name "orgmode" :query "emacs-orgmode@gnu.org and tag:unread")))
 '(org-agenda-files '("~/org"))
 '(org-babel-load-languages
   '((latex . t)
     (shell . t)
     (ditaa . t)
     (C . t)
     (dot . t)
     (lisp . t)
     (emacs-lisp . t)
     (plantuml . t)))
 '(org-directory "~/org")
 '(org-hide-emphasis-markers t)
 '(org-src-preserve-indentation t)
 '(org-startup-folded 'content)
 '(org-todo-keyword-faces '(("IN PROGRESS" . "cyan")))
 '(org-todo-keywords '((sequence "TODO" "IN PROGRESS" "DONE")))
 '(pixel-scroll-precision-mode t)
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(scroll-bar-mode nil)
 '(send-mail-function 'smtpmail-send-it)
 '(shr-image-animate nil)
 '(smtpmail-smtp-server "127.0.0.1")
 '(smtpmail-smtp-service 25)
 '(sudoku-level 'easy)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight regular :height 120 :width normal))))
 '(variable-pitch ((t (:family "Noto Serif")))))
