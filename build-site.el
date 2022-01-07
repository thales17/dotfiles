;; Setup straight.el
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

(setq straight-base-dir ".packages/")

(straight-use-package 'htmlize)

(require 'ox-publish)

(setq org-html-validation-link nil)

(setq org-html-style
      (concat "<style>"
	      ".org-doc { color: blue; }"
	      ".org-keyword { color: red; }"
	      ".org-string { color: green; }"
	      ".org-constant { color: purple; }"
	      ".org-comment { color: darkcyan; }"
	      ".org-type { color: orange; font-weight: bold; }"
	      ".org-warning { color: red; }"
	      "</style>"))

(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      (list
       (list "my-org-site"
	     :recursive t
	     :base-directory "./"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html)))

(org-publish-all t)

(message "Build Complete")
