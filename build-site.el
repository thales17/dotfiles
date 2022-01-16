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
(setq org-html-postamble nil)

(setq org-html-style
      (concat "<style>"
	      ".org-doc { color: blue; }"
	      ".org-keyword { color: red; }"
	      ".org-string { color: green; }"
	      ".org-variable-name { color: black; font-weight: bold; }"
	      ".org-function-name { color: mediumblue; }"
	      ".org-constant { color: purple; }"
	      ".org-comment { color: darkcyan; }"
	      ".org-type { color: orange; font-weight: bold; }"
	      ".org-warning { color: red; }"
	      ".timestamp { color: black; font-size: smaller; }"
	      ".timestamp { text-decoration: underline; }"
	      ".timestamp:before { content: \"Last Updated: \"; }"
	      "</style>"))

(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      (list
       (list "org-site"
	     :recursive t
	     :base-directory "./"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html)
       (list "org-static"
	     :recursive t
	     :base-directory "./"
	     :base-extension "css\\|png\\|gif\\|jpg\\|jpeg"
	     :publishing-directory "./public"
	     :publishing-function 'org-publish-attachment)))

(org-publish-all t)

(message "Build Complete")
