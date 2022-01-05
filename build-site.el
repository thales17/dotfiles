(require 'ox-publish)

(setq org-html-validation-link nil)

(setq org-publish-project-alist
      (list
       (list "my-org-site"
	     :recursive t
	     :base-directory "./"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html)))

(org-publish-all t)

(message "Build Complete")
