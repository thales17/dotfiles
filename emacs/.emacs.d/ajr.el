;; -*- lexical-binding: t -*-


;;; Helpers for creating easy startup and shutdown method for Common
;;; Lisp projects

;;; Examples
;; (ajr--make-cl-lifecycle "chaos-dimension"
;;			"~/code/chaos_dimension/"
;;			"server.lisp"
;;			"setup.lisp"
;;			"chaos-dimension")

;; (ajr--make-cl-lifecycle "lisp-games"
;;			"~/code/lisp-games/lisp/"
;;			"example.lisp"
;;			"setup.lisp"
;;			"lisp-games")


(let ((started nil))
  (defun ajr--cl-startup (startup-file setup-file package)
    (unless started
      (find-file startup-file)
      (save-excursion
	(slime)
	(sleep-for 1)
	(find-file setup-file)
	(slime-eval-buffer)
	(sleep-for 1))
      (switch-to-buffer "*slime-repl sbcl*")
      (other-window 1)
      (slime-repl-set-package package)
      (setq started t)))

  (defun ajr---cl-shutdown (startup-file)
    (when started
      (slime-repl-sayoonara)
      (sleep-for 1)
      (save-excursion
	(find-file startup-file)
	(project-kill-buffers t))
      (setq started nil))))

(defmacro ajr--make-cl-lifecycle (name project-dir startup-file setup-file package)
  `(progn (fset ',(intern (concat name "-startup"))
	  (lambda ()
	    (interactive)
	    (ajr--cl-startup ,(concat (expand-file-name project-dir)
				      startup-file)
			     ,(concat project-dir
				      setup-file)
			     ,package)))

	  (fset ',(intern (concat name "-shutdown"))
		   (lambda ()
		     (interactive)
		     (ajr---cl-shutdown ,(concat (expand-file-name project-dir)
						 startup-file))))))