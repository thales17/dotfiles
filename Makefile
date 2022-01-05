publish:
	@emacs --batch -Q --script build-site.el
	@mv -v public/README.html public/index.html
	@firefox public/index.html

upload:
	@./upload.sh
