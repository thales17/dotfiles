publish: tangle
publish:
	@emacs --batch -Q --script build-site.el
tangle:
	@cd notes && ../scripts/tangle.sh orgmode.org

upload:
	@scripts/upload.sh

serve:
	@cd public && python -m http.server
