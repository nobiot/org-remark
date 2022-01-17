org-remark.org: docs/org-remark-manual.org
	-emacs --batch -L ../org-transclusion -l org-transclusion $< \
	       --eval '(progn (org-transclusion-add-all) (write-region nil nil "org-transclusion.org"))'

.PHONY: test-compile
test-compile:
	emacs --batch --eval "(add-to-list 'load-path default-directory)" \
	      -f batch-byte-compile ./*.el
	# Check declare-function
	emacs --batch --eval "(check-declare-directory default-directory)"

.PHONY: clean
clean:
	find . -name "*.elc" -delete

