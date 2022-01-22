# For ELPA, below should be added to the spec:
# ("org-remark"	:url "https://github.com/nobiot/org-remark"
#  :make "README-elpa"
#  :doc "docs/org-remark.org"
#  :auto-sync t)

README-elpa: README.org
	-emacs --batch $< -f org-ascii-export-to-ascii
	-mv README.txt README-elpa

.PHONY: test-compile
test-compile:
	emacs --batch --eval "(add-to-list 'load-path default-directory)" \
	      -f batch-byte-compile ./*.el
# Check declare-function
	emacs --batch --eval "(check-declare-directory default-directory)"

.PHONY: clean
clean:
	find . -name "*.elc" -delete

