.POSIX:
EMACS = emacs
MAKEINFO = makeinfo
INSTALLINFO = install-info
MV = mv
RM = rm
MANUAL_HTML_ARGS =--html --no-split --footnote-style=separate --css-ref=resources/manual.css

## Build #############################################################

.PHONY: gh-html
gh-html: index.html clean

.PHONY: all
all:  index.html org-remark.info clean

index.html: org-remark.texi
	@printf "\n\n### Generating manual .html files \n\n"
	$(MAKEINFO) $(MANUAL_HTML_ARGS) $< -o index.html

org-remark.texi: org-remark-manual.org
	@printf "\n\n### Generating manual .texi file \n\n"
	$(EMACS) --batch -L --file $< \
                 -f org-texinfo-export-to-texinfo

org-remark.info: org-remark.texi
	@printf "\n\n### Generating manual .info file \n\n"
	makeinfo org-remark.texi -o org-remark.info


.PHONY: clean
clean:
	@printf "\n\n### Clear .texi file \n\n"
	$(RM) org-remark.texi*
