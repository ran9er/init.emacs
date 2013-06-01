WEBFILES=README.html BUGS.html TODO.html .htaccess

web: $(WEBFILES)

webup: web
	scp $(WEBFILES) common-lisp.net:public_html/swankr/

README.html: README
	emacs --batch --visit README --load org-export.el

%.html: %.org
	emacs --batch --visit $^ --load org-export.el

clean:
	-rm $(WEBFILES)

.PHONY: web clean
