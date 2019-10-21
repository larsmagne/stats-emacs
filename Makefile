dist:
	rsync -av ~/src/stats-emacs/*.js ~/src/stats-emacs/stats-emacs.html  www@quimby:html/circus/stats-emacs/

build:
	emacs -nw -batch -l stats-emacs.el -f stats-emacs
