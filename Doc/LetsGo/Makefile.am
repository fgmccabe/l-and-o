# Build the pdf etc of the reference manual

SUFFIXES = .tex

SUBDIRS = Samples

LETSGO_SRCS = letsgo.tex preface.tex first.tex directory.tex dance.tex \
	actions.tex types.tex expressions.tex goals.tex grammar.tex lo.tex packages.tex\
	debugging.tex install.tex strips.tex

EXTRA_DIST = $(LETSGO_SRCS) letsgo.pdf letsgo.glo prog.sty nap.sty manfnt.sty\
	     bussproofs.sty

all-local: letsgo.pdf

letsgo.pdf: ${LETSGO_SRCS} ../LogicBib.bib 
	pdflatex ${srcdir}/letsgo.tex
	bibtex ${srcdir}/letsgo
	makeindex ${srcdir}/letsgo.idx
	makeindex letsgo.glo -s nomencl.ist -o letsgo.gls
	pdflatex ${srcdir}/letsgo.tex

CLEANFILES = *.aux *.log *.gls letsgo.pdf

install-data-local: letsgo.pdf
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/Doc
	$(INSTALL_SCRIPT) letsgo.pdf $(DESTDIR)${prefix}/Doc
