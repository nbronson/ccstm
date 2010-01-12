# workflows:
#
#  %.dia [dia]-> %.eps [ps2pdf]-> %.pdf
#  %.dot [dot]-> %.pdf [pdftops]-> %.eps
#  %.gnumeric [save as image from gnumeric]-> %.pdf [pdftops]-> %.eps

#BASE=ppopp207-bronson

TEX_SRCS=\
        intro.tex \
        library.tex \
	paper.tex \
        ref.tex \
#
TEX_DEPS=$(TEX_SRCS) \
	../../common/ppl.bib \
	sigplanconf.cls \
#

# expecting .dia
DIA_SRCS=\
#

# expecting .mp
MP_SRCS=\
	refs_class_uml.mp
#

# expecting .dot
DOT_SRCS=\
#

# expecting .pdf
GNUMERIC_SRCS=\
#

# expecting .pdf
PDF_SRCS=\
#

PDF_INPUT_FILES=$(DIA_SRCS:%.dia=%.pdf) $(MP_SRCS:%.mp=%.pdf) $(DOT_SRCS:%.dot=%.pdf) $(GNUMERIC_SRCS:%.ps=%.pdf) $(PDF_SRCS)
PDF_INPUT_PATHS=$(PDF_INPUT_FILES:%=build/%)
EPS_INPUT_PATHS=$(PDF_INPUT_PATHS:%.pdf=%.eps)

TEX_OPTIONS=-output-directory build

PDFLATEX=pdflatex $(TEX_OPTIONS)
BIBTEX=TEXMFOUTPUT=build bibtex
LATEX=latex $(TEX_OPTIONS)

.PHONY: default
default: all

.PHONY: all
all: build/paper.pdf
#all: $(BASE).tex $(BASE).pdf $(BASE).ps
#
#$(BASE).tex: build/indexable.tex
#	cp $< $@
#
#$(BASE).pdf: build/paper.pdf
#	cp $< $@
#
#$(BASE).ps: build/paper.ps
#	cp $< $@

build/paper.pdf: $(TEX_DEPS) $(PDF_INPUT_PATHS)
	mkdir -p build
	@echo
	@echo
	@echo PDF_INPUT_PATHS=$(PDF_INPUT_PATHS)
	@echo
	@echo
	@echo
	@echo ----- $(PDFLATEX) 1... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- SKIPPING $(BIBTEX)... -----
	@echo SKIPPING $(BIBTEX) build/`basename $@ .pdf`
	rm -f build/paper.bbl
	touch build/paper.bbl
	@echo ----- $(PDFLATEX) 2... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- $(PDFLATEX) 3... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- done -----

build/paper.dvi: $(TEX_DEPS) $(EPS_INPUT_PATHS)
	mkdir -p build
	@echo ----- $(LATEX) 1... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- SKIPPING $(BIBTEX)... -----
	@echo SKIPPING $(BIBTEX) build/`basename $@ .pdf`
	rm -f build/paper.bbl
	touch build/paper.bbl
	@echo ----- $(LATEX) 2... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- $(LATEX) 3... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- done -----

build/paper.ps: build/paper.dvi
	dvips -o $@ $<

build/%.eps: from_dia/%.dia
	mkdir -p build
	dia --export=$@ $<

build/%.pdf: from_mp/%.mp
	mkdir -p build
	cp $< build
	( cd build && mptopdf `basename $<` && mv `basename $< .mp`-1.pdf `basename $@` )

build/%.pdf: from_dot/%.dot
	mkdir -p build
	dot -Tpdf -o$@ $<

build/%.eps: from_gnumeric/%.ps
	mkdir -p build
	ps2epsi $< $@

# pdftrimwhite is suggested to make these
build/%.pdf: from_pdf/%.pdf
	mkdir -p build
	cp $< $@

build/%.eps: build/%.pdf
	pdftops -eps $< $@

build/%.pdf: build/%.eps
	ps2pdf -dEPSCrop $< $@

# ACM index needs the raw TEX and the *used* bibliography BBL
build/indexable.tex: $(TEX_SRCS) build/paper.pdf
	cat $(TEX_SRCS) build/paper.bbl > $@

.PHONY: clean
clean:
	rm -rf build *~

.PHONY: wordcount
wordcount:
	detex paper.tex | wc -w

grammar.txt: paper.tex
	sed -e 's/~\\ref{[^}]*}/ 42/g' -e 's/~\\cite{[^}]*}//g' paper.tex | detex | uniq | tr '[\n]' '[\\]' | sed 's/\([^\\]\)\\\([^\\]\)/\1 \2/g' | tr '[\\]' '[\n]' > grammar.txt
