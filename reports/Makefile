filestem := outline

all: $(filestem).tex $(filestem).pdf

tex: $(filestem).tex

pdf: $(filestem).pdf

outline.tex : outline.Rnw
	$(info ************  CONVERTING KNITR TO TEX  ************)
	Rscript -e "require(knitr); knit('outline.Rnw')"

outline.pdf : outline.tex
	$(info ************  COMPILING PDF  ************)
	latex outline.tex
	bibtex outline.aux
	latex outline.tex
	pdflatex outline.tex
	$(info ************  CLEANING UP  ************)
	rm outline.aux outline.tex outline.bbl outline.blg outline.dvi outline.log

watch:
	watchman watch "$(shell pwd)"
	watchman -- trigger "$(shell pwd)" remake *.Rnw -- make pdf

unwatch:
	watchman watch-del "$(shell pwd)"


clean:
	rm $(filestem).pdf 

