all: outline.tex outline.pdf

tex: outline.tex

pdf: outline.pdf

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
clean:
	rm outline.pdf 
