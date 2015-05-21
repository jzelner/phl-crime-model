all: outline.tex outline.pdf

tex: outline.tex

pdf: outline.pdf

outline.tex : outline.Rnw
	Rscript -e "require(knitr); knit('outline.Rnw')"

outline.pdf : outline.tex
	latex outline.tex
	bibtex outline.aux
	latex outline.tex
	pdflatex outline.tex
	rm outline.aux outline.tex outline.bbl outline.blg outline.dvi outline.fls outline.log

clean:
	rm outline.pdf 
