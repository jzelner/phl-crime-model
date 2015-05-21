all: outline.tex outline.pdf

outline.tex : outline.Rnw
	Rscript -e "require(knitr); knit('outline.Rnw')"

outline.pdf : outline.tex
	latexmk -pdf -pvc -f outline.tex
