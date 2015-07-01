filestem := self_exciting_violent_crime

all: $(filestem).tex $(filestem).pdf

tex: $(filestem).tex

pdf: $(filestem).pdf

$(filestem).tex : $(filestem).Rnw
	$(info ************  CONVERTING KNITR TO TEX  ************)
	Rscript -e "require(knitr); knit('$(filestem).Rnw')"

$(filestem).pdf : tex
	$(info ************  COMPILING PDF  ************)
	latex $(filestem).tex
	bibtex $(filestem).aux
	latex $(filestem).tex
	pdflatex $(filestem).tex
	$(info ************  CLEANING UP  ************)
	rm $(filestem).aux $(filestem).tex $(filestem).bbl $(filestem).blg $(filestem).dvi $(filestem).log

watch:
	watchman watch "$(shell pwd)"
	watchman -- trigger "$(shell pwd)" remake *.Rnw -- make pdf

unwatch:
	watchman watch-del "$(shell pwd)"


clean:
	rm $(filestem).pdf 

