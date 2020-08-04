all: thesis/build/thesis.pdf

docs/index.html: docs/msc_thesis_figures.Rmd outputs/final/*.csv outputs/final/*.RDS
	sh landcon.sh -g

thesis/build/thesis.pdf: thesis/*.tex thesis/figures/*.png docs/index.html
	pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& biber --output-directory thesis/build thesis \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex
