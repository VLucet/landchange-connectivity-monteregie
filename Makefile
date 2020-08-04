all: thesis.pdf

docs/index.html: outputs/final/*
	sh landcon.sh -g

thesis.pdf: thesis/thesis.tex thesis/chap_1.tex thesis/chap_2.tex thesis/appendix_1.tex thesis/figures/* docs/index.html
	pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& biber --output-directory thesis/build thesis \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex
