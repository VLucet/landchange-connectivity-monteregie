all: thesis.pdf

thesis.pdf: thesis/thesis.tex thesis/chap_1.tex thesis/chap_2.tex thesis/appendix_1.tex thesis/figures/*
	pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& biber -output-directory thesis/build thesis/thesis \
	&& biber -output-directory thesis/build thesis/thesis \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex
