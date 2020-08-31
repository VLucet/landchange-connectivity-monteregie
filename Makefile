all: thesis/build/thesis.pdf

docs/index.html: docs/msc_thesis_figures.Rmd outputs/final/*.csv outputs/final/*.RDS
	sh landcon.sh -e figs -a

thesis/build/thesis.pdf: thesis/*.tex thesis/*.bib thesis/figures/*.png docs/index.html
	pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& biber --output-directory thesis/build thesis \
	&& biber --output-directory thesis/build thesis \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex \
	&& pdflatex -output-directory thesis/build thesis/thesis.tex

clean: 
	cd thesis/build/ \
	&& rm *.aux *.bbl *.bcf *.blg *.lof *.log *.lot *.out *.xml *.toc