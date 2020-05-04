build:
	pdflatex thesis.tex
	bibtex thesis
	pdflatex thesis.tex

build_auto:
	pdflatex -interaction=nonstopmode thesis.tex
	bibtex thesis
	pdflatex -interaction=nonstopmode thesis.tex

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.out
	rm -f thesis-blx.bib
	rm -f *.bbl
	rm -f *.bcf
	rm -f *.blg
	rm -f *.toc
	rm -f *.run.xml
	rm -f texput.log

run:
	./run.sh
