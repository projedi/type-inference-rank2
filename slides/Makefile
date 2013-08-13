slides.pdf: main.tex content.tex
	pdflatex -jobname=slides -halt-on-error main.tex
	pdflatex -jobname=slides -halt-on-error main.tex

clean:
	rm slides.pdf
	rm *.out *.aux *.log *.nav *.toc *.snm
