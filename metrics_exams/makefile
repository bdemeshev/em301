# makefile: Rnw -> tex -> pdf
# .Rnw extension is automatically added
file_name = metrics_exams

$(file_name).pdf: $(file_name).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name).tex

	# clean auxillary files
	latexmk -c $(file_name).tex

$(file_name).tex : $(file_name).Rnw
	Rscript -e "library(knitr); knit('$(file_name).Rnw')"

clean:
	-rm $(file_name).tex
	-rm $(file_name).pdf $(file_name).log $(file_name).fls $(file_name).aux $(file_name).bbl $(file_name).bcf $(file_name).fdb_latexmk $(file_name).out $(file_name).run.xml $(file_name).toc $(file_name).amc $(file_name)-concordance.tex
	-rm junk.txt
	
