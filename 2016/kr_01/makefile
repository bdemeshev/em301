# makefile: Rnw -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added

file_name_1 = kr_01_demo_1

$(file_name_1).pdf: $(file_name_1).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name_1).tex

	# clean auxillary files
	latexmk -c $(file_name_1).tex

$(file_name_1).tex : $(file_name_1).Rnw
	Rscript -e "library(knitr); knit('$(file_name_1).Rnw')"


