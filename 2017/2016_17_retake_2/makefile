# makefile: Rnw -> tex -> pdf

file_name_1 = 2016_17_retake_2

all: $(file_name_1).pdf

$(file_name_1).pdf: $(file_name_1).tex retake_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_1).tex

	# clean auxillary files
	latexmk -c $(file_name_1).tex

$(file_name_1).tex : $(file_name_1).Rnw
	Rscript -e "library(knitr); knit('$(file_name_1).Rnw')"


clean:
	-rm *.aux *.amc *.log *.fls *.pdf *.fdb_latexmk
