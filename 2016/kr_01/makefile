# makefile: Rnw -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added

file_name_1 = kr_01_demo_1
file_name_2 = kr_01_var_1
file_name_3 = kr_01_demo_1_ans

all: $(file_name_1).pdf $(file_name_2).pdf $(file_name_3).pdf



$(file_name_1).pdf: $(file_name_1).tex metrics_test_bank.tex
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


$(file_name_2).pdf: $(file_name_2).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_2).tex

	# clean auxillary files
	latexmk -c $(file_name_2).tex

$(file_name_2).tex : $(file_name_2).Rnw
	Rscript -e "library(knitr); knit('$(file_name_2).Rnw')"


$(file_name_3).pdf: $(file_name_3).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_3).tex

	# clean auxillary files
	latexmk -c $(file_name_3).tex

$(file_name_3).tex : $(file_name_3).Rnw
	Rscript -e "library(knitr); knit('$(file_name_3).Rnw')"
