# makefile: Rnw -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added

file_name_1 = demo_a
file_name_2 = demo_b
file_name_3 = combat_v1
file_name_4 = combat_v2
file_name_5 = combat_v3
file_name_6 = combat_v4


all: $(file_name_1).pdf $(file_name_2).pdf $(file_name_3).pdf $(file_name_4).pdf $(file_name_5).pdf $(file_name_6).pdf



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



$(file_name_2).pdf: $(file_name_2).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name_2).tex

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
	latexmk -xelatex -pdf $(file_name_3).tex

	# clean auxillary files
	latexmk -c $(file_name_3).tex

$(file_name_3).tex : $(file_name_3).Rnw
	Rscript -e "library(knitr); knit('$(file_name_3).Rnw')"




$(file_name_4).pdf: $(file_name_4).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name_4).tex

	# clean auxillary files
	latexmk -c $(file_name_4).tex

$(file_name_4).tex : $(file_name_4).Rnw
	Rscript -e "library(knitr); knit('$(file_name_4).Rnw')"
	
	
$(file_name_5).pdf: $(file_name_5).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name_5).tex

	# clean auxillary files
	latexmk -c $(file_name_5).tex

$(file_name_5).tex : $(file_name_5).Rnw
	Rscript -e "library(knitr); knit('$(file_name_5).Rnw')"
	
	
$(file_name_6).pdf: $(file_name_6).tex metrics_test_bank.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name_6).tex

	# clean auxillary files
	latexmk -c $(file_name_6).tex

$(file_name_6).tex : $(file_name_6).Rnw
	Rscript -e "library(knitr); knit('$(file_name_6).Rnw')"
