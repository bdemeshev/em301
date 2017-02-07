# makefile: Rnw -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added

file_name_1 = midterm_combat_retake2
file_name_2 = midterm_combat_v_01
file_name_3 = midterm_combat_v_02
file_name_4 = midterm_combat_v_03
file_name_5 = midterm_combat_v_04
file_name_6 = kr_01_var_4
file_name_7 = kr_01_var_5
file_name_8 = kr_01_brutal
file_name_9 = kr_01_brutal_demo



all: $(file_name_1).pdf



$(file_name_1).pdf: $(file_name_1).tex retake2_test_bank.Rnw
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


$(file_name_2).pdf: $(file_name_2).tex metrics_test_bank.Rnw
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


$(file_name_3).pdf: $(file_name_3).tex metrics_test_bank.Rnw
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


$(file_name_4).pdf: $(file_name_4).tex metrics_test_bank.Rnw
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_4).tex

	# clean auxillary files
	latexmk -c $(file_name_4).tex

$(file_name_4).tex : $(file_name_4).Rnw
	Rscript -e "library(knitr); knit('$(file_name_4).Rnw')"


$(file_name_5).pdf: $(file_name_5).tex metrics_test_bank.Rnw
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_5).tex

	# clean auxillary files
	latexmk -c $(file_name_5).tex

$(file_name_5).tex : $(file_name_5).Rnw
	Rscript -e "library(knitr); knit('$(file_name_5).Rnw')"

$(file_name_6).pdf: $(file_name_6).tex metrics_test_bank.Rnw
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_6).tex

	# clean auxillary files
	latexmk -c $(file_name_6).tex

$(file_name_6).tex : $(file_name_6).Rnw
	Rscript -e "library(knitr); knit('$(file_name_6).Rnw')"


$(file_name_7).pdf: $(file_name_7).tex metrics_test_bank.Rnw
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_7).tex

	# clean auxillary files
	latexmk -c $(file_name_7).tex

$(file_name_7).tex : $(file_name_7).Rnw
	Rscript -e "library(knitr); knit('$(file_name_7).Rnw')"


clean:
	-rm *.aux *.amc *.log *.fls *.pdf *.fdb_latexmk
	-rm $(file_name_1).tex $(file_name_2).tex $(file_name_3).tex $(file_name_4).tex $(file_name_5).tex $(file_name_6).tex $(file_name_7).tex $(file_name_8).tex $(file_name_9).tex metrics_test_bank.tex


archive:
	zip -e kr_01_all *.pdf *.Rnw makefile linear_regression.png *.sty



$(file_name_8).pdf: $(file_name_8).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_8).tex

	# clean auxillary files
	latexmk -c $(file_name_8).tex

$(file_name_8).tex : $(file_name_8).Rnw
	Rscript -e "library(knitr); knit('$(file_name_8).Rnw')"




$(file_name_9).pdf: $(file_name_9).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_9).tex

	# clean auxillary files
	latexmk -c $(file_name_9).tex

$(file_name_9).tex : $(file_name_9).Rnw
	Rscript -e "library(knitr); knit('$(file_name_9).Rnw')"
