# makefile: Rnw -> tex -> pdf
# .Rnw extension is automatically added
file_name = ht_01_hrusha

$(file_name).pdf: $(file_name).tex
	latexmk -pdf $(file_name).tex # create pdf
	latexmk -c $(file_name).tex  # clean auxillary files
	rm $(file_name)-concordance.tex # remove concordance file

$(file_name).tex : $(file_name).Rnw
	Rscript -e "library(knitr); knit('$(file_name).Rnw')"
