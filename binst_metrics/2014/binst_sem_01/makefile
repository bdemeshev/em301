# makefile
# .Rmd extension is automatically added
fname = nano_research
header = header.tex

all: md word tex pdf html

md: $(fname).Rmd
	Rscript -e "library(knitr); knit('$(fname).Rmd')"

word: $(fname).md
	pandoc -s $(fname).md -o $(fname).docx

header: # will overwrite header.tex 
	echo "\usepackage[utf8]{inputenc}" > $(header) 
	echo "\usepackage[russian]{babel}" >> $(header) # add line

tex: $(fname).md # $(header)
	# pandoc -s --include-in-header=$(header) $(fname).md -o $(fname).tex
	pandoc -s $(fname).md -o $(fname).tex

pdf: $(fname).tex
	latexmk -pdf $(fname).tex # create pdf
	latexmk -c $(fname).tex  # clean auxillary files
	
html: $(fname).md
	pandoc -s $(fname).md -o $(fname).html