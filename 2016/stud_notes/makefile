all: preclean pdf_noclean html epub mobi moveto_docs

preclean:
	export LC_ALL=en_US.UTF-8
	export LC_CTYPE=en_US.UTF-8
	rm -rf `biber --cache`
	-rm -rfv _book
	-rm stud_notes.run.xml
	-rm stud_notes.Rmd

html:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook")'
	cp -fvr images _book/
	cp -fvr css/style.css _book/

epub:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::epub_book")'

mobi: _book/_main.epub
	Rscript -e 'bookdown::kindlegen("_book/_main.epub")'


pdf:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")'

pdf_noclean:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", clean=FALSE)'

site:
	Rscript -e 'rmarkdown::render_site(encoding = "UTF-8")'
	# should test

install:
	Rscript -e 'devtools::install_github("bdemeshev/stud_notes_book")'
	# what the hell?

deploy:
	Rscript -e 'bookdown::publish_book(render="local", account="bdemeshev")'
	# what the hell?

copy:
	-rm -rfv ../../docs/notes_2016
	cp -rfv _book ../../docs/notes_2016


clean:
	Rscript -e "bookdown::clean_book(TRUE)"
	rm -fvr *.log

cleaner:
	make clean
	rm -frv *.aux *.out *.toc # Latex output
