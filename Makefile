all: render

render:
	Rscript -e "rmarkdown::render('slides.Rmd')"

styler:
	Rscript -e "styler::style_file('slides.Rmd')"

clean:
	rm slides.pdf
	rm -rf knitr_output
