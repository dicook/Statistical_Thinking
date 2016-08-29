oneclick:
	Rscript -e 'library(knitr); knit("index.Rmd")';
	pandoc -s --mathjax -t slidy --css ../myslidy.css index.md -o index.html;
	google-chrome index.html &
