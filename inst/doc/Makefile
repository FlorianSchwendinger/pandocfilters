
all: pandocfilters html pdf

html:
	pandoc -s README.md -o README.html

pdf:
	pandoc README.md -o README.pdf

pandocfilters:
	R -e "library(knitr); knitr::spin('README.R')"
