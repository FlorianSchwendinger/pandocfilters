q("no")
R

library(pandocfilters)

get_pandoc_version()
set_pandoc_version(1.5)

str(assignInNamespace)
assignInNamespace("pandoc", 1.16, getNamespace("pandocfilters"))
pandocfilters:::pandoc

get_pandoc_version()

getFromNamespace("pandoc_version", envir="pandocfilters")



