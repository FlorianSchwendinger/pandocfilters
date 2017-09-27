q("no")

library(pandocfilters)

doc <- document()
nrecs <- 1e6
ncol <- 100
x <- sample(1:10, nrecs, T)
M <- matrix(x, ncol)
doc$append_table(M)

writeLines(doc$to_json(), "big_table.json")
cmd <- sprintf("pandoc %s  -t html5 -o %s", "big_table.json", "big_table.html")
system(cmd)


pandocfilters_writer

writer <- function(x, con, format) {
    ##cmd <- sprintf("pandoc -f json -t %s > %s", shQuote(as.character(x)), con)
    print(con)
    system("pwd")
    ##system(cmd)
}

doc$write("big_table.html", format="html", writer=writer)

getwd()

