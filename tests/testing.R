q("no")
R

b <- c(require("testthat", quietly = TRUE), require("pandocfilters", quietly = TRUE))
get_pandoc_version()

if (all(b)) {
    for ( fn in dir("testthat") ) {
        print(fn)
        fp <- file.path("testthat", fn)
        source(fp)
    }
}


