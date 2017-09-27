
b <- c(require("testthat", quietly = TRUE), require("pandocfilters", quietly = TRUE))
if( all(b) ) {
    test_check("pandocfilters")
}

