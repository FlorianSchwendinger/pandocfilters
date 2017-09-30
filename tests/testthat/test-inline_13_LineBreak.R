context("LineBreak")

test_that("LineBreak", {

    ## FIX an api difference between pandoc 1.16 and lower
    j <- function(...) paste(..., collapse="")
    
    y <- j(c("<h1><br />", "</h1>"))
    inline <- LineBreak()

    ## Test LineBreak with Header
    x <- j(pandocfilters:::test(list(Header(list(inline)))))
    expect_that(x, equals(y))

    x <- j(pandocfilters:::test(list(Header(inline))))
    expect_that(x, equals(y))

    y <- j(c("<br />", ""))
    ## Test LineBreak with Plain
    x <- j(pandocfilters:::test(list(Plain(list(inline)))))
    expect_that(x, equals(y))

    x <- j(pandocfilters:::test(list(Plain(inline))))
    expect_that(x, equals(y))

} )
