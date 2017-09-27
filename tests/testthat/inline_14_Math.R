context("Math")

test_that("Math", {

    inline <- Math("e^x")
    y <- if ( get_pandoc_version() < 1.16 ) "$e^x$" else "\\(e^x\\)"

    ## Test Math with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))), "latex")
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Plain(inline)), "latex")
    expect_that(x, equals(y))

} )
