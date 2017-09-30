context("Str")

test_that("Str", {

    y <- c("`lm(hello ~ world)`{#my_r_inline_code .R startFrom=\"0\"}",  
           "=======================================================")

    inline <- Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0)

    ## Test Str with Header
    x <- pandocfilters:::test(list(Header(list(inline))), "markdown")
    expect_that(x, equals(y))
    
    x <- pandocfilters:::test(list(Header(inline)), "markdown")
    expect_that(x, equals(y))

    ## Test Str with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))), "markdown")
    expect_that(x, equals(y[1]))

    x <- pandocfilters:::test(list(Plain(inline)), "markdown")
    expect_that(x, equals(y[1]))

} )
