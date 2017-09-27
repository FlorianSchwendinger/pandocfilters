context("RawInline")

test_that("RawInline", {

    y <- c("some RawInline", "==============")
    inline <- RawInline("latex", "some RawInline")

    ## Test Space with Header
    x <- pandocfilters:::test(list(Header(list(inline))), "markdown")
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Header(inline)), "markdown")
    expect_that(x, equals(y))

    ## Test Space with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))), "markdown")
    expect_that(x, equals(y[1]))

    x <- pandocfilters:::test(list(Plain(inline)), "markdown")
    expect_that(x, equals(y[1]))

} )
