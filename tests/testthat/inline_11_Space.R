context("Space")

test_that("Space", {

    y <- "<h1> </h1>"
    inline <- Space()

    ## Test Space with Header
    x <- pandocfilters:::test(list(Header(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Header(inline)))
    expect_that(x, equals(y))

    y <- " "
    ## Test Space with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Plain(inline)))
    expect_that(x, equals(y))

} )
