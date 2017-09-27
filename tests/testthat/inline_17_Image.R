context("Image")

test_that("Image", {

    y <- "<h1><img src=\"https:://Rlogo.jpg\" title=\"fig:some_caption\" alt=\"some_text\" /></h1>"
    inline <- Image("https:://Rlogo.jpg", list(Str("some_text")), "fig:some_caption")

    ## Test Image with Header
    x <- pandocfilters:::test(list(Header(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Header(inline)))
    expect_that(x, equals(y))

    y <- "<img src=\"https:://Rlogo.jpg\" title=\"fig:some_caption\" alt=\"some_text\" />"
    ## Test Image with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Plain(inline)))
    expect_that(x, equals(y))

} )
