context("Link")

test_that("Link", {

    y <- "<h1><a href=\"https://cran.r-project.org/\" title=\"some title\">Text_Shown</a></h1>"
    inline <- Link("https://cran.r-project.org/", list(Str("Text_Shown")), "some title")

    ## Test Link with Header
    x <- pandocfilters:::test(list(Header(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Header(inline)))
    expect_that(x, equals(y))

    y <- "<a href=\"https://cran.r-project.org/\" title=\"some title\">Text_Shown</a>"
    ## Test Link with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_that(x, equals(y))

    x <- pandocfilters:::test(list(Plain(inline)))
    expect_that(x, equals(y))

} )
