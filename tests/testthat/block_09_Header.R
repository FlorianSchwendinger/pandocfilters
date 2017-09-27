context("Header")

test_that("Header", {

    ## Test Str with Header
    x <- pandocfilters:::test(list(Header(list(Str("Hello R!")))))
    expect_that(x, equals("<h1>Hello R!</h1>"))
    x <- pandocfilters:::test(list(Header(Str("Hello R!"))))
    expect_that(x, equals("<h1>Hello R!</h1>"))

} )
