context("Str")

test_that("Str", {

    ## Test Str with Header
    x <- pandocfilters:::test(list(Header(list(Str("Hello R!")))))
    expect_that(x, equals("<h1>Hello R!</h1>"))
    x <- pandocfilters:::test(list(Header(Str("Hello R!"))))
    expect_that(x, equals("<h1>Hello R!</h1>"))

    ## Test Str with Plain
    x <- pandocfilters:::test(list(Plain(list(Str("Hello R!")))))
    expect_that(x, equals("Hello R!"))
    x <- pandocfilters:::test(list(Plain(Str("Hello R!"))))
    expect_that(x, equals("Hello R!"))

} )
