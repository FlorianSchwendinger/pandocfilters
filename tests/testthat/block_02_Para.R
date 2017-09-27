context("Para")

test_that("Para", {

    ## Test Str with Para
    x <- pandocfilters:::test(list(Para(list(Str("Hello R!")))))
    expect_that(x, equals("<p>Hello R!</p>"))
    x <- pandocfilters:::test(list(Para(Str("Hello R!"))))
    expect_that(x, equals("<p>Hello R!</p>"))

} )
