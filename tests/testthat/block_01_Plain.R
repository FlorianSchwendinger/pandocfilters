context("Plain")

test_that("Plain", {

    ## Test Str with Plain
    x <- pandocfilters:::test(list(Plain(list(Str("Hello R!")))))
    expect_that(x, equals("Hello R!"))
    x <- pandocfilters:::test(list(Plain(Str("Hello R!"))))
    expect_that(x, equals("Hello R!"))

} )
