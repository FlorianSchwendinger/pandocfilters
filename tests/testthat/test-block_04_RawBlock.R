context("BlockQuote")

test_that("BlockQuote", {

	block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

	y <- c("<blockquote>", "Hello R!", "</blockquote>")

    ## Test Str with BlockQuote
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
