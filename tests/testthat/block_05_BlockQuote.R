context("BlockQuote")

test_that("BlockQuote", {

    y <- c("<blockquote>", "Hello R!", "</blockquote>")

    block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
