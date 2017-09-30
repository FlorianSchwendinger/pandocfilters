context("Div")

test_that("Div", {

	blocks <- list(Plain(list(Str("Hello R!"))))
	block <- Div(blocks)

    ## Test Div
    x <- pandocfilters:::test(list(block))
    y <- c("<div>", "Hello R!", "</div>")
    expect_that(x, equals(y))

} )
