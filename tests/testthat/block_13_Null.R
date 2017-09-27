context("Null")

test_that("Null", {

	block <- Null()
	y <- ""
    ## Test Null
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
