context("HorizontalRule")

test_that("HorizontalRule", {

	block <- HorizontalRule()

    ## Test Str with Plain
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals("<hr />"))
    

} )
