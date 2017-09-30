context("Block Combine")

test_that("Block Combine", {

    ## Test Str with Plain
    x <- Header("Hello")
    y <- Plain("R")
    z <- Plain("!")
    expect_that(pandocfilters:::test(c(x, y, z)), equals(c("<h1>Hello</h1>", "R", "!")))

} )
