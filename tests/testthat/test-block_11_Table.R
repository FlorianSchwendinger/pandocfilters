context("Table")

test_that("Table", {

    M <- matrix(1:4, 2)
    T <- Table(M, col_names=c("A", "B"))
     
    x <- pandocfilters:::test(list(T), "markdown")
    y <- c("  A   B", "  --- ---", "  1   3", "  2   4", "", "")
    expect_that(x, equals(y))

} )
