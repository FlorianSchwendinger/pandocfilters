context("Strikeout")

test_that("Strikeout", {

    ## Test Strikeout with Header
    x <- pandocfilters:::test(list(Header(list(Strikeout(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><del>Hello R!</del></h1>"))
    x <- pandocfilters:::test(list(Header(list(Strikeout(Str("Hello R!"))))))
    expect_that(x, equals("<h1><del>Hello R!</del></h1>"))
    x <- pandocfilters:::test(list(Header(Strikeout(Str("Hello R!")))))
    expect_that(x, equals("<h1><del>Hello R!</del></h1>"))   

    ## Test Strikeout with Plain
    x <- pandocfilters:::test(list(Plain(list(Strikeout(list(Str("Hello R!")))))))
    expect_that(x, equals("<del>Hello R!</del>"))
    x <- pandocfilters:::test(list(Plain(list(Strikeout(Str("Hello R!"))))))
    expect_that(x, equals("<del>Hello R!</del>"))
    x <- pandocfilters:::test(list(Plain(Strikeout(Str("Hello R!")))))
    expect_that(x, equals("<del>Hello R!</del>"))
    
} )