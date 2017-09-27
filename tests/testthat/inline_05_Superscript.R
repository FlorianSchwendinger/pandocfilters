context("Superscript")

test_that("Superscript", {

    ## Test Superscript with Header
    x <- pandocfilters:::test(list(Header(list(Superscript(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><sup>Hello R!</sup></h1>"))
    x <- pandocfilters:::test(list(Header(list(Superscript(Str("Hello R!"))))))
    expect_that(x, equals("<h1><sup>Hello R!</sup></h1>"))
    x <- pandocfilters:::test(list(Header(Superscript(Str("Hello R!")))))
    expect_that(x, equals("<h1><sup>Hello R!</sup></h1>"))   

    ## Test Superscript with Plain
    x <- pandocfilters:::test(list(Plain(list(Superscript(list(Str("Hello R!")))))))
    expect_that(x, equals("<sup>Hello R!</sup>"))
    x <- pandocfilters:::test(list(Plain(list(Superscript(Str("Hello R!"))))))
    expect_that(x, equals("<sup>Hello R!</sup>"))
    x <- pandocfilters:::test(list(Plain(Superscript(Str("Hello R!")))))
    expect_that(x, equals("<sup>Hello R!</sup>"))
    
} )