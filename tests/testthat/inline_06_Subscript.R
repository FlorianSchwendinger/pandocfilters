context("Subscript")

test_that("Subscript", {

    ## Test Subscript with Header
    x <- pandocfilters:::test(list(Header(list(Subscript(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><sub>Hello R!</sub></h1>"))
    x <- pandocfilters:::test(list(Header(list(Subscript(Str("Hello R!"))))))
    expect_that(x, equals("<h1><sub>Hello R!</sub></h1>"))
    x <- pandocfilters:::test(list(Header(Subscript(Str("Hello R!")))))
    expect_that(x, equals("<h1><sub>Hello R!</sub></h1>"))   

    ## Test Subscript with Plain
    x <- pandocfilters:::test(list(Plain(list(Subscript(list(Str("Hello R!")))))))
    expect_that(x, equals("<sub>Hello R!</sub>"))
    x <- pandocfilters:::test(list(Plain(list(Subscript(Str("Hello R!"))))))
    expect_that(x, equals("<sub>Hello R!</sub>"))
    x <- pandocfilters:::test(list(Plain(Subscript(Str("Hello R!")))))
    expect_that(x, equals("<sub>Hello R!</sub>"))
    
} )