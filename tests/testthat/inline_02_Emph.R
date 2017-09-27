context("Emph")

test_that("Emph", {

    ## Test Emph with Header
    x <- pandocfilters:::test(list(Header(list(Emph(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><em>Hello R!</em></h1>"))
    x <- pandocfilters:::test(list(Header(list(Emph(Str("Hello R!"))))))
    expect_that(x, equals("<h1><em>Hello R!</em></h1>"))
    x <- pandocfilters:::test(list(Header(Emph(Str("Hello R!")))))
    expect_that(x, equals("<h1><em>Hello R!</em></h1>"))   

    ## Test Emph with Plain
    x <- pandocfilters:::test(list(Plain(list(Emph(list(Str("Hello R!")))))))
    expect_that(x, equals("<em>Hello R!</em>"))
    x <- pandocfilters:::test(list(Plain(list(Emph(Str("Hello R!"))))))
    expect_that(x, equals("<em>Hello R!</em>"))
    x <- pandocfilters:::test(list(Plain(Emph(Str("Hello R!")))))
    expect_that(x, equals("<em>Hello R!</em>"))
    
} )
