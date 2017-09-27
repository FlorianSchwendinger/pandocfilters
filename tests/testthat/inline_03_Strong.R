context("Strong")

test_that("Strong", {

    ## Test Strong with Header
    x <- pandocfilters:::test(list(Header(list(Strong(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><strong>Hello R!</strong></h1>"))
    x <- pandocfilters:::test(list(Header(list(Strong(Str("Hello R!"))))))
    expect_that(x, equals("<h1><strong>Hello R!</strong></h1>"))
    x <- pandocfilters:::test(list(Header(Strong(Str("Hello R!")))))
    expect_that(x, equals("<h1><strong>Hello R!</strong></h1>"))   

    ## Test Strong with Plain
    x <- pandocfilters:::test(list(Plain(list(Strong(list(Str("Hello R!")))))))
    expect_that(x, equals("<strong>Hello R!</strong>"))
    x <- pandocfilters:::test(list(Plain(list(Strong(Str("Hello R!"))))))
    expect_that(x, equals("<strong>Hello R!</strong>"))
    x <- pandocfilters:::test(list(Plain(Strong(Str("Hello R!")))))
    expect_that(x, equals("<strong>Hello R!</strong>"))
    
} )

