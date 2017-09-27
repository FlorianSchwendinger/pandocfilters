context("SmallCaps")

test_that("SmallCaps", {

    ## Test SmallCaps with Header
    x <- pandocfilters:::test(list(Header(list(SmallCaps(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"))
    x <- pandocfilters:::test(list(Header(list(SmallCaps(Str("Hello R!"))))))
    expect_that(x, equals("<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"))
    x <- pandocfilters:::test(list(Header(SmallCaps(Str("Hello R!")))))
    expect_that(x, equals("<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"))   

    ## Test SmallCaps with Plain
    x <- pandocfilters:::test(list(Plain(list(SmallCaps(list(Str("Hello R!")))))))
    expect_that(x, equals("<span style=\"font-variant: small-caps;\">Hello R!</span>"))
    x <- pandocfilters:::test(list(Plain(list(SmallCaps(Str("Hello R!"))))))
    expect_that(x, equals("<span style=\"font-variant: small-caps;\">Hello R!</span>"))
    x <- pandocfilters:::test(list(Plain(SmallCaps(Str("Hello R!")))))
    expect_that(x, equals("<span style=\"font-variant: small-caps;\">Hello R!</span>"))
    
} )