context("Quoted")

test_that("Quoted", {

    ## Test Quoted with Header
    x <- pandocfilters:::test(list(Header(list(Quoted(list(Str("Hello R!")))))))
    expect_that(x, equals("<h1>“Hello R!”</h1>"))
    x <- pandocfilters:::test(list(Header(list(Quoted(Str("Hello R!"))))))
    expect_that(x, equals("<h1>“Hello R!”</h1>"))
    x <- pandocfilters:::test(list(Header(Quoted(Str("Hello R!")))))
    expect_that(x, equals("<h1>“Hello R!”</h1>"))   

    ## Test Quoted with Plain
    x <- pandocfilters:::test(list(Plain(list(Quoted(list(Str("Hello R!")))))))
    expect_that(x, equals("“Hello R!”"))
    x <- pandocfilters:::test(list(Plain(list(Quoted(Str("Hello R!"))))))
    expect_that(x, equals("“Hello R!”"))
    x <- pandocfilters:::test(list(Plain(Quoted(Str("Hello R!")))))
    expect_that(x, equals("“Hello R!”"))
    
} )