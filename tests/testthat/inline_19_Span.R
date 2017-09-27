context("Span")

test_that("Span", {

    attr <- Attr("A", c("B"), list(c("C", "D")))
    inline <- Span(attr, list(Str("some inline string")))

    y <- "<h1><span id=\"A\" class=\"B\" C=\"D\">some inline string</span></h1>"
    ## Test Image with Header
    x <- pandocfilters:::test(list(Header(inline)))
    expect_that(x, equals(y))

    y <- "<span id=\"A\" class=\"B\" C=\"D\">some inline string</span>"
    ## Test Image with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_that(x, equals(y))

} )