context("Note")

test_that("Note", {

    block <- Plain(list(Str("x")))
    inline <- Note(block)
    y <- c("<h1><a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a></h1>",
           "<div class=\"footnotes\">", "<hr />", "<ol>",
           "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>", "</ol>", "</div>")

    ## Test Str with Header
    x <- pandocfilters:::test(list(Header(Note(list(block)))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Header(Note(block))))
    expect_that(x, equals(y))

    y <- c("<a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a>", 
           "<div class=\"footnotes\">", "<hr />", "<ol>", 
           "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>", "</ol>", "</div>")
    ## Test Str with Plain
    x <- pandocfilters:::test(list(Plain(Note(list(block)))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Plain(Note(block))))
    expect_that(x, equals(y))

} )

