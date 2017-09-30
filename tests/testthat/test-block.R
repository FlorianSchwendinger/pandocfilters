context("block")

test_that("Plain", {

    ## Test Str with Plain
    x <- pdf_test(list(Plain(list(Str("Hello R!")))))
    expect_equal(x, "Hello R!")
    x <- pandocfilters:::test(list(Plain(Str("Hello R!"))))
    expect_equal(x, "Hello R!")

} )


test_that("Para", {

    ## Test Str with Para
    x <- pandocfilters:::test(list(Para(list(Str("Hello R!")))))
    expect_equal(x, "<p>Hello R!</p>")
    x <- pandocfilters:::test(list(Para(Str("Hello R!"))))
    expect_equal(x, "<p>Hello R!</p>")

} )


test_that("CodeBlock", {

	attr <- Attr("id", c("Programming Language"), list(c("key", "value")))
	code <- "x <- 3\nprint('Hello R!')"
	block <- CodeBlock(attr, code)

	y <- c("<pre id=\"id\" class=\"Programming Language\" key=\"value\"><code>x &lt;- 3",  
		   "print(&#39;Hello R!&#39;)</code></pre>")

    ## Test Str with CodeBlock
    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("BlockQuote", {

	block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

	y <- c("<blockquote>", "Hello R!", "</blockquote>")

    ## Test Str with BlockQuote
    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("BlockQuote", {

    y <- c("<blockquote>", "Hello R!", "</blockquote>")

    block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("OrderedList", {

	y <- c("<ol>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ol>")

    ordered_1 <- Plain("A")
	ordered_2 <- list(Plain(Str("B")))
	ordered_3 <- list(Plain(list(Str("C"))))
	block <- OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))
    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )



test_that("BulletList", {

	y <- c("<ul>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ul>")

	bullet_1 <- list(Plain(list(Str("A"))))
	bullet_2 <- list(Plain(list(Str("B"))))
	bullet_3 <- list(Plain(list(Str("C"))))
	block <- BulletList(list(bullet_1, bullet_2, bullet_3))

    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("DefinitionList", {

    y <- c("<dl>", "<dt>key</dt>", "<dd>value", "</dd>", "<dt>key</dt>",  
           "<dd>value", "</dd>", "</dl>")

    key <- list(Str("key"))
    value <- list(list(Plain(list(Str("value")))))
    block <- DefinitionList(list(list(key, value), list(key, value)))

    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("Header", {

    ## Test Str with Header
    x <- pandocfilters:::test(list(Header(list(Str("Hello R!")))))
    expect_equal(x, "<h1>Hello R!</h1>")
    x <- pandocfilters:::test(list(Header(Str("Hello R!"))))
    expect_equal(x, "<h1>Hello R!</h1>")

} )


test_that("HorizontalRule", {

	block <- HorizontalRule()

    ## Test Str with Plain
    x <- pandocfilters:::test(list(block))
    expect_equal(x, "<hr />")
    

} )


test_that("Table", {

    M <- matrix(1:4, 2)
    T <- Table(M, col_names=c("A", "B"))
     
    x <- pandocfilters:::test(list(T), "markdown")
    y <- c("  A   B", "  --- ---", "  1   3", "  2   4", "", "")
    expect_equal(x, y)

} )


test_that("Div", {

	blocks <- list(Plain(list(Str("Hello R!"))))
	block <- Div(blocks)

    ## Test Div
    x <- pandocfilters:::test(list(block))
    y <- c("<div>", "Hello R!", "</div>")
    expect_equal(x, y)

} )


test_that("Null", {

	block <- Null()
	y <- ""
    ## Test Null
    x <- pandocfilters:::test(list(block))
    expect_equal(x, y)

} )


test_that("Block Combine", {

    ## Test Str with Plain
    x <- Header("Hello")
    y <- Plain("R")
    z <- Plain("!")
    expect_that(pandocfilters:::test(c(x, y, z)), equals(c("<h1>Hello</h1>", "R", "!")))

} )
