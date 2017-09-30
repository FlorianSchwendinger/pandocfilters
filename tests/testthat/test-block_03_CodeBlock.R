context("CodeBlock")

test_that("CodeBlock", {

	attr <- Attr("id", c("Programming Language"), list(c("key", "value")))
	code <- "x <- 3\nprint('Hello R!')"
	block <- CodeBlock(attr, code)

	y <- c("<pre id=\"id\" class=\"Programming Language\" key=\"value\"><code>x &lt;- 3",  
		   "print(&#39;Hello R!&#39;)</code></pre>")

    ## Test Str with CodeBlock
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
