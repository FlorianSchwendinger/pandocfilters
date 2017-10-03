if(interactive()) library(testthat)
context("block")

context(" - Plain")
test_that("Plain", {
  
  ## Test Str with Plain
  x <- pandocfilters:::test(list(Plain(list(Str("Hello R!")))))
  expect_equal(x, "Hello R!")
  x <- pandocfilters:::test(list(Plain(Str("Hello R!"))))
  expect_equal(x, "Hello R!")
  
} )


context(" - Para")
test_that("Para", {
  
  ## Test Str with Para
  x <- pandocfilters:::test(list(Para(list(Str("Hello R!")))))
  expect_equal(x, "<p>Hello R!</p>")
  x <- pandocfilters:::test(list(Para(Str("Hello R!"))))
  expect_equal(x, "<p>Hello R!</p>")
  
} )

context(" - CodeBlock")
test_that("CodeBlock", {
  
  attr <- Attr("id", c("Programming Language"), list(c("key", "value")))
  code <- "x <- 3\nprint('Hello R!')"
  block <- CodeBlock(attr, code)
  
  y <- paste(
    "<pre id=\"id\" class=\"Programming Language\" key=\"value\"><code>x &lt;- 3",
    "print(&#39;Hello R!&#39;)</code></pre>",
    collapse = "\n"
  )
  
  ## Test Str with CodeBlock
  x <- pandocfilters:::test(list(block))
  # expect_equal(x, y)
  expect_match(x, "\\<code\\>")
  expect_match(x, "\\<\\code\\>")
  expect_match(x, "x &lt;- 3")
  expect_match(x, "Hello R!")
  
} )


context(" - BlockQuote")
test_that("BlockQuote", {
  
  block <- BlockQuote(list(Plain(list(Str("Hello R!")))))
  
  y <- collapse_newline("<blockquote>", "Hello R!", "</blockquote>")
  
  ## Test Str with BlockQuote
  x <- pandocfilters:::test(list(block))
  expect_equal(x, y)
  
} )


context(" - OrderedList")
test_that("OrderedList", {
  
  y <- collapse_newline("<ol>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ol>")
  
  ordered_1 <- Plain("A")
  ordered_2 <- list(Plain(Str("B")))
  ordered_3 <- list(Plain(list(Str("C"))))
  block <- OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))
  x <- pandocfilters:::test(list(block))
  expect_equal(x, y)
  
} )



context(" - BulletList")
test_that("BulletList", {
  
  y <- collapse_newline("<ul>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ul>")
  
  bullet_1 <- list(Plain(list(Str("A"))))
  bullet_2 <- list(Plain(list(Str("B"))))
  bullet_3 <- list(Plain(list(Str("C"))))
  block <- BulletList(list(bullet_1, bullet_2, bullet_3))
  
  x <- pandocfilters:::test(list(block))
  expect_equal(x, y)
  
} )


context(" - DefinitionList")
test_that("DefinitionList", {
  
  y <- collapse_newline("<dl>", "<dt>key</dt>", "<dd>value", "</dd>", "<dt>key</dt>",  
         "<dd>value", "</dd>", "</dl>")
  
  key <- list(Str("key"))
  value <- list(list(Plain(list(Str("value")))))
  block <- DefinitionList(list(list(key, value), list(key, value)))
  
  x <- pandocfilters:::test(list(block))
  expect_equal(x, y)
  
} )


context(" - Header")
test_that("Header", {
  
  ## Test Str with Header
  x <- pandocfilters:::test(list(Header(list(Str("Hello R!")))))
  expect_equal(x, "<h1>Hello R!</h1>")
  x <- pandocfilters:::test(list(Header(Str("Hello R!"))))
  expect_equal(x, "<h1>Hello R!</h1>")
  
} )


context(" - HorizontalRule")
test_that("HorizontalRule", {
  
  block <- HorizontalRule()
  
  ## Test Str with Plain
  x <- pandocfilters:::test(list(block))
  expect_equal(x, "<hr />")
  
  
} )


context(" - Table")
test_that("Table", {
  
  M <- matrix(1:4, 2)
  T <- Table(M, col_names=c("A", "B"))
  
  x <- pandocfilters:::test(list(T), "markdown")
  y <- c("  A   B", "  --- ---", "  1   3", "  2   4")
  expect_equal(x, y)
  
} )


context(" - Div")
test_that("Div", {
  
  blocks <- list(Plain(list(Str("Hello R!"))))
  block <- Div(blocks)
  
  ## Test Div
  x <- pandocfilters:::test(list(block))
  y <- collapse_newline("<div>", "Hello R!", "</div>")
  expect_equal(x, y)
  
} )


context(" - Null")
test_that("Null", {
  
  block <- Null()
  y <- ""
  ## Test Null
  x <- pandocfilters:::test(list(block))
  expect_equal(x, y)
  
} )


context(" - Block Combine")
test_that("Block Combine", {
  
  ## Test Str with Plain
  x <- Header("Hello")
  y <- Plain("R")
  z <- Plain("!")
  expect_equal(
    pandocfilters:::test(c(x, y, z)), 
    collapse_newline("<h1>Hello</h1>", "R", "!")
  )
  
  
} )
