if(interactive()) library(testthat)
context("inline")


context(" - Str")
test_that("Str", {
  
  ## Test Str with Header
  x <- pandocfilters:::test(list(Header(list(Str("Hello R!")))))
  expect_equal(x, "<h1>Hello R!</h1>")
  x <- pandocfilters:::test(list(Header(Str("Hello R!"))))
  expect_equal(x, "<h1>Hello R!</h1>")
  
  ## Test Str with Plain
  x <- pandocfilters:::test(list(Plain(list(Str("Hello R!")))))
  expect_equal(x, "Hello R!")
  x <- pandocfilters:::test(list(Plain(Str("Hello R!"))))
  expect_equal(x, "Hello R!")
  
} )


context(" - Emph")
test_that("Emph", {
  
  ## Test Emph with Header
  x <- pandocfilters:::test(list(Header(list(Emph(list(Str("Hello R!")))))))
  expect_equal(x, "<h1><em>Hello R!</em></h1>")
  x <- pandocfilters:::test(list(Header(list(Emph(Str("Hello R!"))))))
  expect_equal(x, "<h1><em>Hello R!</em></h1>")
  x <- pandocfilters:::test(list(Header(Emph(Str("Hello R!")))))
  expect_equal(x, "<h1><em>Hello R!</em></h1>")   
  
  ## Test Emph with Plain
  x <- pandocfilters:::test(list(Plain(list(Emph(list(Str("Hello R!")))))))
  expect_equal(x, "<em>Hello R!</em>")
  x <- pandocfilters:::test(list(Plain(list(Emph(Str("Hello R!"))))))
  expect_equal(x, "<em>Hello R!</em>")
  x <- pandocfilters:::test(list(Plain(Emph(Str("Hello R!")))))
  expect_equal(x, "<em>Hello R!</em>")
  
} )


context(" - Strong")
test_that("Strong", {
  
  ## Test Strong with Header
  x <- pandocfilters:::test(list(Header(list(Strong(list(Str("Hello R!")))))))
  expect_equal(x, "<h1><strong>Hello R!</strong></h1>")
  x <- pandocfilters:::test(list(Header(list(Strong(Str("Hello R!"))))))
  expect_equal(x, "<h1><strong>Hello R!</strong></h1>")
  x <- pandocfilters:::test(list(Header(Strong(Str("Hello R!")))))
  expect_equal(x, "<h1><strong>Hello R!</strong></h1>")   
  
  ## Test Strong with Plain
  x <- pandocfilters:::test(list(Plain(list(Strong(list(Str("Hello R!")))))))
  expect_equal(x, "<strong>Hello R!</strong>")
  x <- pandocfilters:::test(list(Plain(list(Strong(Str("Hello R!"))))))
  expect_equal(x, "<strong>Hello R!</strong>")
  x <- pandocfilters:::test(list(Plain(Strong(Str("Hello R!")))))
  expect_equal(x, "<strong>Hello R!</strong>")
  
} )



context(" - Strikeout")
test_that("Strikeout", {
  
  ## Test Strikeout with Header
  x <- pandocfilters:::test(list(Header(list(Strikeout(list(Str("Hello R!")))))))
  expect_equal(x, "<h1><del>Hello R!</del></h1>")
  x <- pandocfilters:::test(list(Header(list(Strikeout(Str("Hello R!"))))))
  expect_equal(x, "<h1><del>Hello R!</del></h1>")
  x <- pandocfilters:::test(list(Header(Strikeout(Str("Hello R!")))))
  expect_equal(x, "<h1><del>Hello R!</del></h1>")   
  
  ## Test Strikeout with Plain
  x <- pandocfilters:::test(list(Plain(list(Strikeout(list(Str("Hello R!")))))))
  expect_equal(x, "<del>Hello R!</del>")
  x <- pandocfilters:::test(list(Plain(list(Strikeout(Str("Hello R!"))))))
  expect_equal(x, "<del>Hello R!</del>")
  x <- pandocfilters:::test(list(Plain(Strikeout(Str("Hello R!")))))
  expect_equal(x, "<del>Hello R!</del>")
  
} )


context(" - Superscript")
test_that("Superscript", {
  
  ## Test Superscript with Header
  x <- pandocfilters:::test(list(Header(list(Superscript(list(Str("Hello R!")))))))
  expect_equal(x, "<h1><sup>Hello R!</sup></h1>")
  x <- pandocfilters:::test(list(Header(list(Superscript(Str("Hello R!"))))))
  expect_equal(x, "<h1><sup>Hello R!</sup></h1>")
  x <- pandocfilters:::test(list(Header(Superscript(Str("Hello R!")))))
  expect_equal(x, "<h1><sup>Hello R!</sup></h1>")   
  
  ## Test Superscript with Plain
  x <- pandocfilters:::test(list(Plain(list(Superscript(list(Str("Hello R!")))))))
  expect_equal(x, "<sup>Hello R!</sup>")
  x <- pandocfilters:::test(list(Plain(list(Superscript(Str("Hello R!"))))))
  expect_equal(x, "<sup>Hello R!</sup>")
  x <- pandocfilters:::test(list(Plain(Superscript(Str("Hello R!")))))
  expect_equal(x, "<sup>Hello R!</sup>")
  
} )


context(" - Subscript")
test_that("Subscript", {
  
  ## Test Subscript with Header
  x <- pandocfilters:::test(list(Header(list(Subscript(list(Str("Hello R!")))))))
  expect_equal(x, "<h1><sub>Hello R!</sub></h1>")
  x <- pandocfilters:::test(list(Header(list(Subscript(Str("Hello R!"))))))
  expect_equal(x, "<h1><sub>Hello R!</sub></h1>")
  x <- pandocfilters:::test(list(Header(Subscript(Str("Hello R!")))))
  expect_equal(x, "<h1><sub>Hello R!</sub></h1>")   
  
  ## Test Subscript with Plain
  x <- pandocfilters:::test(list(Plain(list(Subscript(list(Str("Hello R!")))))))
  expect_equal(x, "<sub>Hello R!</sub>")
  x <- pandocfilters:::test(list(Plain(list(Subscript(Str("Hello R!"))))))
  expect_equal(x, "<sub>Hello R!</sub>")
  x <- pandocfilters:::test(list(Plain(Subscript(Str("Hello R!")))))
  expect_equal(x, "<sub>Hello R!</sub>")
  
} )


context(" - SmallCaps")
test_that("SmallCaps", {
  
  ## Test SmallCaps with Header
  x <- pandocfilters:::test(list(Header(list(SmallCaps(list(Str("Hello R!")))))))
  y <- if(get_pandoc_version() < "2.0"){
    "<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"
  } else {
    "<h1><span class=\"smallcaps\">Hello R!</span></h1>"
  }
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(list(SmallCaps(Str("Hello R!"))))))
  y <- if(get_pandoc_version() < "2.0"){
    "<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"
  } else {
    "<h1><span class=\"smallcaps\">Hello R!</span></h1>"
  }
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(SmallCaps(Str("Hello R!")))))
  y <- if(get_pandoc_version() < "2.0"){
    "<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"
  } else {
    "<h1><span class=\"smallcaps\">Hello R!</span></h1>"
  }
  expect_equal(x, y)
  
  ## Test SmallCaps with Plain
  x <- pandocfilters:::test(list(Plain(list(SmallCaps(list(Str("Hello R!")))))))
  y <- if(get_pandoc_version() < "2.0"){
    "<span style=\"font-variant: small-caps;\">Hello R!</span>"
  } else {
    "<span class=\"smallcaps\">Hello R!</span>"
  }
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(list(SmallCaps(Str("Hello R!"))))))
  y <- if(get_pandoc_version() < "2.0"){
    "<span style=\"font-variant: small-caps;\">Hello R!</span>"
  } else {
    "<span class=\"smallcaps\">Hello R!</span>"
  }
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(SmallCaps(Str("Hello R!")))))
  y <- if(get_pandoc_version() < "2.0"){
    "<span style=\"font-variant: small-caps;\">Hello R!</span>"
  } else {
    "<span class=\"smallcaps\">Hello R!</span>"
  }
  expect_equal(x, y)
  
} )


context(" - Quoted")
test_that("Quoted", {
  
  ## Test Quoted with Header
  x <- pandocfilters:::test(list(Header(list(Quoted(list(Str("Hello R!")))))))
  expect_equal(x, "<h1>“Hello R!”</h1>")
  x <- pandocfilters:::test(list(Header(list(Quoted(Str("Hello R!"))))))
  expect_equal(x, "<h1>“Hello R!”</h1>")
  x <- pandocfilters:::test(list(Header(Quoted(Str("Hello R!")))))
  expect_equal(x, "<h1>“Hello R!”</h1>")   
  
  ## Test Quoted with Plain
  x <- pandocfilters:::test(list(Plain(list(Quoted(list(Str("Hello R!")))))))
  expect_equal(x, "“Hello R!”")
  x <- pandocfilters:::test(list(Plain(list(Quoted(Str("Hello R!"))))))
  expect_equal(x, "“Hello R!”")
  x <- pandocfilters:::test(list(Plain(Quoted(Str("Hello R!")))))
  expect_equal(x, "“Hello R!”")
  
} )


context(" - Cite")
test_that("Cite", {
  
  ci <- Citation(suffix=list(Str("Suffix_1")),
                 id="Citation_ID_1", prefix=list(Str("Prefix_1")))
  
  ## Test Cite with Header
  cite <- Cite(list(ci), list(Str("some text")))
  x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
  expect_equal(
    x,
    collapse_newline("@Citation_ID_1 [Suffix\\_1]", "==========================")
  )
  
  cite <- Cite(list(ci), Str("some text"))
  x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
  expect_equal(
    x, 
    collapse_newline(
      "@Citation_ID_1 [Suffix\\_1]", 
      "==========================")
  )
  
  cite <- Cite(ci, Str("some text"))
  x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
  expect_equal(
    x, 
    collapse_newline(
      "@Citation_ID_1 [Suffix\\_1]", 
      "==========================")
  )
  
  x <- pandocfilters:::test(list(Header(cite)), to="markdown")
  expect_equal(
    x, 
    collapse_newline(
      "@Citation_ID_1 [Suffix\\_1]",
      "==========================")
  )
  
  ## Test Cite with Plain
  cite <- Cite(list(ci), list(Str("some text")))
  x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
  expect_equal(x, "@Citation_ID_1 [Suffix\\_1]")
  
  cite <- Cite(list(ci), Str("some text"))
  x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
  expect_equal(x, "@Citation_ID_1 [Suffix\\_1]")
  
  cite <- Cite(ci, Str("some text"))
  x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
  expect_equal(x, "@Citation_ID_1 [Suffix\\_1]")
  
  x <- pandocfilters:::test(list(Plain(cite)), to="markdown")
  expect_equal(x, "@Citation_ID_1 [Suffix\\_1]")
  
} )


context(" - Str")
test_that("Str", {
  
  y <- collapse_newline(
    "`lm(hello ~ world)`{#my_r_inline_code .R startFrom=\"0\"}",  
    "======================================================="
  )
  
  inline <- Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0)
  
  ## Test Str with Header
  x <- pandocfilters:::test(list(Header(list(inline))), "markdown")
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)), "markdown")
  expect_equal(x, y)
  
  ## Test Str with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))), "markdown")
  expect_equal(
    x,
    "`lm(hello ~ world)`{#my_r_inline_code .R startFrom=\"0\"}"
  )
  
  x <- pandocfilters:::test(list(Plain(inline)), "markdown")
  expect_equal(
    x, 
    "`lm(hello ~ world)`{#my_r_inline_code .R startFrom=\"0\"}"
  )
  
} )


context(" - Space")
test_that("Space", {
  
  y <- "<h1> </h1>"
  inline <- Space()
  
  ## Test Space with Header
  x <- pandocfilters:::test(list(Header(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)))
  expect_equal(x, y)
  
  ## Test Space with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))))
  expect_equal(x, " ")
  
  x <- pandocfilters:::test(list(Plain(inline)))
  expect_equal(x, " ")
  
} )


context(" - SoftBreak")
test_that("SoftBreak", {
  
  if ( pandocfilters:::get_pandoc_version() > 1.15 ) {
    
    y <- "<h1> </h1>"
    inline <- SoftBreak()
    
    ## Test SoftBreak with Header
    x <- pandocfilters:::test(list(Header(list(inline))))
    expect_equal(x, y)
    
    x <- pandocfilters:::test(list(Header(inline)))
    expect_equal(x, y)
    
    ## Test SoftBreak with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_equal(x, " ")
    
    x <- pandocfilters:::test(list(Plain(inline)))
    expect_equal(x, " ")
    
  }
  
} )


context(" - LineBreak")
test_that("LineBreak", {
  
  y <- collapse_newline("<h1><br />", "</h1>")
  inline <- LineBreak()
  
  ## Test LineBreak with Header
  x <- pandocfilters:::test(list(Header(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)))
  expect_equal(x, y)
  
  y <- collapse_newline("<br />", "")
  ## Test LineBreak with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(inline)))
  expect_equal(x, y)
  
} )


context(" - Math")
test_that("Math", {
  
  inline <- Math("e^x")
  y <- if ( get_pandoc_version() < "1.16" ) "$e^x$" else "\\(e^x\\)"
  
  ## Test Math with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))), "latex")
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(inline)), "latex")
  expect_equal(x, y)
  
} )


context(" - RawInline")
test_that("RawInline", {
  
  y <- collapse_newline("some RawInline", "==============")
  inline <- RawInline("latex", "some RawInline")
  
  ## Test Space with Header
  x <- pandocfilters:::test(list(Header(list(inline))), "markdown")
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)), "markdown")
  expect_equal(x, y)
  
  ## Test Space with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))), "markdown")
  expect_equal(x, "some RawInline")
  
  x <- pandocfilters:::test(list(Plain(inline)), "markdown")
  expect_equal(x, "some RawInline")
  
} )


context(" - Link")
test_that("Link", {
  
  y <- "<h1><a href=\"https://cran.r-project.org/\" title=\"some title\">Text_Shown</a></h1>"
  inline <- Link("https://cran.r-project.org/", list(Str("Text_Shown")), "some title")
  
  ## Test Link with Header
  x <- pandocfilters:::test(list(Header(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)))
  expect_equal(x, y)
  
  y <- "<a href=\"https://cran.r-project.org/\" title=\"some title\">Text_Shown</a>"
  ## Test Link with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(inline)))
  expect_equal(x, y)
  
} )


context(" - Image")
test_that("Image", {
  
  y <- "<h1><img src=\"https:://Rlogo.jpg\" title=\"fig:some_caption\" alt=\"some_text\" /></h1>"
  inline <- Image("https:://Rlogo.jpg", list(Str("some_text")), "fig:some_caption")
  
  ## Test Image with Header
  x <- pandocfilters:::test(list(Header(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Header(inline)))
  expect_equal(x, y)
  
  y <- "<img src=\"https:://Rlogo.jpg\" title=\"fig:some_caption\" alt=\"some_text\" />"
  ## Test Image with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))))
  expect_equal(x, y)
  
  x <- pandocfilters:::test(list(Plain(inline)))
  expect_equal(x, y)
  
} )


context(" - Note")
test_that("Note", {
  
  block <- Plain(list(Str("x")))
  inline <- Note(block)
  y <- if(get_pandoc_version() < "2.0"){
    collapse_newline(
      "<h1><a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a></h1>",
      "<div class=\"footnotes\">",
      "<hr />",
      "<ol>",
      "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>",
      "</ol>",
      "</div>"
    )
  } else {
    collapse_newline(
      "<h1><a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a></h1>",
      "<section class=\"footnotes\">",
      "<hr />",                        
      "<ol>",
      "<li id=\"fn1\">x<a href=\"#fnref1\" class=\"footnoteBack\">â†©</a></li>",
      "</ol>",
      "</section>"
      )
  }
  
  ## Test Str with Header
  x <- pandocfilters:::test(list(Header(Note(list(block)))))
  expect_equal(x, y)
  x <- pandocfilters:::test(list(Header(Note(block))))
  expect_equal(x, y)
  
  y <- collapse_newline(
    "<a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a>", 
    "<div class=\"footnotes\">", "<hr />", "<ol>", 
    "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>", "</ol>", "</div>"
  )
  ## Test Str with Plain
  x <- pandocfilters:::test(list(Plain(Note(list(block)))))
  expect_equal(x, y)
  x <- pandocfilters:::test(list(Plain(Note(block))))
  expect_equal(x, y)
  
} )



context(" - Span")
test_that("Span", {
  
  attr <- Attr("A", c("B"), list(c("C", "D")))
  inline <- Span(attr, list(Str("some inline string")))
  
  y <- if(get_pandoc_version() < "2.0"){
    "<h1><span id=\"A\" class=\"B\" C=\"D\">some inline string</span></h1>"
  } else {
    "<h1><span id=\"A\" class=\"B\" data-C=\"D\">some inline string</span></h1>"
  }
  ## Test Image with Header
  x <- pandocfilters:::test(list(Header(inline)))
  expect_equal(x, y)
  
  y <- if(get_pandoc_version() < "2.0"){
    "<span id=\"A\" class=\"B\" C=\"D\">some inline string</span>"
  } else {
    "<span id=\"A\" class=\"B\" data-C=\"D\">some inline string</span>"
  }
  ## Test Image with Plain
  x <- pandocfilters:::test(list(Plain(list(inline))))
  expect_equal(x, y)
  
} )
