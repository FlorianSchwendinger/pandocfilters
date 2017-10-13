## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize----------------------------------------------------------
require("pandocfilters", quietly = TRUE, warn.conflicts = FALSE)

## ------------------------------------------------------------------------
# Create a new document.
doc <- document()

# Create a non-standard writer function so we can look at the document while writing it.
cat_writer <- function(x, con = stdout(), format = "json") {
  x <- pandoc_from_json(x, to = format)
  cat(paste(x, collapse = "\n"))
}

# Append a Header and look at the document
args(doc$append_header)

## ------------------------------------------------------------------------
doc$append_header( "R Basics" )
doc$write(format = "html", writer = cat_writer)

## ---- results='hide'-----------------------------------------------------
# Append a level 2 Header
doc$append_header( "What is R?", level = 2)

# Append Plain text with inline formating
x <- c(
  Emph("R"), Space(), "is a system for ", 
  Strong("statistical computation"), Space(), Strikeout("and"), 
  Space(), Superscript("graphics"), ". ", LineBreak(), 
  Subscript("It"), Space(), SmallCaps("consists"), Space(), 
  Quoted("of", quote_type = "SingleQuote"), Space(), 
  Quoted("a", quote_type = "DoubleQuote"), Space(), 
  RawInline("html", "<i>language</i>"), 
  " plus a run-time environment with", " graphics, a debugger, access to ",
  "certain system functions,", 
  " and the ability to run programs stored in script files."
)

doc$append_plain( x )
doc$write(format = "html", writer = cat_writer)

## ---- results='asis', echo = FALSE---------------------------------------
doc$write(format = "html", writer = cat_writer)

## ---- results='hide'-----------------------------------------------------
fix_quotes_fun <- function(x) {
  RawInline("html", sprintf("<q>%s</q>", x$c))
}

fix_quotes <- function(type, content, ...) {
  if (type == "Quoted") {       
    lapply(content[[-1]], fix_quotes_fun)
  }
}

## ---- results='hide'-----------------------------------------------------
doc$doc <- astrapply(doc$doc, FUN = fix_quotes)
doc$write(format = "html", writer = cat_writer)

## ---- results='asis', echo=FALSE-----------------------------------------
doc$write(format = "html", writer = cat_writer)

## ------------------------------------------------------------------------
table <- document()
table$append_table(cars[1:3, ])

## ------------------------------------------------------------------------
table$write(format = "markdown", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "html", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "html5", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "org", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "latex", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "rst", writer = cat_writer)

## ------------------------------------------------------------------------
table$write(format = "asciidoc", writer = cat_writer)

