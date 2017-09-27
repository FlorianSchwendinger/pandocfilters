library(jsonlite)
library(pandocfilters)

fjson <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE, flatten=TRUE)

pandoc_to_json <- function(file, from="markdown") {
    cmd <- sprintf("pandoc -f %s -t json %s", from, file)
    system(cmd, intern=TRUE)
}

pandoc_from_json <- function(json, to) {
    cmd <- sprintf("echo %s | pandoc -f json -t %s", shQuote(json), to)
    system(cmd, intern=TRUE)
}

##' new_doc creates a new document x has to be a list of pandoc objects
new_doc <- function(x) {
    list(list(unMeta=pandocfilters:::nlist()),
         list(list(t="Plain", c=x)))
}

##' test tests a constructor by putting it into a document and tranforming it!
test <- function(x, to="html") {
    pandoc_from_json(as.character( jsonlite::toJSON(new_doc(x), auto_unbox=TRUE) ), to=to)
}

test2 <- function(x, to="html") {
    d <- list(list(unMeta=pandocfilters:::nlist()), x)
    pandoc_from_json(as.character(jsonlite::toJSON(d, auto_unbox=TRUE)), to=to)
}

cat(dir("Block"), sep="', '")

c('BlockQuote.md', 'BulletList.md', 'CodeBlock.md', 'DefinitionList.md',
  'Div.md', 'Header.md', 'HorizontalRule.md', 'Null.md', 'OrderedList.md',
  'Para.md', 'Plain.md', 'RawBlock.md', 'Table.md')

str(new_doc(list(Str("text"))))
xx <- list( list(unMeta=pandocfilters:::nlist()),
           list(Plain(list(Str("text")))) )
str(xx)
pandoc_from_json(as.character(jsonlite::toJSON(xx, auto_unbox=TRUE)), to="html")

test2(list(Plain(list(Str("text")))))

##' -------------------------------------
##' Plain
##' -------------------------------------
Plain <- function(x) list(t="Plain", c=x)
test2(list(Plain(list(Str("text1"), Str("text2")))))

##' -------------------------------------
##' CodeBlock
##' -------------------------------------
j <- pandoc_to_json("Block/CodeBlock.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param attr an object of type Attr
#' @param code a character string containing the source code.
#' @examples
#' attr <- Attr("id", list("Programming Language"), list(c("key", "value")))
#' code <- "x <- 3\nprint('Hello R!')"
#' CodeBlock(attr, code)
## Attr String
CodeBlock <- function(attr, code) {
      list(t="CodeBlock", c=list(attr, code))
}

str(CodeBlock(Attr("id", list("Programming Language"), list(c("key", "value"))), "x <- 3\nprint('Hello R!')"))
test2(list(CodeBlock(Attr("id", list("Programming Language"), list(c("key", "value"))), "x <- 3\nprint('Hello R!')")), "org")

cb <- CodeBlock(Attr("id", list("Programming Language"), list(c("key", "value"))), "x <- 3\nprint('Hello R!')")
test2(list(cb), "org")
test2(list(Note(cb)))


##' -------------------------------------
##' BlockQuote
##' -------------------------------------
j <- pandoc_to_json("Block/BlockQuote", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param blocks a list of blocks
#' @examples
BlockQuote(list(Plain(list(Str("Hello R!")))))
## Attr String
BlockQuote <- function(blocks) {
      list(t="BlockQuote", c=blocks)
}

test2(list(BlockQuote(list(Plain(list(Str("Hello R!")))))), "org")
test2(list(BlockQuote(list(cb))), "org")


##' -------------------------------------
##' ListAttributes
##' -------------------------------------

#' @param first_number an integer giving the first number of the list
#' @param style a character string giving the style possible values are \dQuote{DefaultStyle}, 
#'              \dQuote{Example}, \dQuote{Decimal}, \dQuote{LowerRoman}, 
#'              \dQuote{UpperRoman}, \dQuote{LowerAlpha} and \dQuote{UpperAlpha}.
#' @param delim a character string giving the delimiter possible values are \dQuote{DefaultDelim},
#'              \dQuote{Period}, \dQuote{OneParen} and \dQuote{TwoParens}.
#'
#' 
## ListAttributes = (Int, ListNumberStyle, ListNumberDelim) 
ListAttributes <- function(first_number=1L, style="DefaultStyle", delim="DefaultDelim") {
    list(first_number, list(t=style, c=list()), list(t=delim, c=list()))
}
  
##' -------------------------------------
##' OrderedList
##' -------------------------------------
j <- pandoc_to_json("Block/OrderedList.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' @param lattr a list of attributes
#' @param lblocks a list of lists of blocks
#' @examples
## Attr String
OrderedList <- function(lattr, lblocks) {
      list(t="OrderedList", c=list(lattr, lblocks))
}

ordered_1 <- list(Plain(list(Str("A"))))
ordered_2 <- list(Plain(list(Str("B"))))
ordered_3 <- list(Plain(list(Str("C"))))

test2(list(OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))), "markdown")
ListAttributes()
test2(list(OrderedList(ListAttributes(), list(list(Plain(list(Str("Hello R!"))))))), "org")

##' -------------------------------------
##' BulletList
##' -------------------------------------
j <- pandoc_to_json("Block/BulletList.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' @param lblocks a list of lists of blocks
#' @examples
## Attr String
BulletList <- function(lblocks) list(t="BulletList", c=lblocks)
test2(list(BulletList(list(list(Plain(list(Str("Hello R!"))))))), "markdown")
test2(list(BulletList(list(list(Plain(list(Str("Hello R!"))))))), "markdown")

bullet_1 <- list(Plain(list(Str("A"))))
bullet_2 <- list(Plain(list(Str("B"))))
bullet_3 <- list(Plain(list(Str("C"))))

test2(list(BulletList(list(bullet_1, bullet_2, bullet_3))), "markdown")

##' -------------------------------------
##' Header
##' -------------------------------------
j <- pandoc_to_json("Block/Header.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' Header - level (integer) and text (inlines)
#' @param inlines a list of inline objects
#' @param level an integer 
#' @param attr an object of type Attr
#' @examples
#' Header(list(Str("My Header"))))
## Attr String
Header <- function(inlines, level=1L, attr=Attr()) {
    list(t="Header", c=list(level, attr, inlines))
}



test2(list(Header(list(Str("My Header")))), "markdown")

##' -------------------------------------
##' HorizontalRule
##' -------------------------------------
j <- pandoc_to_json("Block/HorizontalRule.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' Horizontal Rule
#' @examples
#' HorizontalRule()
## Attr String
HorizontalRule <- function() list(t="HorizontalRule", c=list())

test2(list(HorizontalRule()), "markdown")

##' -------------------------------------
##' Div
##' -------------------------------------
j <- pandoc_to_json("Block/Div.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' Div
#' @param blocks a list of blocks
#' @param attr an object of type Attr
#' @examples
#' blocks <- list(Plain(list(Str("Hello R!"))))
#' Div(blocks)
## Attr String
Div <- function(blocks, attr=Attr()) {
    list(t="Div", c=list(attr, inlines))
}

blocks <- list(Plain(list(Str("Hello R!"))))
Div(blocks)
test2(list(Div(blocks)), "markdown")

##' -------------------------------------
##' DefinitionList
##' -------------------------------------
j <- pandoc_to_json("Block/DefinitionList.md", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)

#' Definition list Each list item is a pair consisting of a term (a list of inlines) 
#' and one or more definitions (each a list of blocks).
#' @param definition_list a list of key value pairs, the key is of type list of inlines and
#'                        and the values are list of lists of objects of type block.
#' @examples
#' key <- list(Str("key"))
#' value <- list(list(Plain(list(Str("value")))))
#' DefinitionList(list(list(key, value), list(key, value))))
## Attr String
DefinitionList <- function(definition_list) {
    list(t="DefinitionList", c=definition_list)
}

key <- list(Str("key"))
value <- list(list(Plain(list(Str("value")))))
DefinitionList(blocks)
test2(list(DefinitionList(list(list(key, value), list(key, value)))), "markdown")


