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

## test(list(Math("x^2")))
setwd("/home/florian/SVN/baRpkgs/trunk/pandocfilters/devel/Constructors")

f <- "Inline"

## Constructors for inline elements
inline <- c('Str', 'Emph', 'Strong', 'Strikeout', 'Superscript',
            'Subscript', 'SmallCaps', 'Quoted', 'Cite', 'Code', 'Space',
            'LineBreak', 'Math', 'RawInline', 'Link', 'Image', 'Note',
            'SoftBreak', 'Span')

p <- file.path(f, sprintf("%s.md", inline))

x <- sapply(p, readLines)
todo <- x[nchar(x) == 0]
todo
x <- x[nchar(x) > 0]
x
## ----------------------------
## Str
## ----------------------------
i <- 1
x[i]
j <- pandoc_to_json(names(x[i]))
j
fjson(j)
fjson(j)[[2]][[1]]$c
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a character string
#' @examples
#' Str("SomeString")
Str <- function(x) list(t="Str", c=x)
Str("SomeString")
str(Str("SomeString"))

## ----------------------------
## Emph
## ----------------------------
i <- 2
x[i]
j <- pandoc_to_json(names(x[i]))
j
fjson(j)
fjson(j)[[2]][[1]]$c
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a list of inline pandoc objects
#' @examples
#' Emph(list(Str("emphasis")))
Emph <- function(x) list(t="Emph", c=x)
Emph(list(Str("emphasis")))
str(Emph(list(Str("emphasis"))))

## ----------------------------
## Strong
## ----------------------------
i <- 3
x[i]
j <- pandoc_to_json(names(x[i]))
j
fjson(j)
fjson(j)[[2]][[1]]$c
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a list of inline pandoc objects
#' @examples
#' Strong(list(Str("strong")))
Strong <- function(x) list(t="Strong", c=x)
Strong(list(Str("strong")))
str(Strong(list(Str("strong"))))

## ----------------------------
## Strikeout
## ----------------------------
i <- 4
x[i]
j <- pandoc_to_json(names(x[i]))
j
fjson(j)
fjson(j)[[2]][[1]]$c
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a list of inline pandoc objects
#' @examples
#' Strikeout(list(Str("strikeout")))
Strikeout <- function(x) list(t="Strikeout", c=x)
Strikeout(list(Str("strikeout")))
str(Strikeout(list(Str("strikeout"))))

## ----------------------------
## Superscipt
## ----------------------------
i <- 5
x[i]
j <- pandoc_to_json(names(x[i]), from="latex")
j
fjson(j)
fjson(j)[[2]][[1]]$c[[2]]
str(fjson(j)[[2]][[1]]$c[[2]])

#' @param x a list of inline pandoc objects
#' @examples
#' Superscript(list(Str("some text written in superscript")))
Superscript <- function(x) list(t="Superscript", c=x)
Superscript(list(Str("2")))
str(Superscript(list(Str("2"))))


## ----------------------------
## Subscript
## ----------------------------
i <- 6
x[i]
j <- pandoc_to_json(names(x[i]), from="latex")
j
fjson(j)
fjson(j)[[2]][[1]]$c[[2]]
str(fjson(j)[[2]][[1]]$c[[2]])

#' @param x a list of inline pandoc objects
#' @examples
#' Subscript(list(Str("some text written in superscript")))
Subscript <- function(x) list(t="Subscript", c=x)
Subscript(list(Str("2")))
str(Subscript(list(Str("2"))))


## ----------------------------
## Inline Code
## ----------------------------
i <- 7
x[i]
j <- pandoc_to_json(names(x[i]))
j
fjson(j)
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param code a character string giving the inline code
#' @param name a optional character string giving the name of the inline code chunk
#' @param language a optional character string giving the programming language
#' @param line_numbers a logical controling if line numbers should be used
#' @param start_from an integer giving the first line number
#' @examples
#' Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0)
#' Code("lm(hello ~ world)")
#' Subscript(list(Str("some text written in superscript")))
## type Attr = (String, [String], [(String, String)])
## Attributes: identifier, classes, key-value pairs
## one example on the pandoc homepage shows the following for
## inline code: `<$>`{.haskell}
## block code:
##     ~~~~ {#mycode .haskell .numberLines startFrom="100"}
##     qsort []     = []
##     qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
##                    qsort (filter (>= x) xs)
##     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     Here mycode is an identifier, haskell and numberLines are classes,
##     and startFrom is an attribute with value 100.
## or in html:
##    <pre id="mycode" class="haskell numberLines" startFrom="100"><code>... </code></pre>
Code <- function(code, name="", language=NULL, line_numbers=FALSE, start_from=1) {
    if (line_numbers) {
        linum <- list(list("startFrom", sprintf("%i", start_from)))
    } else {
        linum <- list()
    }
    if ( !is.null(language) ) {
        lang <- list(language)
    } else {
        lang <- list()
    }
    meta <- list(name, lang, linum)
    x <- list(meta, code)
    list(t="Code", c=x)
}
Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0)
str(Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0))
str(Code("lm(hello ~ world)"))


List of 2
 $ t: chr "Code"
 $ c:List of 2
  ..$ :List of 3
  .. ..$ : chr ""
  .. ..$ : list()
  .. ..$ : list()
  ..$ : chr "{r}inline_code"

List of 2
 $ t: chr "Code"
 $ c:List of 2
  ..$ :List of 3
  .. ..$ : chr ""
  .. ..$ : list()
  .. ..$ : list()
  ..$ : chr "inline_code"


## ----------------------------
## Math
## ----------------------------
i <- 8
x[i]
j <- pandoc_to_json(names(x[i]), from="latex")
j
fjson(j)
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a list of inline pandoc objects
#' @examples
#' Math(list(Str("some text written in superscript")))
## Math MathType String	
TeX math (literal)
InlineMath <- function() list(t="InlineMath", c=list())
InlineMath(list())
Math <- function(x) list(t="Math", c=list(InlineMath(), x))
Math("math^4*3")
str(Math("math^4*3"))

Str("av")

## ----------------------------
## Link
## ----------------------------
i <- 9
x[i]
j <- pandoc_to_json(names(x[i]))
getwd()
j <- pandoc_to_json("Inline/Link.html", "html")
j <- pandoc_to_json("Inline/Link.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param target a character string giving the target (hyper reference)
#' @param text a list of inline pandoc objects which will be shown
#' @param some alternatice text
#' @examples
#' Link("https://cran.r-project.org/", list(Str("Text_Shown")), "some_alternative_text")
#' Subscript(list(Str("some text written in superscript")))
## <A HREF="url.html" TITLE="some_alterinative_text">the_text_shown</A>
## Link: Attr [Inline] Target | Hyperlink: alt text (list of inlines), target
Link <- function(target, text, attr=Attr()) {
    if ( get_pandoc_version() < 1.16 ) {
        list(t="Link", c=list(text, list(target, attr[[1]])))        
    }
    target <- as.list(target)
    if ( length(target) == 1) target[[2]] <- ""
    list(t="Link", c=list(attr, text, target))
}
Link15 <- function(target, text, alt_text="") list(t="Link", c=list(text, list(target, alt_text)))
Link16 <- function(target, text, alt_text="") {
    list(t="Link", c=list(text, target, alt_text))
}
str(Attr())
str(fjson(j)[[2]][[1]]$c[[1]])

target <- c("https://cran.r-project.org/", "title_A")
loio <- list(Str("Text_Shown"))
attr <- Attr()
attr <- Attr("A", list("B"), list(c("C", "D")))
inline <- Link(target, loio, attr)

str(inline)
str(fjson(j)[[2]][[1]]$c[[1]])
test(list(inline))
test(list(inline), "latex")
test(list(inline), "markdown")


## ----------------------------
## Image
## ----------------------------
i <- 10
x[i]
j <- pandoc_to_json(names(x[i]))
j <- pandoc_to_json("Inline/Image.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param target a character string giving the target (hyper reference)
#' @param text a list of inline pandoc objects which will be shown
#' @param caption a character string describing the picture
#' @examples
#' Image("https:://Rlogo.jpg", list(Str("some_text")), "fig:some_caption")
## Image Attr [Inline] Target | Image: alt text (list of inlines), target
Image <- function(target, text, caption="") list(t="Image", c=list(text, list(target, caption)))
str(Image("https:://Rlogo.jpg", list(Str("some_text")), "fig:some_caption"))
test(list(Image("https:://Rlogo.jpg", list(Str("some_text")), "fig:some_caption")))


## ----------------------------
## SmallCaps
## ----------------------------
i <- 11
x[i]
j <- pandoc_to_json("Inline/SmallCaps.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param x a list of inline pandoc objects
#' @examples
#' SmallCaps(list(Str("The latex command for 'small caps' is 'textsc'")))
SmallCaps <- function(x) list(t="SmallCaps", c=x)
SmallCaps(list(Str("some text")))
str(SmallCaps(list(Str("some text"))))
test(list(SmallCaps(list(Str("some text")))))
test(list(SmallCaps(list(Str("some text")))), to="latex")

## ----------------------------
## SingleQuote
## ----------------------------
#' @param x a character string
#' @examples
#' SingleQuote("single quote")
SingleQuote <- function(x) list(t="SingleQuote", c=x)

## ----------------------------
## DoubleQuote
## ----------------------------
#' @param x a character string
#' @examples
#' DoubleQuote("double quote")
DoubleQuote <- function(x) list(t="DoubleQuote", c=x)


## ----------------------------
## Quoted
## ----------------------------
j <- pandoc_to_json("Inline/Quoted.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param quote_type a character giving the quote type,
#'                   valid types are \code{"SingleQuote"} and \code{"DoubleQuote"}
#' @param x a list of inline pandoc objects
#' @examples
#' Quoted(list(Str("some text")), quote_type="SingleQuote")
#' Quoted(list(Str("some text")), quote_type="DoubleQuote")
## Quoted QuoteType [Inline]
## Quoted text (list of inlines)
Quoted <- function(x, quote_type="DoubleQuote") {
    list(t="Quoted", c=list(list(t=quote_type, c=list()), x))
}
Quoted(list(Str("some text")))
str(Quoted(list(Str("some text"))))
test(list(Quoted(list(Str("some text")))))
test(list(Quoted(list(Str("some text")))), to="latex")
test(list(Quoted(list(Str("some text")), quote_type="SingleQuote")), to="latex")

## ----------------------------
## RawInline
## ----------------------------
#' @param format a character string giving the fromat (e.g. \code{"latex"})
#' @param x a character string giving the inline
#' @examples
#' RawInline("latex", "some RawInline")
RawInline <- function(format, x) {
    list(t="RawInline", c=list(format, x))
}
str(RawInline("latex", "some RawInline"))
test(list(RawInline("latex", "some RawInline")), to="latex")



## ----------------------------
## Cite
## ----------------------------
## Citation	 
##   citationId :: String
##   citationPrefix :: [Inline]
##   citationSuffix :: [Inline]
##   citationMode :: CitationMode
##   citationNoteNum :: Int
##   citationHash :: Int
##
## mode: AuthorInText, SuppressAuthor, NormalCitation

##  Citation
##
#' @param suffix a list of inline objects
#' @param id a character string 
#' @param note_num an integer 
#' @param mode a character string giving the citaiton mode possible values are 
#'             \dQuote{AuthorInText}, \dQuote{SuppressAuthor} and \dQuote{NormalCitation}.
#' @param prefix  a list of inline objects
#' @param hash an integer
Citation <- function(suffix, id, note_num=0L, mode="AuthorInText", prefix=list(), hash=0L) {
    list(citationSuffix = suffix,
         citationNoteNum = note_num,
         citationMode = list(t=mode, c=list()),
         citationPrefix = prefix,
         citationId = id,
         citationHash = hash)
}

test(list(Citation("A", "B")), to="latex")

## ----------------------------
## Cite
## ----------------------------
j <- pandoc_to_json("Inline/Cite.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param quote_type a character giving the quote type,
#'                   valid types are \code{"SingleQuote"} and \code{"DoubleQuote"}
#' @param x a list of inline pandoc objects
#' @examples
## Cite [Citation] [Inline]	
## Citation (list of inlines)
Cite <- function(Citation, x) {
    list(t="Cite", c=list(Citation, x))
}

ci <- fjson(j)[[2]][[1]]$c[[1]]
str(ci)

pandoc_from_json(as.character( jsonlite::toJSON(fjson(j), auto_unbox=TRUE) ), to="latex")

test(list(ci), "latex")

## Esolang, A. N. (2014). Obscure Reference Generator [Computer software]. 
## Washington, DC: E & K Press.

## <cite><a href="http://www.brucelawson.co.uk/2013/on-citing-quotations-again/">Bruce Lawson</a>

## Cite Example
ci <- Citation(suffix=list(Str("SuffixA")),
               id="CitationIdA", prefix=list(Str("prefix")))
str(ci)



ci <- Citation(suffix=list(Str("SuffixA")),
               id="CitationIdA", prefix=list(Str("prefix")))
str(ci)
 2006-2015 John MacFarlane (jgm@berkeley.edu). 
 Released under the GPL, version 2 or greater. 

str(ci)
test(list(Cite(list(ci), list(Str("")))))
test(list(Cite(list(ci), list(Str("cite")))), "latex")
test(list(Cite(list(ci), list(Str("text shown")))), "latex")

Quoted(list(Str("some text")))
str(Quoted(list(Str("some text"))))
test(list(Quoted(list(Str("some text")))))
test(list(ci), to="latex")
test(list(Quoted(list(Str("some text")), quote_type="SingleQuote")), to="latex")

## ----------------------------
## Span
## ----------------------------
j <- pandoc_to_json("Inline/Span.html", "markdown")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c[[1]])

#' @param attr an object of type Attr
#' @param inline a list of inline pandoc objects which will be shown
#' @examples
#' 
Span <- function(attr, inline) list(t="Span", c=list(attr, list(inline)))

#' @param identifier a character string
#' @param classes a list of character
#' @param key_val_pairs a list of tuple of type character
#' @examples
## type Attr = (String, [String], [(String, String)])      
Attr <- function(identifier, classes, key_val_pairs) {
    list(identifier, classes, key_val_pairs)
}         

str(Span(Attr("A", list("B"), list(c("C", "D"))), Str("some inline string")))
test(list(Span(Attr("A", list("B"), list(c("C", "D"))), Str("some inline string"))))


test(list(Para("some para")))


     
