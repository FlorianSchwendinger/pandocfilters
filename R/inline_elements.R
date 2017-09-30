##
##   Inline Element Constructors
##
##  1. Str
##  2. Emph
##  3. Strong
##  4. Strikeout
##  5. Superscript
##  6. Subscript
##  7. SmallCaps
##  8. Quoted
##  9. Cite
## 10. Code
## 11. Space
## 12. SoftBreak
## 13. LineBreak
## 14. Math
## 15. RawInline
## 16. Link
## 17. Image
## 18. Note
## 19. Span


#   1. Str

#' Text (String).
#' 
#' A constructor of an inline object of type `"Str"`.
#' 
#' To minimize the amount of unnecessary typing, pandoc filters automatically converts character strings to pandoc objects of type `"Str"` if needed. Furthermore, if a single inline object is provided where a list of inline objects is needed \pkg{pandocfilters} automatically converts this inline object into a list of inline objects. For example, the canonical way to emphasize the character string `"some text"` would be `Emph(list(Str("some text")))`. 
#' 
#' Since single inline objects are automatically transformed to lists of inline objects, this is equivalent to `Emph(Str("some text"))`. Since a character string is automatically transformed to an inline object, this is is equivalent to `Emph("some string")`. In short, whenever a list of inline objects is needed one can also use a single inline object or a character string.
#'   
#' @param x a character string
#' @examples
#' Str("SomeString")
#' 
#' @family Inline element constructors
#' @export
Str <- function(x) {
  structure(
    list(t="Str", c=x), 
    class=c("inline", "list")
  )
}


#  2. Emph
#'
#' Emphasized Text.
#' 
#' A constructor of an inline object of type `"Emph"`.
#' 
#' @param x a inline object or a list of inline objects
#' @examples
#' Emph("emphasize")
#' 
#' @family Inline element constructors
#' @export
Emph <- function(x) {
  structure(list(t="Emph", c=as.loio(x)), class=c("inline", "list"))
}


#  3. Strong
#'
#' Strongly Emphasized Text.
#' 
#' A constructor of an inline object of type `"Strong"`.
#' 
#' @param x a inline object or a list of inline objects
#' @examples
#' Strong("strong")
#' 
#' @family Inline element constructors
#' @export
Strong <- function(x) {
  structure(
    list(t="Strong", 
         c=as.loio(x)), 
    class=c("inline", "list")
  )
}


#  4. Strikeout
#'
#' Strikeout Text.
#' 
#' A constructor of an inline object of type `"Strikeout"`.
#' 
#' @param x a inline object or a list of inline objects
#' @examples
#' Strikeout("strikeout")
#' 
#' @family Inline element constructors
#' @export
Strikeout <- function(x) {
  structure(list(t="Strikeout", c=as.loio(x)), class=c("inline", "list"))
}


#  5. Superscript
#'
#' Superscripted Text.
#' 
#' A constructor of an inline object of type `"Superscript"`.
#' 
#' @param x a inline object or a list of inline objects
#' @examples
#' Superscript("some text written in superscript")
#' 
#' @family Inline element constructors
#' @export
Superscript <- function(x) {
  structure(
    list(
      t="Superscript", 
      c=as.loio(x)), 
    class=c("inline", "list")
  )
}


#  6. Subscript
#'
#' Subscripted Text.
#' 
#' A constructor of an inline object of type `"Subscript"`.
#' 
#' @param x a inline object or a list of inline objects
#' @examples
#' Subscript("some text written in superscript")
#' 
#' @family Inline element constructors
#' @export
Subscript <- function(x) {
  structure(list(t="Subscript", c=as.loio(x)), class=c("inline", "list"))
}


#  7. SmallCaps
#'=====
#' Small Caps Text
#' A constructor of an inline object of type `"SmallCaps"`.
#' @param x a inline object or a list of inline objects
#' @examples 
#' SmallCaps("The latex command for 'small caps' is 'textsc'!")
#' 
#' @family Inline element constructors
#' @export
SmallCaps <- function(x) {
  structure(
    list(
      t="SmallCaps", 
      c=as.loio(x)), 
    class=c("inline", "list")
  )
}


#  8. Quoted
#'
#' Quoted Text.
#' 
#' A constructor of an inline object of type `"Quoted"`.
#' 
#' @param x a inline object or a list of inline objects
#' @param quote_type a character giving the quote type,
#'                   valid types are `"SingleQuote"` and `"DoubleQuote"`
#' @examples
#' Quoted("some text", quote_type="SingleQuote")
#' Quoted("some text", quote_type="DoubleQuote")
#' 
#' @family Inline element constructors
#' @export
## Quoted QuoteType [Inline]
## Quoted text (list of inlines)
Quoted <- function(x, quote_type="DoubleQuote") {
  structure(list(t="Quoted", c=list(list(t=quote_type, c=list()), as.loio(x))), 
            class=c("inline", "list"))
}


#  9. Cite
#'
#' Citation
#' A constructor of an inline object of type `"Cite"`.
#' @param citation an object of type `"Citation"`
#' @param x a inline object or a list of inline objects
#' @examples
#' ci <- Citation(suffix=list(Str("Suffix_1")),
#'                id="Citation_ID_1", prefix=list(Str("Prefix_1")))
#' Cite(ci, Str("some text"))
#' 
#' @family Inline element constructors
#' @export
## Cite [Citation] [Inline] 
## Citation (list of inlines)
Cite <- function(citation, x) {
  if ( is.citation(citation) ) citation <- list(citation)
  structure(
    list(
      t="Cite", 
      c=list(citation, as.loio(x))), 
    class=c("inline", "list")
  )
}


#  10. Code
#'
#' Inline Code.
#' 
#' A constructor of an inline object of type `"Code"`.
#' 
#' @param code a character string giving the inline code
#' @param name an optional character string giving the name of the inline code chunk
#' @param language an optional character string giving the programming language
#' @param line_numbers a logical which controls if line numbers should be used
#' @param start_from an integer giving the first line number
#' 
#' @examples
#' Code("lm(hello ~ world)", "my_r_inline_code", "R", TRUE, 0)
#' Code("lm(hello ~ world)")
#' 
#' @family Inline element constructors
#' @export
## Additional material (from the pandoc homepage)
## 
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
  linum <- if (line_numbers) {
    list(list("startFrom", sprintf("%i", start_from)))
  } else {
    list()
  }
  lang <- if ( !is.null(language) ) {
    list(language)
  } else {
    list()
  }
  meta <- list(name, lang, linum)
  x <- list(meta, code)
  structure(
    list(
      t="Code", 
      c=x), 
    class=c("inline", "list")
  )
}


#  11. Space
#'
#' Inter-word space.
#' 
#' A constructor of an inline object of type `"Space"`.
#' 
#' @examples
#' Space()
#' 
#' @family Inline element constructors
#' @export
Space <- function() structure(Type("Space"), class=c("inline", "list"))



#  12. SoftBreak
#'
#
#  NOTE: SoftBreak, the created data structure should be correct but
#        I couldn't figure out what it actually does.
#
#' Soft Line Break.
#' 
#' A constructor of an inline object of type `"SoftBreak"`.
#' @examples
#' SoftBreak()
#' 
#' @family Inline element constructors
#' @export
SoftBreak <- function()
  structure(
    Type("SoftBreak"), 
    class=c("inline", "list")
  )


#  13. LineBreak
#'
#' Hard Line Break.
#' 
#' A constructor of an inline object of type `"LineBreak"`.
#' 
#' @examples
#' LineBreak()
#' 
#' @family Inline element constructors
#' @export
LineBreak <- function() 
  structure(
    Type("LineBreak"), 
    class=c("inline", "list")
  )


#  14. Math

#' TeX Math. 
#' 
#' A constructor of an inline object of type `"Math"`.
#'
#' @param x a character string 
#' @examples
#' Math("3*x^2")
#' 
#' @family Inline element constructors
#' @export
Math <- function(x) 
  structure(
    list(
      t="Math", 
      c=list(Type("InlineMath"), x)), 
    class=c("inline", "list")
  )



#  15. RawInline
#'
#' Raw Inline.
#' 
#' A constructor of an inline object of type `"RawInline"`.
#' 
#' @param format a character string giving the format (e.g. `"latex"`, `"html"`)
#' @param x a character string giving the inline
#' @examples
#' RawInline("latex", "some RawInline")
#' 
#' @family Inline element constructors
#' @export
RawInline <- function(format, x) {
  structure(list(t="RawInline", c=list(format, x)), class=c("inline", "list"))
}


#  16. Link
#'
#' Hyperlink.
#' 
#' A constructor of an inline object of type `"Link"`.
#' 
#' @param target a character string giving the target (hyper reference)
#' @param text a inline object or a list of inline objects giving the visible part
#' @param title an optional character string giving the title
#' @param attr an optional object of type `"Attr"`
#' @details Further Usage examples can be found in the README.
#' @examples
#' Link("https://cran.r-project.org/", "Text_Shown", "some title")
#' 
#' @family Inline element constructors
#' @export
## <A HREF="url.html" TITLE="some_alterinative_text">the_text_shown</A>
## Link Attr [Inline] Target | Hyperlink: "alt text" (list of inlines), target
Link <- function(target, text, title="", attr=Attr()) {
  if ( get_pandoc_version() < 1.16 ) {
    return( structure(list(t="Link", c=list(as.loio(text), 
                                            list(target, title))), 
                      class=c("inline", "list")) )
  }
  structure(
    list(t="Link", c=list(attr,
                          as.loio(text), 
                          list(target, title))), 
    class=c("inline", "list")
    )
}


#' Image.
#' 
#' A constructor of an inline object of type `"Image"`.
#' 
#' @param target a character string giving the target (hyper reference)
#' @param text a inline object or a list of inline objects giving the visible part
#' @param caption a character string describing the picture
#' @param attr an optional object of type `"Attr"`
#' @details Further Usage examples can be found in the README.
#' @examples
#' Image("https:://Rlogo.jpg", "some_text", "fig:some_caption")
#' 
#' @family Inline element constructors
#' @export
## Image Attr [Inline] Target | Image: alt text (list of inlines), target
Image <- function(target, text, caption="", attr=Attr()) {
  if ( get_pandoc_version() < 1.16 ) {
    return(
      structure(
        list(t="Image", 
             c=list(
               as.loio(text),
               list(target, caption))
        ),
        class=c("inline", "list")) 
    )
  }
  structure(
    list(
      t="Image", 
      c=list(attr,as.loio(text), list(target, caption))
    ),
    class=c("inline", "list")
  )
}



#  18. Note
#'
#' Note.
#' 
#' A constructor of an inline object of type `"Note"`.
#' 
#' @param x a pandoc block object or a list of pandoc block objects
#' @examples
#' block <- Plain("x")
#' Note(block)
#' 
#' @family Inline element constructors
#' @export
## Note [Block]
## note <- Note(block)
## pandocfilters:::test(list(Plain(note)))
Note <- function(x) {
  structure(
    list(
      t="Note", 
      c=as.lobo(x)
    ), 
    class=c("inline", "list")
  )
}



#  19. Span

#' Generic Inline Container with Attributes.
#' 
#' A constructor of an inline object of type `"Span"`.
#' 
#' @param attr an object of type `"Attr"`
#' @param inline a inline object or a list of inline objects which will be shown
#' @examples
#' attr <- Attr("A", "B", list(c("C", "D")))
#' Span(attr, "some inline string")
#' 
#' @family Inline element constructors
#' @export
Span <- function(attr, inline) {
  structure(
    list(
      t="Span", 
      c=list(attr, as.loio(inline))), 
    class=c("inline", "list")
  )
}

