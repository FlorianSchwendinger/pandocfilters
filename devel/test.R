q("no")
R

require("pandocfilters", quietly = TRUE, warn.conflicts = FALSE)

test <- function(x, to="html") {
    empty_named_list <- setNames(list(), character())
    meta_data <- list(unMeta=empty_named_list)
    d <- list(meta_data, x)
    pandoc_from_json(as.character(jsonlite::toJSON(d, auto_unbox=TRUE)), to=to)
}

txt <- c(Emph(list(Str('R'))), Space(), Strong(list(Str('is'))), Space(),
         Strikeout(list(Str('a'))), Space(), Superscript(list(Str('system'))), 
         Space(), Subscript(list(Str('for'))), Space(), 
         SmallCaps(list(Str('statistical'))), Space(), 
         Quoted(list(Str("computation")), quote_type="SingleQuote"), Space(), 
         Quoted(list(Str("and")), quote_type="DoubleQuote"), Space(), 
         RawInline("html", "<i>graphics</i>"), Str('.'), LineBreak())

## create a new document
doc <- list()
## append header 1
doc <- append(doc, list(Header(list(Str("R Basics")))))
## append header 2
doc <- append(doc, list(Header(list(Str("What is R?")), level=2L)))
## append plain text
doc <- append(doc, list(Plain(txt)))
cat(pandocfilters:::test(doc, to="html"), file="test_1.html", sep="\n")

getwd()