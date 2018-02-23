nlist <- function(...) {
  x <- list(...)
  names(x) <- character(0)
  x
}


pan_document <- R6::R6Class(
  "pan_document",
  public = list(
    doc = list(),
    meta = nlist(),
    ## append
    append = function(x) {
      self$doc <- c(self$doc, as.lobo(x))
      invisible(self)
    },
    ##  1. Plain
    append_plain = function(x) {
      "appends plain text block ----------"
      self$append(Plain(x))
      invisible(self)
    },
    ##  2. Para
    append_para = function(x) {
      self$append(Para(x))
      invisible(self)
    },
    ##  3. CodeBlock
    append_code_block = function(attr, code) {
      self$append(CodeBlock(attr, code))
      invisible(self)
    },
    
    ##  4. RawBlock
    
    ##  5. BlockQuote
    append_block_quote = function(blocks) {
      self$append(BlockQuote(blocks))
      invisible(self)
    },
    
    ##  6. OrderedList
    append_ordered_list = function(lattr, lblocks) {
      self$append(OrderedList(lattr, lblocks))
      invisible(self)
    },
    
    ##  7. BulletList
    append_bullet_list = function(lblocks) {
      self$append(BulletList(lblocks))
      invisible(self)
    },
    
    ##  8. DefinitionList
    append_definition_list = function(x) {
      self$append(DefinitionList(x))
      invisible(self)
    },
    
    ##  9. Header
    append_header = function(x, level=1L, attr=Attr()) {
      self$append(Header(x, level, attr))
      invisible(self)
    },
    
    ## 10. HorizontalRule 
    append_horizontal_rule = function() {
      self$append(HorizontalRule())
      invisible(self)
    },
    
    ## 11. Table
    append_table = function(rows, col_names=NULL, aligns=NULL,
                                 col_width=NULL, caption=list()) {
      self$append(Table(rows, col_names, aligns, col_width, caption))
      invisible(self)
    },
    
    ## 12. Div
    append_div = function(blocks, attr) {
      self$append(Div(blocks, attr))
      invisible(self)
    },
    
    ## 13. Null
    append_null = function() {
      self$append(Null())
      invisible(self)
    },
    
    ## to_json
    to_json = function() {
      to_pandoc_json(self$doc, meta = self$meta)
    },
    
    ## Write
    write = function(con = stdout(), format = "markdown", 
                          writer = pandocfilters_writer) {
      match.fun(writer)(self$to_json(), con, format)
    }
    
  )
)


#' Create a new Document.
#' 
#' Class for creating documents using pandoc format.
#' 
#' @docType class
#' @rdname document
#' @importFrom R6 R6Class
#' 
#' @section Usage:
#' \preformatted{
#' d <- document()
#' d$append(x) 
#' d$append_block_quote(blocks) 
#' d$append_bullet_list(lblocks) 
#' d$append_code_block(attr, code) 
#' d$append_definition_list(x) 
#' d$append_div(blocks, attr) 
#' d$append_header(x, level = 1L, attr = Attr()) 
#' d$append_horizontal_rule() 
#' d$append_null() 
#' d$append_ordered_list(lattr, lblocks) 
#' d$append_para(x) 
#' d$append_plain(x) 
#' d$append_table(rows, col_names = NULL, aligns = NULL, col_width = NULL)
#' d$to_json() 
#' d$write(con, format = "markdown", writer = pandocfilters_writer)
#' 
#' print(d)
#' }
#' 
#' @section Arguments:
#' \describe{
#' \item{x}{a block object or list of block objects}
#' \item{blocks}{passed to [BlockQuote()]}
#' \item{lbocks}{see [OrderedList()]}
#' \item{attr}{see [Attr()]}
#' \item{code}{see [CodeBlock()]}
#' \item{level}{see [Header()]}
#' \item{lattr}{see [OrderedList()]}
#' \item{lblocks}{see [BulletList()]}
#' \item{rows}{see [Table()]}
#' \item{col_names}{see [Table()]}
#' \item{aligns}{see [Table()]}
#' \item{col_width}{see [Table()]}
#' \item{con}{a connection object or a character string to which the document is written}
#' \item{format}{a character string giving the format (e.g. `latex`, `html`)}
#' \item{writer}{an optional writer function, see [pandocfilters_writer()]. Any function with the three arguments `x`, `con` and `format` can be used as writer function}
#' }
#' 
#' @section Details:
#' `append(x)` appends a new block to the document.
#' 
#' `append_plain(x)`: appends [Plain()]
#' 
#' `append_para(x)`: appends [Para()]
#' 
#' `append_code_block()`:  appends [CodeBlock()]
#' 
#' `append_block_quote()`: appends [BlockQuote()]
#' 
#' `append_ordered_list()`: appends [OrderedList()]
#' 
#' `append_bullet_list()`: appends [BulletList()]
#' 
#' `append_definition_list()`: appends [DefinitionList()]
#' 
#' `append_header()`: appends [Header()]
#' 
#' `append_horizontal_rule()`: appends [HorizontalRule()]
#' 
#' `append_table()`: appends [Table()]
#' 
#' `append_div()`: appends [Div()]
#' 
#' `append_null()`: appends [Null()]
#' 
#' `to_json()` returns the `JSON` representation of the document. See also [to_pandoc_json()]
#' 
#' `write()` writes the JSON-formatted AST to a connection. See also [pandocfilters_writer()]
#'
#' @export 
document <- function()pan_document$new()
