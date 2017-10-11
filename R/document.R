#' Create a new Document.
#' 
#' A constructor of an object of type `"document"`.
#' 
#' Each document has the following methods:
#' 
#' *  `to_json()
#'     * Description
#'          * Returns the `JSON` representation of the document.
#' *  `write(con, format="markdown", writer=pandocfilters_writer)`
#'     * Description
#'          *  Write the JSON-formatted AST to a connection.
#'     * Arguments
#'          * `con` : a connection object or a character string to which the document is written
#'          * `format` : a character string giving the format (e.g. `"latex"`, `"html"`)
#'          * `writer` : an optional writer function, see [pandocfilters_writer]    
#'     *  Note
#'          * Any function with the three arguments `x`, `con` and `format` can be used as writer function.
#'      
#' * `append(x)`
#'     * Description
#'         * Append a new block to the document.
#'     * Arguments
#'         * `x` : a block object or list of block objects
#' *  `append_plain(x)`
#'       * see [Plain]
#' *  `append_para(x)`
#'       * see [Para]
#' *  `append_code_block(attr, code)`
#'       * see [CodeBlock]
#' *  `append_block_quote(blocks)`
#'       * see [BlockQuote]
#' *  `append_ordered_list(lattr, lblocks)`
#'       * see [OrderedList]
#' *  `append_bullet_list(lblocks)`
#'       * see [BulletList]
#' *  `append_definition_list(x)` 
#'       * see [DefinitionList]
#' *  `append_header(x, level=1L, attr=Attr())`
#'       * see [Header]
#' *  `append_horizontal_rule()`
#'       * see [HorizontalRule]
#' *  `append_table(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list())`
#'       * see [Table]
#' *  `append_div(blocks, attr)`
#'       * see [Div]
#' *  `append_null()`
#'       * see [Null]
#'
#' @export
document <- function() {
  env <- new.env()
  env$doc <- list()
  env$meta <- nlist()
  env$append <- function(x) {
    self <- parent.env(environment())$env
    self$doc <- c(self$doc, as.lobo(x))
    invisible(NULL)
  }
  
  ##  1. Plain
  env$append_plain <- function(x) {
    self <- parent.env(environment())$env
    self$append(Plain(x))
  }
  
  ##  2. Para
  env$append_para <- function(x) {
    self <- parent.env(environment())$env
    self$append(Para(x))
  }
  
  ##  3. CodeBlock
  env$append_code_block <- function(attr, code) {
    self <- parent.env(environment())$env
    self$append(CodeBlock(attr, code))
  }
  
  ##  4. RawBlock
  
  ##  5. BlockQuote
  env$append_block_quote <- function(blocks) {
    self <- parent.env(environment())$env
    self$append(BlockQuote(blocks))
  }
  
  ##  6. OrderedList
  env$append_ordered_list <- function(lattr, lblocks) {
    self <- parent.env(environment())$env
    self$append(OrderedList(lattr, lblocks))
  }
  
  ##  7. BulletList
  env$append_bullet_list <- function(lblocks) {
    self <- parent.env(environment())$env
    self$append(BulletList(lblocks))
  }
  
  ##  8. DefinitionList
  env$append_definition_list <- function(x) {
    self <- parent.env(environment())$env
    self$append(DefinitionList(x))
  }
  
  ##  9. Header
  env$append_header <- function(x, level=1L, attr=Attr()) {
    self <- parent.env(environment())$env
    self$append(Header(x, level, attr))
  }
  
  ## 10. HorizontalRule 
  env$append_horizontal_rule <- function() {
    self <- parent.env(environment())$env
    self$append(HorizontalRule())
  }
  
  ## 11. Table
  env$append_table <- function(rows, col_names=NULL, aligns=NULL,
                               col_width=NULL, caption=list()) {
    self <- parent.env(environment())$env
    self$append(Table(rows, col_names, aligns, col_width, caption))
  }
  
  ## 12. Div
  env$append_div <- function(blocks, attr) {
    self <- parent.env(environment())$env
    self$append(Div(blocks, attr))
  }
  
  ## 13. Null
  env$append_null <- function() {
    self <- parent.env(environment())$env
    self$append(Null())
  }
  
  ## to_json
  env$to_json <- function() {
    self <- parent.env(environment())$env
    # d <- list(list(unMeta=self$meta), self$doc)
    # jsonlite::toJSON(d, auto_unbox=TRUE)
    to_pandoc_json(self$doc, meta = self$meta)
  }
  
  ## Write
  env$write <- function(con = stdout(), format="markdown", 
                        writer=pandocfilters_writer) {
    self <- parent.env(environment())$env
    match.fun(writer)(self$to_json(), con, format)
  }
  structure(env, class="document")
}

##' @noRd
##' @export
print.document <- function(x, ...) print("A pandoc document.")

