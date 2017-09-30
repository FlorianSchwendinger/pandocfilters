#' Create a new Document.
#' 
#' A constructor of an object of type \code{"document"}.
#' 
#' @details Each document has the following methods:
#' \itemize{
#'   \item[] \code{to_json()}
#'      \itemize{ 
#'        \item[] \emph{Description}
#'        \item[] \itemize{ Returns the \code{JSON} representation of the document. } }
#'   \item[] \code{write(con, format="markdown", writer=pandocfilters_writer)}
#'      \itemize{ 
#'        \item[] \emph{Description}
#'        \item[] \itemize{\item[] Write the JSON-formatted AST to a connection.}
#'        \item[] \emph{Arguments}
#'        \item[] \itemize{\item[] \code{con}    \sspace a connection object or a character string to which the document is written  }
#'        \item[] \itemize{\item[] \code{format} \sspace a character string giving the format (e.g. \code{"latex"}, \code{"html"})  }
#'        \item[] \itemize{\item[] \code{writer} \sspace an optional writer function, see \link{pandocfilters_writer} }    
#'        \item[] \emph{Note}
#'        \item[] \itemize{\item[] Any function with the three arguments \code{x}, \code{con} and \code{format} can be used as writer function.}
#'      }
#'   \item[] \code{append(x)}
#'      \itemize{ 
#'        \item[] \emph{Description}
#'        \item[] \itemize{\item[] Append a new block to the document.}
#'        \item[] \emph{Arguments}
#'        \item[] \itemize{\item[] \code{x} \sspace a block object or list of block objects} }
#'   \item[] \code{append_plain(x)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Plain}. }
#'   \item[] \code{append_para(x)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Para}.}
#'   \item[] \code{append_code_block(attr, code)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{CodeBlock}.}
#'   \item[] \code{append_block_quote(blocks)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{BlockQuote}.}
#'   \item[] \code{append_ordered_list(lattr, lblocks)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{OrderedList}.}
#'   \item[] \code{append_bullet_list(lblocks)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{BulletList}.}
#'   \item[] \code{append_definition_list(x)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{DefinitionList}.}
#'   \item[] \code{append_header(x, level=1L, attr=Attr())} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Header}.}
#'   \item[] \code{append_horizontal_rule()} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{HorizontalRule}.}
#'   \item[] \code{append_table(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list())} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Table}.}
#'   \item[] \code{append_div(blocks, attr)} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Div}.}
#'   \item[] \code{append_null()} \sspace
#'      \itemize{ \item[] For more information about the arguments see \link{Null}.}
#' }
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
  env$append_table <- function(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list()) {
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
    d <- list(list(unMeta=self$meta), self$doc)
    return( jsonlite::toJSON(d, auto_unbox=TRUE) )
  }
  ## Write
  env$write <- function(con, format="markdown", writer=pandocfilters_writer) {
    self <- parent.env(environment())$env
    writer(self$to_json(), con, format)
  }
  structure(env, class="document")
}

##' @noRd
##' @export
print.document <- function(x, ...) print("A pandoc document.")

