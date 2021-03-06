## Imports


##
##   Block Element Constructors
##

##  1. Plain
##  2. Para
##  3. CodeBlock
##  4. RawBlock
##  5. BlockQuote
##  6. OrderedList
##  7. BulletList
##  8. DefinitionList
##  9. Header
## 10. HorizontalRule
## 11. Table
## 12. Div
## 13. Null

Block <- function(t, c){
  z <- list(t = t, c = c)
  class(z) <- c("block", "list")
  z
}


#' Plain Text.
#' 
#' Constructs a block object of type `Plain`, a plain paragrph.
#' 
#' @param x a inline object or list of inline objects
#' @examples
#' Plain("x")
#' 
#' @family Block element constructors
#' @export
Plain <- function(x) {
  Block(t ="Plain", c = as.loio(x))
}


#' Paragraph.
#' 
#' Constructs a block object of type `Para`.
#' 
#' @inheritParams Plain
#' @examples
#' Para("x")
#' 
#' @family Block element constructors
#' @export
Para <- function(x) {
  Block(t = "Para", c = as.loio(x))
}


#' Code Block.
#' 
#' Constructs a block object of type `CodeBlock`.
#' 
#' @param attr an object of type [Attr]
#' @param code a character string containing the source code.
#' @examples
#' attr <- Attr("id", "Programming Language", list(c("key", "value")))
#' code <- "x <- 3\nprint('Hello R!')"
#' CodeBlock(attr, code)
#' 
#' @family Block element constructors
#' @export
## Attr String
CodeBlock <- function(attr, code) {
  Block(t = "CodeBlock", c = list(attr, code))
}


#  4. RawBlock
# #TODO: RawBlock <- elt('RawBlock', 2)
# #NOTE: Currently not implemented since


#' Block Quote.
#' 
#' Constructs a block object of type `BlockQuote`.
#' 
#' @param blocks a block object or list of block objects
#' @examples
#' BlockQuote(Plain("Hello R!"))
#' 
#' @family Block element constructors
#' @export
## Attr String
BlockQuote <- function(blocks) {
  Block(t = "BlockQuote", c = as.lobo(blocks))
}


#' Ordered List.
#' 
#' Constructs a block object of type `OrderedList`.
#' 
#' @param lattr a list of attributes
#' @param llblocks a list of lists of blocks
#' @examples
#' ordered_1 <- Plain("A")
#' ordered_2 <- list(Plain(Str("B")))
#' ordered_3 <- list(Plain(list(Str("C"))))
#' OrderedList(ListAttributes(), ordered_1)
#' OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))
#' 
#' @family Block element constructors
#' @export
## Attr String
OrderedList <- function(lattr, llblocks) {
  Block(t = "OrderedList", c = list(lattr, as.lolobo(llblocks)))
}


#' Bullet List.
#' 
#' Constructs a block object of type `BulletList`.
#' 
#' @param llblocks a list of lists of blocks
#' @examples
#' bullet_1 <- Plain("A")
#' bullet_2 <- Plain(Str("B"))
#' bullet_3 <- list(Plain(list(Str("C"))))
#' BulletList(list(bullet_1, bullet_2, bullet_3))
#' @export
## Attr String
BulletList <- function(llblocks) {
  Block(t = "BulletList", c=as.lolobo(llblocks))
}


#' Definition.
#' 
#' Constructs a `Definition` which can be used as an element of a [DefinitionList].
#' 
#' @param key a inline object or list of inline objects 
#' @param value a block object or list of block objects
#' @examples
#' Definition("some key", Plain("some value"))
#' 
#' @family Block element constructors
#' @export
Definition <- function(key, value) {
  list(as.loio(key), as.lolobo(value))
}


#' Definition List.
#' 
#' Constructs a block object of type `DefinitionList`.
#' 
#' @param x a list of key value pairs, the key is a list of `inline` objects and the values are a list of lists of objects of type `block`.
#' @details In the pandoc API \url{http://johnmacfarlane.net/BayHac2014/doc/pandoc-types/Text-Pandoc-Definition.html} the `DefinitionList` is described as follows, each list item is a pair consisting of a term (a list of `inline` objects) and one or more definitions (each a list of blocks).
#' @examples
#' key <- list(Str("key"))
#' value <- list(list(Plain(list(Str("value")))))
#' DefinitionList(list(list(key, value), Definition("some key", Plain("some value"))))
#' 
#' @family Block element constructors
#' @export
## Attr String
DefinitionList <- function(x) {
  Block(t = "DefinitionList", c = x)
}


#' Header.
#' 
#' Constructs a block object of type `Header`.
#' 
#' @param x a inline object or a list of inline objects
#' @param level an integer giving the level
#' @param attr an object of type [Attr]
#' @examples
#' Header("My Header")
#' 
#' @family Block element constructors
#' @export
Header <- function(x, level=1L, attr=Attr()) {
  Block(t = "Header", c = list(level, attr, as.loio(x)))
}


#' Horizontal Rule.
#' 
#' Constructs a block object of type `HorizontalRule`.
#' 
#' @examples
#' HorizontalRule()
#' 
#' @family Block element constructors
#' @export
## Attr String
HorizontalRule <- function() {
  Block(t = "HorizontalRule", c = list() )
}


#' Table.
#' 
#' Constructs a block object of type `Table`.
#' 
#' @param rows an object of class `matrix`, `data.frame`, `table` or a list of lists of pandoc objects of type [TableCell]
#' @param col_names a list of objects of type [TableCell]
#' @param aligns a character vector of alignments, possible values are `l` (left), `r` (right), `c` (center) and `d` (default).
#' @param col_width a numeric vector
#' @param caption a inline object or a list of inline objects giving the caption
#' @details Table, with caption, column alignments (required), relative column widths (0 = default), column headers (each a list of blocks), and rows (each a list of lists of blocks)
#' 
#' @importFrom stats setNames
#' 
#' @family Block element constructors
#' @export
## Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
Table <- function(rows, col_names=NULL, aligns=NULL, col_width=NULL, 
                  caption=list() ) {
  
  if ( is.null(col_names) & (! is.null(colnames(rows))) ) {
    col_names <- colnames(rows)
  }
  
  if ( is.matrix(rows) || is.data.frame(rows) || is.table(rows) ) {
    
    if ( is.table(rows) ) {
      rows <- as.matrix(rows)
      if ( min(dim(rows)) == 1 ) {
        rows <- t(rows)
      }
    }
    
    col_fun <- function(m, n) TableCell(as.character(rows[[m, n]]))
    row_fun <- function(m) lapply(seq_len(ncol(rows)), 
                                  function(n) col_fun(m, n))
    rows <- lapply(seq_len(nrow(rows)), row_fun)
  }
  
  number_of_columns <- length( rows[[1]] )
  if ( is.null(col_names) ) col_names <- rep("", number_of_columns)
  
  if ( length(col_names) ==  number_of_columns ) {
    col_names <- lapply(col_names, function(x) TableCell(paste(x)))
  } else {
    msg <- sprintf(
      "argument 'col_names' has length %i but the Table has %i columns.", 
      length(col_names), number_of_columns)
    stop(msg, 
         "The number of columns have to match the number of 'col_names'."
    )
  }
  
  if ( is.null(aligns) ) {
    aligns <- rep("d", number_of_columns)
  } else {
    if ( length(aligns) != number_of_columns ) {
      msg <- sprintf(
        "argument 'aligns' has length %i but the Table has %i columns.", 
        length(aligns), number_of_columns)
      stop(msg, 
           "The number of columns have to match the number of 'aligns'."
      )
    }
  }
  if ( is.null(col_width) ) {
    col_width <- integer(number_of_columns)
  } else {
    if ( length(col_width) != number_of_columns ) {
      msg <- sprintf(
        "argument 'col_width' has length %s but the Table has %i columns.", 
        length(col_width), number_of_columns)
      stop(msg, 
           "The number of columns have to match the number of 'col_width'."
      )
    }
  }
  
  alignments <- c(
    l = "AlignLeft", 
    r = "AlignRight", 
    c = "AlignCenter", 
    d = "AlignDefault"
  )
  if ( !all(aligns %in% names(alignments)) ) {
    stop("wrong alignment, possible values are 'l', 'r', 'c' or 'd'")
  }
  aligns <- unname(lapply(alignments[aligns], 
                          FUN=function(x) list(t=unname(x), c=list())))
  if ( is.character(caption) ) {
    caption <- Str(caption)
  }
  Block(
    t = "Table", 
    c = list(
      as.loio(caption), 
      aligns, 
      as.list(col_width),
      col_names, 
      rows
    )
  )
}


#' Div is a Generic Block Container with Attributes.
#' 
#' Constructs a block object of type `Div`.
#' 
#' @param blocks a block object or list of block objects
#' @param attr an object of type [Attr]
#' @examples
#' blocks <- Plain("Hello R!")
#' Div(blocks)
#' 
#' @family Block element constructors
#' @export
## Attr String
Div <- function(blocks, attr=Attr()) {
  Block(t = "Div", c = list(attr, as.lobo(blocks)))
}


#' Nothing.
#' 
#' Constructs a block object of type `Null`.
#' @examples
#' Null()
#' 
#' @family Block element constructors
#' @export
Null <- function() Block(t = "Null", c = list() )

