#' Attributes.
#' 
#' A constructor for pandoc attributes.
#' 
#' @param identifier a character string
#' @param classes a character giving the classes
#' @param key_val_pairs a list of tuple of type `character`
#' @examples
#' Attr("A", c("B", "C"), list(c("D", "E")))
#' 
#' @family Argument constructors
#' @export
## type Attr = (String, [String], [(String, String)]) 
Attr <- function(identifier="", classes=character(), key_val_pairs=list()) {
  if ( !is.character(classes) ) stop("'classes' has to be of type character!")
  z <- list(identifier, as.list(classes), key_val_pairs)
  class(z) <- c("Attr", "list")
  z
}


#' Citation.
#' 
#' A constructor of an object of type `"Citation"`.
#' 
#' @param suffix a inline object or list of inline objects
#' @param id a character string (not visible in the text)
#' @param note_num an integer 
#' @param mode a character string giving the citation mode, possible values are 
#'             `"AuthorInText"`, `"SuppressAuthor"` and `"NormalCitation"`.
#' @param prefix a inline object or list of inline objects
#' @param hash an integer
#' 
#' @family Argument constructors
#' @export
Citation <- function(suffix, id, note_num=0L, mode="AuthorInText", prefix=list(), hash=0L) {
  suffix <- as.loio(suffix)
  prefix <- as.loio(prefix)
  x <- list(citationSuffix = suffix,
            citationNoteNum = note_num,
            citationMode = list(t=mode, c=list()),
            citationPrefix = list(),
            citationId = id,
            citationHash = hash)
  structure(x, 
            class=c("Citation", "list"))
}

is.citation <- function(x) class(x)[1] == "Citation"



#' Table Cell.
#' 
#' Table cells is a constructor for plain table cells.
#' 
#' @param x a character string giving the content of the table cell
#' @details In general table cells are a list of block elements, the 
#'          constructor `TableCell` creates a plain table cell.
#' @examples
#' TableCell("Cell 1")
#' 
#' @family Argument constructors
#' @export
TableCell <- function(x) 
  structure(
    list(
      Plain(list(Str(x)))
    ), 
    class=c("TableCell", "list")
  )


