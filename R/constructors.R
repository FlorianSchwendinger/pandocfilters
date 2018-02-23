##
##   Additional Constructors (only used as function arguments)
##


#' Inline Objects.
#' 
#' Objects of the classes `NULL` and `character` can be coerced to `inline`.
#' 
#' @param x an object of type `NULL`, `character` or `inline`.
#' @return an object of class `inline`.
#' @examples
#' as.inline("some text")
#' as.inline(NULL)
#' @export
as.inline <- function( x ) {
  UseMethod( "as.inline" )
}

##' @noRd
##' @export
as.inline.inline <- identity

##' @noRd
##' @export
as.inline.character <- function( x ) {
  Str(paste(x, collapse=" "))
}

##' @noRd
##' @export
as.inline.NULL <- function( x ) structure(list(), class=c("inline", "list"))


#' Inline Objects
#' 
#' Tests if an object has the class attribute `inline`.
#' 
#' @param x an object to be tested.
#' @return a logical indicating if the provided object is of type `inline`.
#' @examples
#' is.inline(as.inline(NULL))
#' @export
is.inline <- function(x) inherits(x, "inline")

combine_two <- function(x, y) {
  if ( is.null(x) ) return(y)
  if ( is.null(y) ) return(x)
  if ( is.inline(x) & is.inline(y)) {
    return(list(x, y))
  }
  if ( is.loio(x) & is.inline(y) ) {
    return(c(x, list(y)))
  }
  if ( is.inline(x) & is.loio(y) ) {
    return(c(list(x), y))
  }
  return( c(as.loio(x), as.loio(y)) )
}

#' Combine Inline Objects
#' 
#' Objects of class `inline` can be combined by using the generic default method [c] (combine).
#' @param ... objects to be concatenated.
#' @return an list of `inline` objects.
#' @examples
#' c(Str("some"), Strong("text"))
#' @export
c.inline <- function(...) {
  x <- lapply(list(...), as.inline)
  rval <- Reduce(combine_two, x)
  if ( length(rval) == 1 ) return(structure(rval, class=c("inline", "list")))
  return(structure(rval, class=c("loio", "list")))
}



# loio (List of Inline Objects) ---------------------------------------

as.loio <- function( x ) UseMethod( "as.loio" )

##' @noRd
##' @export
as.loio.loio <- identity

##' @noRd
##' @export
as.loio.NULL <- function( x ) structure(list(), class=c("loio", "list"))

##' @noRd
##' @export
as.loio.inline <- function( x ) structure(list(x), class=c("loio", "list"))

##' @noRd
##' @export
as.loio.character <- function( x ) structure(list(as.inline(x)), class=c("loio", "list"))

##' @noRd
##' @export
as.loio.list <- function( x ) {
  x <- lapply(x, as.inline)
  structure(
    x, 
    class=c("loio", "list")
  )
}

is.loio <- function(x) class(x)[1] == "loio"



#' Block Objects
#' 
#' In pandoc `block` objects are used as container for `inline` objects and to give them specific roles. Objects of the classes `NULL` and `character` can be coerced to `block`.
#' 
#' @param x an object of type `NULL` or `character` or `block`.
#' @return an object of class `block`.
#' @examples
#' as.block("some text")
#' as.block(NULL)
#' @export
as.block <- function( x ) {
  UseMethod( "as.block" )
}


##' @noRd
##' @export
as.block.NULL <- function(x) structure(list(), class=c("block", "list"))

##' @noRd
##' @export
as.block.character <- function(x) Plain(x)

#' Block Objects
#' 
#' Tests if an object has the class attribute `block`.
#' 
#' @param x an object to be tested.
#' @return a logical indicating if the provided object is of type `block`.
#' @examples
#' is.block(as.block(NULL))
#' @export
is.block <- function(x) class(x)[1] == "block"

combine_two_blocks <- function(x, y) {
  if ( is.null(x) ) return(y)
  if ( is.null(y) ) return(x)
  if ( is.block(x) & is.block(y)) {
    return(list(x, y))
  }
  if ( is.lobo(x) & is.block(y) ) {
    return(c(x, list(y)))
  }
  if ( is.block(x) & is.lobo(y) ) {
    return(c(list(x), y))
  }
  return( c(as.lobo(x), as.lobo(y)) )
}

#' Combine Block Objects
#' 
#' Objects of class `block` can be combined by using the generic default method `c` (combine).
#' @param ... objects to be concatenated.
#' @return an list of `block` objects.
#' @examples
#' c(Header( "R Basics" ), Header("What is R?", level=2),
#' Plain(c(Emph("R"), Space(), "is a system for ", Strong("statistical computation"))))
#' @export
c.block <- function(...) {
  x <- list(...)
  rval <- Reduce(combine_two_blocks, x)
  if ( length(rval) == 1 ) return(structure(rval, class=c("block", "list")))
  return( structure(rval, class=c("lobo", "list")) )
}


# lobo (List of Block Objects) --------------------------------------------

as.lobo <- function( x ) UseMethod( "as.lobo" )

##' @noRd
##' @export
as.lobo.lobo <- identity

##' @noRd
##' @export
as.lobo.NULL <- function( x ) structure(list(), class=c("lobo", "list"))

##' @noRd
##' @export
as.lobo.block <- function( x ) structure(list(x), class=c("lobo", "list"))

##' @noRd
##' @export
as.lobo.list <- function( x ) {
  b <- sapply(x, is.block)
  if ( !all(b) ) {
    stop(
      sprintf("TypeError: elements %s are not of type block ", 
              paste(which(!b), collapse=", ")), 
      "All elements must be of type block!")
  }
  class(x) <- c("lobo", class(x))
  x
}

is.lobo <- function(x) class(x)[1] == "lobo"


## lolobo (List of List of Block Objects)
as.lolobo <- function( x ) UseMethod( "as.lolobo" )

##' @noRd
##' @export
as.lolobo.lolobo <- identity

##' @noRd
##' @export
as.lolobo.NULL  <- function( x ) 
  structure(
    list(as.lobo(x)), 
    class=c("lolobo", "lobo", "list")
  )

##' @noRd
##' @export
as.lolobo.block <- function( x ) 
  structure(
    list(as.lobo(x)), 
    class=c("lolobo", "lobo", "list")
  )

##' @noRd
##' @export
as.lolobo.lobo  <- function( x ) 
  structure(
    list(x), 
    class=c("lolobo", "lobo", "list")
  )

##' @noRd
##' @export
as.lolobo.list  <- function( x ) {
  structure(
    lapply(x, as.lobo), 
    class=c("lolobo", "lobo", "list")
  )
}


## pandoc Types
##
## A constructor for pandoc types.
##
## param x a character string giving the type
## details A convenience function to create the following data structure
##          `list(t=x, c=list())` by only providing x.
## examples
## Type("SmallCaps")
Type <- function(x) {
  z <- list(t = x, c = list())
  class(z) <- c("Type", "list")
  z
}

