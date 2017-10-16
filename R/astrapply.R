#' Apply a function on a AST.
#' 
#' Apply the function `FUN` on the abstract syntax tree (AST) obtained from pandoc.
#' 
#' @param x a list representing the AST obtained from pandoc.
#' @param FUN the function to be applied to the AST.
#' @param ... optional arguments to `FUN`.
#' 
#' @return A list containing the modified AST.
#' 
#' @export
astrapply <- function(x, FUN, ...) {
  if ( is.list(x) ) {
    if ( is.null(names(x)) ) {
      obj <- list()
      for (item in x) {
        if ( is.list(item) & ("t" %in% names(item)) ) {
          res <- FUN(item[['t']], item[['c']], ...)
          if ( is.null(res) ) {
            obj[[length(obj) + 1]] <- astrapply(item, FUN, ...)
          } else if ( is.list(res) & is.null(names(res)) ) {
            for (z in res) {
              obj[[length(obj) + 1]] <- astrapply(z, FUN, ...)
            }
          } else {
            obj[[length(obj) + 1]] <- astrapply(res, FUN, ...)
          }
        } else {
          obj[[length(obj) + 1]] <- astrapply(item, FUN, ...)
        }
      }
      return( obj )
    } else {
      obj <- nlist()
      for (k in names(x)) {
        obj[[k]] <- astrapply(x[[k]], FUN, ...)
      }
      return( obj )
    }
  }
  return( x )
}
