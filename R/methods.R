##' @noRd
##' @export
print.inline <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.loio <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.block <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.TableCell <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.ListAttributes <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.Attr <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.Type <- function(x, ...) print(unclass(x))

##' @noRd
##' @export
print.Citation <- function(x, ...) print(unclass(x))
