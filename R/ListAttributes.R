#' ListAttributes.
#' 
#' A constructor for pandoc list attributes.
#' 
#' @param first_number an integer giving the first number of the list
#' @param style a character string giving the style, possible values are \code{"DefaultStyle"}, \code{"Example"}, \code{"Decimal"}, \code{"LowerRoman"}, \code{"UpperRoman"}, \code{"LowerAlpha"} and \code{"UpperAlpha"}.
#' @param delim a character string giving the delimiter, possible values are \code{"DefaultDelim"}, \code{"Period"}, \code{"OneParen"} and \code{"TwoParens"}.
#' 
#' @export
## ListAttributes = (Int, ListNumberStyle, ListNumberDelim) 
ListAttributes <- function(first_number=1L, style="DefaultStyle", delim="DefaultDelim") {
  structure(list(first_number, list(t=style, c=list()), list(t=delim, c=list())), class=c("ListAttributes", "list"))
}

