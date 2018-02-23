#' ListAttributes.
#' 
#' A constructor for pandoc list attributes.
#' 
#' @param first_number an integer giving the first number of the list
#' @param style a character string giving the style, possible values are `DefaultStyle`, `Example`, `Decimal`, `LowerRoman`, `UpperRoman`, `LowerAlpha` and `UpperAlpha`.
#' @param delim a character string giving the delimiter, possible values are `DefaultDelim`, `Period`, `OneParen` and `TwoParens`.
#' 
#' @export
## ListAttributes = (Int, ListNumberStyle, ListNumberDelim) 
ListAttributes <- function(first_number = 1L, style = "DefaultStyle", 
                           delim = "DefaultDelim") {
  z <- list(
    first_number, 
    list(t=style, c=list()), 
    list(t=delim, c=list())
  )
  class(z) <- c("ListAttributes", "list")
  z
}

