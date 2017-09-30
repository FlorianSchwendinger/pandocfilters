#' Filter JSON-formatted AST.
#' 
#' Apply a filter on the JSON-formatted abstract syntax tree (AST).
#' 
#' @param FUN the function to be applied on the AST.
#' @param ... optional arguments to \code{FUN}.
#' @param input a connection object or a character string from which the JSON-formatted AST is read
#' @param output a connection object or a character string to which the JSON-formatted AST is written
#' 
#' @export
filter <- function(FUN, ..., input = stdin(), output = stdout()) {
  ## read ast (in json format)
  
  json <- character(0)
  while(length(line <- readLines(input, n=1)) > 0) {
    json <- c(json, as.character(line))
  }
  
  if ( length(json) == 1 ) {
    if ( nchar(json) == 0 ){
      stop("InputError: The JSON-formatted AST for read in is empty!")
    }
  }
  
  ## convert json to native r
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE, flatten=TRUE)
  
  ## modify the ast tree
  x <- astrapply(x, FUN, ...)
  
  out <- as.character( jsonlite::toJSON(x, auto_unbox=TRUE) )
  writeLines(out, con=output)
}
