## a constructor for named lists
nlist <- function(...) {
  x <- list(...)
  names(x) <- character(0)
  x
}

##
##   Not Exported Function for Testing
##
sys_call <- function(cmd, args) {
  tryCatch(system2(cmd, args, stdout=TRUE, stderr=TRUE), error=function(e) NULL)
}


#' Utility functions for testing filters.
#' 
#' @param file file name
#' @param from markdown, html, latex or native
#' @param json json formatted text
#' @param to markdown, html, latex or native
#' 
#' @rdname utility_functions
#' 
#' @export
pandoc_to_json <- function(file, from="markdown") {
  args <- sprintf("-f %s -t json %s", from, file)
  system2("pandoc", args, stdout=TRUE, stderr=TRUE)
}

#' @rdname utility_functions
#' @export
pandoc_from_json <- function(json, to="markdown") {
  tf <- tempfile(fileext = ".txt")
  writeLines(json, tf, useBytes = TRUE)
  on.exit(unlink(tf))
  args <- sprintf("%s --from=json --to=%s", tf, to)
  system2("pandoc", args, stdout=TRUE, stderr=TRUE)
}


#' Write the JSON-formatted AST to a connection.
#' 
#' Write the JSON-formatted AST to a connection.
#' 
#' @param x a JSON representation of the AST to be written out
#' @param con a connection object or a character string to which the JSON-formatted AST is written
#' @param format a character string giving the format (e.g. `"latex"`, `"html"`)
#' @details If you want to apply a filter to the document before it get's written out, or your pandoc installation is not registered in the `PATH` it can be favorable to provide your own writer function to the document class.
#' @export
pandocfilters_writer <- function(x, con, format) {
  args <- sprintf("%s -f json -t %s", shQuote(as.character(x)), format)
  x <- system2("pandoc", args, stdout=TRUE, stderr=TRUE)
  writeLines(x, con=con)
}


to_pandoc_json <- function(x, meta = nlist()){
  z <- if(get_pandoc_version() < "1.18"){
    list(list(unMeta = meta), x)
  } else {
    list(
      blocks = x,
      `pandoc-api-version` = c(1, 17, 0),
      meta = meta
    )
  }
  jsonlite::toJSON(z, auto_unbox = TRUE)
}


collapse_newline <- function(...)paste(..., sep = "\n", collapse = "\n")

test <- function(x, to="html") {
  d <- to_pandoc_json(x)
  z <- pandoc_from_json(d, to=to)
  collapse_newline(z)
}


pandoc_test <- function(x, to = "html") test(x, to)

detect_pandoc_version <- function() {
  x <- sys_call("pandoc", "--version")
  if ( is.null(x) ) {
    packageStartupMessage(
      "Info message:"
    )
    packageStartupMessage(
      "Couldn't find 'pandoc'!  Pandoc version is set to '1.16'"
    )
    packageStartupMessage(
      "Use `set_pandoc_version()` to change the settings if necessary."
    )
    numeric_version("1.6")
  }
  version <- strsplit(x[[1]], " ")[[1]][[2]]
  numeric_version(version)
}
## detect_pandoc_version()


#' Get Pandoc Version
#' 
#' Get the version of pandoc.
#' 
#' @export
get_pandoc_version <- function() {
  base::getNamespace("pandocfilters")$pandoc$version
}


#' Set Pandoc Version.
#' 
#' Set the version version pandoc.
#' 
#' @param x a numeric giving the pandoc version (e.g. 1.14 or 1.15 or 1.16 or 1.17)
#' @export
set_pandoc_version <- function(x) {    
  assign("version", x, envir=base::getNamespace("pandocfilters")$pandoc)
}
