## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize----------------------------------------------------------
require("pandocfilters", quietly = TRUE, warn.conflicts = FALSE)
# Get the pandoc version.
get_pandoc_version()

## ----pandoc_version, eval=FALSE------------------------------------------
#  # Set the pandoc version.
#  set_pandoc_version(1.16)

## ----emph, eval = FALSE--------------------------------------------------
#  Emph(list(Str("some text")))
#  Emph(Str("some text"))
#  Emph("some text")

## ----utility_functions, eval=FALSE---------------------------------------
#  pandoc_to_json()
#  pandoc_from_json()
#  test_filter()

## ----caps----------------------------------------------------------------
caps <- function(key, value, ...) {
    if (key == "Str") return( Str( tolower(value) ) )
    return(NULL)
}

example <- file.path(
  system.file(package = "pandocfilters"), 
  "examples", 
  "lower_case.md"
)

# the file before transformation
readLines(example)

## ----apply_caps----------------------------------------------------------
# read connection
input_connection <- textConnection(pandoc_to_json(example, from = "markdown"))
# write connection
output_connection <- textConnection("modified_ast", open = "w")

# apply filter
filter(caps, input = input_connection, output = output_connection)

# convert altered ast to markdown
pandoc_from_json(modified_ast, to = "markdown")

## ----close---------------------------------------------------------------
close(input_connection)
close(output_connection)

