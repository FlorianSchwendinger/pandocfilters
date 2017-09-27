#!/usr/bin/env Rscript

require(pandocfilters, warn.conflicts = FALSE, quietly = TRUE)

caps <- function(key, value, ...) {
    if (key == "Str") return( Str( toupper(value)) )
    return(NULL)
}

conin <- file("stdin", open="r")
pandocfilters::filter(caps, conin=conin)
close(rcon)
