library(jsonlite)
library(pandocfilters)

fjson <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE, flatten=TRUE)

pandoc_to_json <- function(file, from="markdown") {
    cmd <- sprintf("pandoc -f %s -t json %s", from, file)
    system(cmd, intern=TRUE)
}

pandoc_from_json <- function(json, to) {
    cmd <- sprintf("echo %s | pandoc -f json -t %s", shQuote(json), to)
    system(cmd, intern=TRUE)
}

##' new_doc creates a new document x has to be a list of pandoc objects
new_block <- function(x, type="Plain") {
    list(list(unMeta=pandocfilters:::nlist()),
         list(list(t=type, c=x)))
}

##' test tests a constructor by putting it into a document and tranforming it!
test <- function(x, to="html", type="Plain") {
    pandoc_from_json(as.character( jsonlite::toJSON(new_block(x, type), auto_unbox=TRUE) ), to=to)
}


##' test tests a constructor by putting it into a document and tranforming it!
test2 <- function(x, to="html") {
    data <- list(list(unMeta=pandocfilters:::nlist()), x)
    pandoc_from_json(as.character( jsonlite::toJSON(data, auto_unbox=TRUE) ), to=to)
}


## test(list(Math("x^2")))


## ----------------------------
## Table
## ----------------------------
j <- pandoc_to_json("Block/Table.tex", "latex")
j
str(fjson(j))
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$c)
str(fjson(j)[[2]][[1]]$c[1:4])
str(fjson(j)[[2]][[1]]$c[[5]][[1]])
str(fjson(j)[[2]][[1]]$c[[5]][[2]])

j <- pandoc_to_json("Block/Table.md", "markdown")
j
str(fjson(j))
str(fjson(j)[[1]])
fjson(j)[[2]][[1]]$c[[1]]
str(fjson(j)[[2]][[1]]$t)
str(fjson(j)[[2]][[1]]$c)
str(fjson(j)[[2]][[1]]$c[[1]])
str(fjson(j)[[2]][[1]]$c[[2]])
str(fjson(j)[[2]][[1]]$c[[3]])
str(fjson(j)[[2]][[1]]$c[[4]])
str(fjson(j)[[2]][[1]]$c[1:4])
str(fjson(j)[[2]][[1]]$c[[5]][[1]])
str(fjson(j)[[2]][[1]]$c[[5]][[2]])

#' @param rows a list of lists of T objects of type TableCell
#' @param col_names a list of objects of type TableCell
#' @param aligns a character vector of alginments, possible values are \dQuote{l} for left,
#'               \dQuote{r} for right, \dQuote{c} for center and \dQuote{d} for default.
#' @param col_width a numeric vector
#' @param caption a list of in line objects giving the caption
#' Table, with caption, column alignments (required), relative column widths 
#' (0 = default), column headers (each a list of blocks), 
#' and rows (each a list of lists of blocks)
##
## 
## Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
Table <- function(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list() ) {
    
    if ( is.matrix(rows) ) {
        col_fun <- function(m, n) TableCell(as.character(rows[[m, n]]))
        row_fun <- function(m) lapply(seq_len(ncol(rows)), function(n) col_fun(m, n))
        rows <- lapply(seq_len(nrow(rows)), row_fun)
    }

    number_of_columns <- length( rows[[1]] )
    if ( is.null(col_names) ) {
        col_names <- rep(list(TableCell("")), number_of_columns)
    } else if ( is.character(col_names) ) {
        col_names <- lapply(col_names, function(x) TableCell(x))
    }
    if ( is.null(aligns) ) {
        aligns <- rep("d", number_of_columns)
    }
    if ( is.null(col_width) ) {
        col_width <- integer(number_of_columns)
    }

    alignments <- setNames(c("AlignLeft", "AlignRight", "AlignCenter", "AlignDefault"), c("l", "r", "c", "d"))
    if ( !all(aligns %in% names(alignments)) ) stop("wrong alignment, possible values are 'l', 'r', 'c' or 'd'")
    aligns <- unname(lapply(alignments[aligns], FUN=function(x) list(t=unname(x), c=list())))
    list(t="Table", c=list(caption, aligns, as.list(col_width), col_names, rows))
}



TableCell <- function(x) list(Plain(list(Str(x))))

x <- Table(matrix(1:6, nrow=3))
cat(test2(list(x), to="markdown"), sep="\n")
x <- Table(matrix(1:6, nrow=3), col_names=c("COL_A", "COL_B"))
cat(test2(list(x), to="markdown"), sep="\n")
cat(test2(list(x), to="latex"), sep="\n")

names(x)
x$t
str(x$c[[1]])
str(x$c[[2]])
str(x$c[[3]])
str(x$c[[4]])
str(x)
cat(test2(list(x), to="markdown"), sep="\n")

jsonlite::toJSON(as.TableCell("abc"))


tab <- list(list(TableCell("1"), TableCell("2")), 
            list(TableCell("3"), TableCell("4")),
            list(TableCell("5"), TableCell("6")))
cnames <- list(TableCell("H1"), TableCell("H2"))
ta <-  Table(tab, cnames)
str(ta)
str(ta$t)
cat(test2(list(ta), to="markdown"), sep="\n")
test2(list(ta), to="latex")
test(list(fjson(j)[[2]][[1]]), to="markdown")
test(list(), to="markdown")
str(fjson(j))

List of 5
 $ : list()
 $ :List of 2
  ..$ :List of 2
  .. ..$ t: chr "AlignRight"
  .. ..$ c: list()
  ..$ :List of 2
  .. ..$ t: chr "AlignLeft"
  .. ..$ c: list()
 $ :List of 2
  ..$ : int 0
  ..$ : int 0
 $ :List of 2
  ..$ :List of 1
  .. ..$ :List of 2
  .. .. ..$ t: chr "Plain"
  .. .. ..$ c:List of 1
  .. .. .. ..$ :List of 2
  .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. ..$ c: chr "Right"
  ..$ :List of 1
  .. ..$ :List of 2
  .. .. ..$ t: chr "Plain"
  .. .. ..$ c:List of 1
  .. .. .. ..$ :List of 2
  .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. ..$ c: chr "Left"
 $ :List of 2
  ..$ :List of 2
  .. ..$ :List of 1
  .. .. ..$ :List of 2
  .. .. .. ..$ t: chr "Plain"
  .. .. .. ..$ c:List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. ..$ c: chr "1"
  .. ..$ :List of 1
  .. .. ..$ :List of 2
  .. .. .. ..$ t: chr "Plain"
  .. .. .. ..$ c:List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. ..$ c: chr "2"
  ..$ :List of 2
  .. ..$ :List of 1
  .. .. ..$ :List of 2
  .. .. .. ..$ t: chr "Plain"
  .. .. .. ..$ c:List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. ..$ c: chr "3"
  .. ..$ :List of 1
  .. .. ..$ :List of 2
  .. .. .. ..$ t: chr "Plain"
  .. .. .. ..$ c:List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. ..$ c: chr "4"


str(new_block(list(), "Table"))

List of 2
 $ :List of 1
  ..$ unMeta: Named list()
 $ :List of 1
  ..$ :List of 2
  .. ..$ t: chr "Table"
  .. ..$ c:List of 5
  .. .. ..$ : list()
  .. .. ..$ :List of 2
  .. .. .. ..$ :List of 2
  .. .. .. .. ..$ t: chr "AlignRight"
  .. .. .. .. ..$ c: list()
  .. .. .. ..$ :List of 2
  .. .. .. .. ..$ t: chr "AlignLeft"
  .. .. .. .. ..$ c: list()
  .. .. ..$ :List of 2
  .. .. .. ..$ : int 0
  .. .. .. ..$ : int 0
  .. .. ..$ :List of 2
  .. .. .. ..$ :List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Plain"
  .. .. .. .. .. ..$ c:List of 1
  .. .. .. .. .. .. ..$ :List of 2
  .. .. .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. .. .. ..$ c: chr "Right"
  .. .. .. ..$ :List of 1
  .. .. .. .. ..$ :List of 2
  .. .. .. .. .. ..$ t: chr "Plain"
  .. .. .. .. .. ..$ c:List of 1
  .. .. .. .. .. .. ..$ :List of 2
  .. .. .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. .. .. ..$ c: chr "Left"
  .. .. ..$ :List of 2
  .. .. .. ..$ :List of 2
  .. .. .. .. ..$ :List of 1
  .. .. .. .. .. ..$ :List of 2
  .. .. .. .. .. .. ..$ t: chr "Plain"
  .. .. .. .. .. .. ..$ c:List of 1
  .. .. .. .. .. .. .. ..$ :List of 2
  .. .. .. .. .. .. .. .. ..$ t: chr "Str"
  .. .. .. .. .. .. .. .. ..$ c: chr "1"
  .. .. .. .. ..$ :List of 1
  .. .. .. .. .. ..$ :List of 2
  .. .. .. .. .. .. ..$ t: chr "Plain"
