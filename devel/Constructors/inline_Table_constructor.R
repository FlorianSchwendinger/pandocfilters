
## rows can be of type matrix, data.frame list of inlines

q("no")
R

library(pandocfilters)
##attachNamespace("pandocfilters")

folder <- "/home/florian/SVN/baRpkgs/trunk/pandocfilters/R"
lapply(dir(folder), function(x) source(file.path(folder, x)))

Table <- function(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list() ) {

    if ( is.null(col_names) ) {
        if ( is.null( colnames(rows) ) ) {
            col_names <- rep("", number_of_columns)
        } else {
            col_names <- colnames(rows)
        }
    }
    
    if ( is.matrix(rows) | is.data.frame(rows) | is.table(rows) ) {

        if ( is.table(rows) ) {
            rows <- as.matrix(rows)
            if ( min(dim(rows)) == 1 ) {
                rows <- t(rows)
            }
        }

        col_fun <- function(m, n) TableCell(as.character(rows[[m, n]]))
        row_fun <- function(m) lapply(seq_len(ncol(rows)), function(n) col_fun(m, n))
        rows <- lapply(seq_len(nrow(rows)), row_fun)
    }

    number_of_columns <- length( rows[[1]] )

    if ( length(col_names) ==  number_of_columns ) {
        col_names <- lapply(col_names, function(x) TableCell(paste(x)))
    } else {
        msg <- sprintf("argument 'col_names' has length %i but the Table has %i columns.", 
                       length(col_names), number_of_columns)
        stop(msg, "The number of columns have to match the number of 'col_names'.")
    }
    
    if ( is.null(aligns) ) {
        aligns <- rep("d", number_of_columns)
    } else {
        if ( length(aligns) != number_of_columns ) {
            msg <- sprintf("argument 'aligns' has length %i but the Table has %i columns.", 
                           length(aligns), number_of_columns)
            stop(msg, "The number of columns have to match the number of 'algins'.")
        }
    }
    if ( is.null(col_width) ) {
        col_width <- integer(number_of_columns)
    } else {
        if ( length(col_width) != number_of_columns ) {
            msg <- sprintf("argument 'col_width' has length %s but the Table has %i columns.", 
                           length(col_width), number_of_columns)
            stop(msg, "The number of columns have to match the number of 'col_width'.")
        }
    }

    alignments <- setNames(c("AlignLeft", "AlignRight", "AlignCenter", "AlignDefault"), 
                           c("l", "r", "c", "d") )
    if ( !all(aligns %in% names(alignments)) ) {
        stop("wrong alignment, possible values are 'l', 'r', 'c' or 'd'")
    }
    aligns <- unname(lapply(alignments[aligns], FUN=function(x) list(t=unname(x), c=list())))
    if ( is.character(caption) ) {
        caption <- Str(caption)
    }
    structure(list(t="Table", c=list(as.loio(caption), aligns, 
                   as.list(col_width), col_names, rows)), class=c("block", "list"))
}


writer <- function(x, con, format) {
    cmd <- sprintf("echo %s | pandoc -f json -t %s", shQuote(as.character(x)), format)
    x <- system(cmd, intern=TRUE)
    cat(x, sep="\n")
}

test <- function(rows, col_names) {
    doc <- document()
    doc$append(Table(rows, col_names))
    doc$write(NULL, writer=writer)    
}

## Test 1
rows <- matrix(1:9, 3)
col_names <- NULL
test(rows, col_names)
rows

## Test 2
rows <- matrix(1:9, 3)
col_names <- LETTERS[1:3]
test(rows, col_names)

## Test 3
rows <- as.data.frame(matrix(1:9, 3))
col_names <- LETTERS[1:3]
test(rows, col_names)

rows <- as.data.frame(matrix(1:9, 3))
colnames(rows) <- LETTERS[4:6]
col_names <- NULL
test(rows, col_names)

## Test 3
rows <- as.data.frame(matrix(1:9, 3))
colnames(rows) <- LETTERS[4:6]
col_names <- NULL
test(rows, col_names)

## Test 3
rows <- as.data.frame(matrix(1:9, 3))
col_names <- NULL
test(rows, col_names)

## Test 4
rows <- matrix(1:9, 3)
col_names <- LETTERS[1:3]
test(rows, col_names)

## Test 5
rows <- table(c(2, 2, 3, 3, 3, 4, 4, 4, 4))
col_names <- LETTERS[1:3]
test(rows, col_names)

## Test 6
rows <- table(c(2, 2, 3, 3, 3, 4, 4, 4, 4))
col_names <- NULL
test(rows, col_names)

## Test 7
## should throw error
rows <- table(c(2, 2, 3, 3, 3, 4, 4, 4, 4, 5))
col_names <- LETTERS[1:3]
test(rows, col_names)

