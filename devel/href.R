library(pandocfilters)

pandoc_to_json <- function(file, from) {
    cmd <- sprintf("pandoc -f %s -t json %s", from, file)
    system(cmd, intern=TRUE)
}

pandoc_from_json <- function(json, to) {
    cmd <- sprintf("echo %s | pandoc -f json -t %s", shQuote(json), to)
    system(cmd, intern=TRUE)
}

href <- function(key, value, ...) {
    if (key == "RawInline") {
        if ( any(grepl("^\\\\href", value)) ) {
            x <- unlist(regmatches(value, gregexpr("\\{.*?\\}", value)))
            x <- sapply(x, function(s) substr(s, 2, nchar(s)-1))
            return( Link("", Str(x[[2]]), x[[1]]) )
        }       
    }
    return(NULL)
}

x[[1]]

str(Link(Str(x[[2]]), x[[1]]))

json
json <- pandoc_to_json("href.md", from="markdown")
value <- "\\href{https://cran.r-project.org/}{R}"

value
x <- jsonlite::fromJSON(json, simplifyVector = FALSE, flatten=TRUE)
x
x[[2]][[2]]$c[[7]]
x[[2]][[2]]$c[[7]][[2]]
link <- x[[2]][[3]]$c[[1]]
str(link)
link[[2]][[1]]
link[[2]][[2]]

json

str(Link("alt_text", list(Str("some_link")), list("https:://", "")))

Link("a", "b", "v")
Link <- function(text, url, alternative) {
    list(t="Link", c=list(list(Str(text)), list(url, alternative)))
}
str(LINK("some_link", "https://", ""))


## read connection
conin <- textConnection(pandoc_to_json("href.md", from="markdown"))
## write connection
stdout <- textConnection("modified_ast", open="w")
filter(href, conin=conin, conout=stdout)

cat(pandoc_from_json(modified_ast, "markdown"), sep="\n")

close(conin)
close(stdout)

