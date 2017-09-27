library(pandocfilters)

pandoc_to_json <- function(file, from) {
    cmd <- sprintf("pandoc -f %s -t json %s", from, file)
    system(cmd, intern=TRUE)
}

pandoc_from_json <- function(json, to) {
    cmd <- sprintf("echo %s | pandoc -f json -t %s", shQuote(json), to)
    system(cmd, intern=TRUE)
}

caps <- function(key, value, ...) {
    if (key == "Str") return( Str( toupper(value)) )
    return(NULL)
}

## read connection
conin <- textConnection(pandoc_to_json("caps.md", from="markdown"))
## write connection
stdout <- textConnection("modified_ast", open="w")
filter(caps, conin=conin, conout=stdout, meta=list(unmeta=list("Flo")))

cat(pandoc_from_json(modified_ast, "markdown"), sep="\n")

close(conin)
close(stdout)

