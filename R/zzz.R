.onLoad <- function( libname, pkgname ) {
    version <- detect_pandoc_version()
    set_pandoc_version(version)
}