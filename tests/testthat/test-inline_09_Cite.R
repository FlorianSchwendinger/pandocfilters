context("Cite")

test_that("Cite", {

    ci <- Citation(suffix=list(Str("Suffix_1")),
                   id="Citation_ID_1", prefix=list(Str("Prefix_1")))

    ## Test Cite with Header
    cite <- Cite(list(ci), list(Str("some text")))
    x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
    expect_that(x, equals(c("@Citation_ID_1 [Suffix\\_1]", "==========================")))

    cite <- Cite(list(ci), Str("some text"))
    x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
    expect_that(x, equals(c("@Citation_ID_1 [Suffix\\_1]", "==========================")))

    cite <- Cite(ci, Str("some text"))
    x <- pandocfilters:::test(list(Header(list(cite))), to="markdown")
    expect_that(x, equals(c("@Citation_ID_1 [Suffix\\_1]", "==========================")))

    x <- pandocfilters:::test(list(Header(cite)), to="markdown")
    expect_that(x, equals(c("@Citation_ID_1 [Suffix\\_1]", "==========================")))

    ## Test Cite with Plain
    cite <- Cite(list(ci), list(Str("some text")))
    x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
    expect_that(x, equals("@Citation_ID_1 [Suffix\\_1]"))

    cite <- Cite(list(ci), Str("some text"))
    x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
    expect_that(x, equals("@Citation_ID_1 [Suffix\\_1]"))

    cite <- Cite(ci, Str("some text"))
    x <- pandocfilters:::test(list(Plain(list(cite))), to="markdown")
    expect_that(x, equals("@Citation_ID_1 [Suffix\\_1]"))

    x <- pandocfilters:::test(list(Plain(cite)), to="markdown")
    expect_that(x, equals("@Citation_ID_1 [Suffix\\_1]"))
    
} )
