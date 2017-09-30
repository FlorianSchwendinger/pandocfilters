context("BulletList")

test_that("BulletList", {

	y <- c("<ul>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ul>")

	bullet_1 <- list(Plain(list(Str("A"))))
	bullet_2 <- list(Plain(list(Str("B"))))
	bullet_3 <- list(Plain(list(Str("C"))))
	block <- BulletList(list(bullet_1, bullet_2, bullet_3))

    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
