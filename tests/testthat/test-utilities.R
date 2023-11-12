test_that("is_ontology() works", {
    expect_true(is_ontology("cl"))
    expect_true(is_ontology("CL"))           # not case sensitive
    expect_false(is_ontology("not_an_ontology"))
    expect_false(is_ontology(character()))
    expect_false(is_ontology(""))
    expect_false(is_ontology(NA_character_))
    expect_false(is_ontology(c("cl", "CL"))) # scalar string only
})

test_that("is_varying_column() works", {
    expect_true(is_varying_column(c("A", "B")))
    expect_true(is_varying_column(c("A", NA)))

    expect_false(is_varying_column(c("A", "A")))
    expect_false(is_varying_column(rep(NA_character_, 2)))

    ## 0- and 1-length columns 'vary'; see implementation note
    expect_true(is_varying_column(character()))
    expect_true(is_varying_column("A"))
})

test_that("as_description_string() works", {
    expect_identical(as_description_string(list(c("Foo", "bar"))), "Foo, bar")
    expect_true(is.na(as_description_string(list(""))))

    expect_identical(
        as_description_string(list(c("Foo", "bar"), "")),
        c("Foo, bar", NA)
    )
})
