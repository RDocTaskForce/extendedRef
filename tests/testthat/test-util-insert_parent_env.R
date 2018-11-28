#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `util-insert_parent_env.R`')
#line 9 "R/util-insert_parent_env.R"
test_that('insert_parent_env', {#@testing
    a <- new.env(parent = baseenv())
    b <- new.env(parent = emptyenv())

    expect_identical(parent.env(a), baseenv())
    expect_identical(parent.env(b), emptyenv())

    insert_parent_env(a, b)

    expect_identical(parent.env(a), b)
    expect_identical(parent.env(b), baseenv())
})
