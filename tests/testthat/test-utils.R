#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `utils.R`')
#line 56 "R/utils.R"
test_that('number_of_arguments', {#@testing
    expect_equal(number_of_arguments(rnorm), 3L)
    expect_equal(number_of_arguments(paste), Inf)
    expect_equal(number_of_arguments(xtfrm), 1)
})
