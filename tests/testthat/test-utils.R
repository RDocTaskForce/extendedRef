#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `utils.R`')
#line 49 "R/utils.R"
test_that('are', {#@testing
    lst <- list('a', 1L, TRUE)
    
    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
})
#line 136 "R/utils.R"
test_that('number_of_arguments', {#@testing
    expect_equal(number_of_arguments(rnorm), 3L)
    expect_equal(number_of_arguments(paste), Inf)
    expect_equal(number_of_arguments(xtfrm), 1)
})
