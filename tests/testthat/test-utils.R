#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `utils.R`')
#line 13 "C:/rdtf/extendedRef/R/utils.R"
test_that('s', {#@testing
    val <- s(FALSE, msg="hello there")
    expect_identical(attributes(val), list(msg='hello there'))

    name = 'frosty'
    val <- s(FALSE, name)
    expect_identical(attributes(val), list(name=name))

    val <- s(FALSE, msg="hello there", name)
    expect_identical(attributes(val), list(msg='hello there', name=name))
})
#line 35 "C:/rdtf/extendedRef/R/utils.R"
test_that('ngettextf', {#@testing
    who_mad <- function(mad.bears)
        ngettextf(length(mad.bears), "%2$s the bear was mad."
                      , "The %1$d bears, %2$s, were mad."
                      , length(mad.bears), comma_list(mad.bears))

    expect_identical(who_mad('Baloo'), "Baloo the bear was mad.")
    expect_identical(who_mad(c('Papa Bear', 'Mama Bear', 'Baby Bear'))
                    , "The 3 bears, Papa Bear, Mama Bear, and Baby Bear, were mad." )
})
#line 79 "C:/rdtf/extendedRef/R/utils.R"
test_that('number_of_arguments', {#@testing
    expect_equal(number_of_arguments(rnorm), 3L)
    expect_equal(number_of_arguments(paste), Inf)
    expect_equal(number_of_arguments(xtfrm), 1)
})
