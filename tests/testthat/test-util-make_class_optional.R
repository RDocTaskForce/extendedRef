#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `util-make_class_optional.R`')
#line 9 "C:/rdtf/extendedRef/R/util-make_class_optional.R"
test_that('make_class_optional', {#@testing
    if (exists(classMetaName('test'), where=globalenv()))
        removeClass('test', where=globalenv())
    new_test <- setClass('test', representation = list(a='logical'), where=globalenv())
    make_class_optional('test', 'optional-test', where=globalenv())

    expect_true(exists(classMetaName('optional-test'), where=globalenv()))

    expect_true(is(NULL, 'optional-test'))

    expect_is(x <- new_test(a=TRUE), 'test')
    expect_true(is(x, 'optional-test'))

    expect_true(removeClass('optional-test', where=globalenv()))
    expect_true(removeClass('test', where=globalenv()))
})
