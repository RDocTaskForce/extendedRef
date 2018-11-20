#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `MethodsLibrary.R`')
#line 90 "R/MethodsLibrary.R"
test_that('initialize,MethodsLibrary-method', {#@testing
    expect_is(lib <- new('MethodsLibrary'), 'MethodsLibrary')

    parent <- s(new.env(), name = 'test methods parent')

    lib <- new('MethodsLibrary'
              , list(say_hi = function()cat('hi\n'))
              , method.parent = parent
              )
    expect_length(ls(lib, all=TRUE), 1L)
    expect_identical(environment(lib$say_hi), parent)
})
