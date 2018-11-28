#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `MethodsLibrary.R`')
#line 94 "R/MethodsLibrary.R"
test_that('initialize,MethodsLibrary-method', {#@testing
    expect_is(lib <- new('MethodsLibrary'), 'MethodsLibrary')
    testextra::expect_valid(lib)

    parent <- s(new.env(), name = 'test methods parent')

    lib <- new('MethodsLibrary'
              , list(say_hi = function()cat('hi\n'))
              , method.parent = parent
              , .lock=FALSE
              )
    expect_length(ls(lib, all=TRUE), 1L)
    expect_identical(environment(lib$say_hi), parent)
    testextra::expect_valid(lib)

    expect_false(environmentIsLocked(lib))
    assign('.self', lib, envir=lib@.xData)
    testextra::expect_valid(lib)

    assign('say_goodby', 'goodby', envir=lib@.xData)
    expect_error( validObject(lib), "Element say_goodby is not a valid refMethodDef object")

    rm(say_goodby, envir = lib)
    copy <- new('MethodsLibrary'
               , methods = lib
               , method.parent = emptyenv()
               , parent = globalenv()
               )
    expect_identical(parent.env(copy), globalenv())
    expect_identical(environment(copy$say_hi), emptyenv())
    expect_false(exists('.self', copy))
    expect_true(environmentIsLocked(copy))
})
