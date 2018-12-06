#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `StaticTriad.R`')
#line 48 "R/StaticTriad.R"
test_that('$,StaticTriad-method', {#@testing
    x <- new('StaticTriad')
    x$const   <- const   <- static_const(list(a=1))
    x$vars    <- vars    <- new_static_env(c(int = 'integer'), parent=globalenv())
    x$methods <- methods <- static_methods(list(say_hi = function()cat('hi\n')), parent = globalenv())

    L <- list(x=x)

    expect_identical(x$const  , const)
    expect_identical(x$vars   , vars)
    expect_identical(x$methods, methods)
    expect_identical(L$x$const  , x$const)
    expect_identical(L$x$vars   , x$vars)
    expect_identical(L$x$methods, x$methods)

    expect_identical(x$int, integer(0))
    expect_identical(x$a, 1)
    expect_is(x$say_hi, 'function')

    expect_identical(x$.self, x)

    expect_is(x, 'StaticTriad')
    expect_error(x$"not a valid name")
})
#line 94 "R/StaticTriad.R"
test_that('$<-,StaticTriad-method', {#@testing
    x <- new('StaticTriad')
    x$const <- static_const(list(a=1))
    x$vars  <- new_static_env(c(int = 'integer'))
    x$methods <- static_methods(list(say_hi = function()cat('hi\n')))
    expect_identical(x$a, 1)
    expect_identical(x$int, integer())


    expect_identical(x$int <- 3L, 3L)
    expect_identical(x$int, 3L)
    expect_is(x, 'StaticTriad')

    expect_error(x$say_hi <- 'hi', "is a static method and cannot be changed")
    expect_error(x$a <- 2, "is a static const value and cannot be changed")
    expect_error(x$b <- TRUE, "is not a valid static variable.")
})
