#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `static_methods.R`')
#line 13 "R/static_methods.R"
test_that('setClass("StaticMethod", ...)', {#@testing
    def <- function().count <<- 0L
    def <- function().count <<- 0L
    def <- function().count <<- 0L
    method <- new('StaticMethod', def, refClassName = 'test', name='reset')
    expect_equal(method@name, 'reset')
    expect_equal(method@refClassName, 'test')
    expect_equal(method@refClassName, 'test')
    expect_identical(environment(method), environment(def))
})
#line 49 "R/static_methods.R"
test_that('initialize,StaticMethods-method', {#@testing
    expect_is(bare <- static_methods(), 'StaticMethods')
    expect_length(as.list(bare, all=TRUE), 0L)
    
    expect_is(lib <- static_methods(list(say_hi= function(){cat('hi\n')})), 'StaticMethods')
    expect_length(ls(lib, all=TRUE), 1L)
    
    expect_is(lib$say_hi, 'StaticMethod')
    expect_identical(environment(lib$say_hi), lib@.xData)
})
