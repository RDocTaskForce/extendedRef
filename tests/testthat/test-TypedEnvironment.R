#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `TypedEnvironment.R`')
#line 79 "R/TypedEnvironment.R"
test_that('initialize,TypedEnvironment-method', {#@testing
    expect_is(bare <- new('TypedEnvironment'), 'TypedEnvironment')
    expect_identical(ls(bare, all=TRUE), character())
    expect_null(bare@initializer)
    testextra::expect_valid(bare)

    expect_is( typed <- new('TypedEnvironment', c(int = 'integer', char = 'character', fun = 'function'))
             , 'TypedEnvironment')
    expect_identical(ls(typed, all=TRUE), c('char', 'fun', 'int'))
    expect_identical(typed$int, integer())
    expect_identical(typed$char, character())
    expect_identical(typed$fun, new('function'))
    expect_null(typed@initializer)
    testextra::expect_valid(typed)

    expect_is( named <- new('TypedEnvironment', c(int = 'integer', char = 'character', fun = 'function')
                           , self.name = '.self')
               , 'TypedEnvironment')
    expect_identical(ls(named, all=TRUE), c('.self', 'char', 'fun', 'int'))
    expect_identical(named$.self, named)

    initializer <- function(){
        .self$int <- 101L
        .self$char <- 'y'
        invisible(.self)
    }

    expect_is( winit <- new('TypedEnvironment'
                           , c(int = 'integer', char = 'character')
                           , self.name = '.self'
                           , initializer = initializer
                           )
             , 'TypedEnvironment')
    expect_identical(ls(winit, all=TRUE), c('.self', 'char', 'int'))
    expect_is(winit@initializer, 'function')
    expect_identical(winit$.self, winit)

    winit@initializer()
    expect_identical(winit$int, 101L)
    expect_identical(winit$char, 'y')
    expect_identical(winit$.self, winit)
})
