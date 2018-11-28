#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `static_variables.R`')
#line 64 "R/static_variables.R"
test_that('initialize,classStaticEnv-method', {#@testing
    bare <- new_static_env()
    expect_is(bare, "classStaticEnv")
    expect_identical(environmentName(bare), "Static Environment")
    expect_error(bare$a <- TRUE, "cannot add bindings to a locked environment")

    w.name <- new_static_env(className = 'test class')
    expect_is(w.name, "classStaticEnv")
    expect_identical(environmentName(w.name), "test class Static Environment")
    expect_error(w.name$a <- TRUE, "cannot add bindings to a locked environment")

    w.objects <- new_static_env(className='test class', c(a='logical', b='integer'))
    expect_is(w.objects, "classStaticEnv")
    expect_identical(environmentName(w.objects), "test class Static Environment")
    expect_identical(w.objects$a, logical(0))
    expect_identical(w.objects$b, integer(0))
    expect_silent(w.objects$a <- TRUE)
    expect_silent(w.objects$b <- TRUE)
    expect_error(validObject(w.objects), paste0("Object ", sQuote('b'),  " is not a ", dQuote('integer'), "."))
    expect_error(w.objects$c <- TRUE, "cannot add bindings to a locked environment")

    expect_silent(w.objects$a <- 1)
    expect_error(validObject(w.objects), paste0("Objects ", comma_list(sQuote(c('a', 'b')))
                                               ,  " are not of the correct class."))
})
#line 104 "R/static_variables.R"
test_that('initialize,StaticConstEnv-method', {#@testing
    const.env <-
        static_const(list( flag = TRUE
                         , char = 'a'
                         ))
    expect_identical(const.env$flag, TRUE)
    expect_identical(const.env$char, 'a')

    expect_identical(environmentName(const.env), "Static Const Environment")

    const.w.name <- static_const(list( flag = TRUE, char = 'a')
                                , className = "test-class" )
    expect_identical(environmentName(const.w.name), "test-class Static Const Environment")
    expect_true(environmentIsLocked(const.w.name))

    expect_error(const.env$flag <- FALSE)
    expect_error(const.env$char <- 'b')
    expect_error(const.env$int  <- 1L)
})
