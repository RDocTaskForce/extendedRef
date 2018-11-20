#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `private_methods.R`')
#line 59 "R/private_methods.R"
test_that('PrivateMethod', {#@testing PrivateMethod
    fun <- function(...)append(...)
    method <- new('PrivateMethod', fun
                 , name='test', className = 'test-class'
                 , public.methods = 'append'
                 )
    expect_identical(s(S3Part(method, TRUE), class=NULL), fun)
    expect_is(method, 'PrivateMethod')
    expect_is(method, 'refMethodDef')
    expect_identical(method@mayCall, 'append')
})
#line 101 "R/private_methods.R"
test_that('initialize,privateMethodsLibrary-method', {#@testing
    bare <- privateMethodsLibrary()
    expect_is(bare, 'privateMethodsLibrary')
    expect_identical(ls(bare, all=TRUE), '.private.methods.library')
    expect_identical(environmentName(bare), "private methods library")

    w.methods <- privateMethodsLibrary(methods = list(say_hello=function()cat('hello\n')))
    expect_is(w.methods, 'privateMethodsLibrary')
    expect_identical( ls(w.methods, all=TRUE)
                      , c('.private.methods.library', 'say_hello'))
    expect_identical(environmentName(w.methods), "private methods library")
    expect_identical( environment(w.methods$say_hello)
                      , as.environment(w.methods))

    dne <- privateMethodsLibrary(className = "Does not exist")
    expect_identical(environmentName(dne), "Does not exist private methods library")
})
#line 118 "R/private_methods.R"
test_that('initialize,privateMethodsLibrary-method', {#@testing
    if (exists(classMetaName('test-class')))
        try(removeClass('test-class'), TRUE)
    test_class <- setRefClass('test-class', where = globalenv())
    classDef <- test_class$def
    # expect_false(has_private_methods_library(classDef))

    private.library <- privateMethodsLibrary( className = test_class@className
                                              , methods = list(hw=function()print('hello world'))
                                              , .lock=TRUE)
    expect_is(private.library, 'privateMethodsLibrary')
    expect_identical( environmentName(private.library)
                      , "test-class private methods library"
    )
    expect_true(environmentIsLocked(private.library))

    expect_true(exists('hw', private.library))
    expect_identical(environment(private.library$hw), as.environment(private.library))

    # expect_true(has_private_methods_library(test_class$def))

    # expect_warning( lib2 <- privateMethodsLibrary( test_class )
    #               , "test-class already has a private methods library defined")
    # expect_identical(lib2, private.library)

    expect_true(removeClass(test_class@className, where = globalenv()))
})
#line 174 "R/private_methods.R"
test_that('initialize,objectPrivateMethods-method', {#@testing
    if (exists(classMetaName('test-class')))
        try(removeClass('test-class'), TRUE)
    test_class <- setRefClass('test-class')
    library <- privateMethodsLibrary()
    test.obj <- test_class()
    .Object <- private_methods(test.obj, library)

    expect_identical(ls(.Object, all=TRUE), character())
    expect_true(removeClass(test_class$def@className))
    if (exists(classMetaName('test-class')))
        try(removeClass('test-class'), TRUE)
})
#line 187 "R/private_methods.R"
test_that('initialize,objectPrivateMethods-method', {#@testing
    if (exists(classMetaName('test-class')))
        try(removeClass('test-class'), TRUE)
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    expect_is(classDef, "refClassRepresentation")

    private.library <- privateMethodsLibrary( className= test_class@className
                                            , list(hw=function()print('hello world'))
                                            , .lock=TRUE)
    test.obj <- test_class()
    test.methods <- private_methods(test.obj, private.library)

    expect_is(test.methods, 'objectPrivateMethods')
    expect_true(environmentIsLocked(test.methods))
    expect_identical(parent.env(test.obj@.xData), as.environment(test.methods))

    expect_equal( ls(test.methods, all=TRUE), c('hw'))
    expect_identical(environment(test.methods$hw), test.obj@.xData)

    expect_true(removeClass(test_class$def@className))
})
#line 209 "R/private_methods.R"
test_that('private_methods with .', {#@testing private_methods with .
    Class <- "test with ."
    if (exists(classMetaName(Class))) removeClass(Class)
    gen <- setRefClass(Class, c(.='list'))
    library <-
        privateMethodsLibrary( className= Class
                             , list( hw=function()print('hello world')
                                   , .__initialize__. = function(...)
                                       . <<- list(...)
                                   )
                             , .lock=TRUE)
    obj <- gen()
    methods <- private_methods(obj, library)

    expect_identical(ls(methods, all=TRUE), c('.__initialize__.', 'hw'))

    expect_true(removeClass(Class))
})
