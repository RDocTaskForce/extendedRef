#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `ExtendedRefClass.R`')
#line 33 "C:/rdtf/extendedRef/R/ExtendedRefClass.R"
test_that('setClass("extendedRefClassDefinition", ...)', {#@testing
    bare <- new('extendedRefClassDefinition')
    expect_null(bare@private.library)
    expect_identical(bare@private.classes, character())
    expect_null(bare@static)
    expect_null(bare@static.const)
    expect_null(bare@static.methods)

    expect_is(triad <- as(bare, 'StaticTriad'), "StaticTriad")
    expect_null(triad$vars)
    expect_null(triad$methods)
    expect_null(triad$const)
})
#line 46 "C:/rdtf/extendedRef/R/ExtendedRefClass.R"
test_that('setClass("extendedRefClassDefinition", ...)', {#@testing
    if (exists(classMetaName('test'), where = globalenv()))
        try(removeClass('test', where = globalenv()), silent = TRUE)
    ref_generator <- setRefClass('test', fields = list(count = 'integer')
                                , where = globalenv() )
    ref.def <- ref_generator$def

    private.classes = c(count.when.created = 'integer')
    private.library <- privateMethodsLibrary( className = 'test' )
    static          <- new_static_env( c(count='integer'), className = 'test'
                                     , initializer = function(){count <<- 0L} )
    static.const    <- static_const(list(name = "A counted class"), className='test')
    static.methods  <- static_methods(list( reset_count = function(){count <<- 0L}))

    wprivate <- new( 'extendedRefClassDefinition'
                   , ref.def
                   , private.classes = private.classes
                   , private.library = private.library
                   , static = static
                   , static.const = static.const
                   , static.methods = static.methods
                   )
    expect_is(wprivate, 'extendedRefClassDefinition')
    expect_identical(wprivate@private.classes, private.classes)
    expect_identical(wprivate@private.library, private.library)
    expect_identical(wprivate@static, static)
    expect_identical(wprivate@static.const, static.const)
    expect_identical(wprivate@static.methods, static.methods)

    expect_false(wprivate@static$static.initialized)
    expect_identical(wprivate@static$count, integer(0))

    wprivate@static@initializer()
    expect_identical(wprivate@static$count, 0L)
    expect_true(wprivate@static$static.initialized)

    expect_is(triad <- as(wprivate, 'StaticTriad'), 'StaticTriad')
    expect_identical(triad$vars, static)
    expect_identical(triad$const, static.const)
    expect_identical(triad$methods, static.methods)

    removeClass(ref_generator@className, where = globalenv())
})
#line 176 "C:/rdtf/extendedRef/R/ExtendedRefClass.R"
test_that('extendedRefObjectGenerator', {#@testing extendedRefObjectGenerator
    if (exists(classMetaName('test')))
        try(removeClass('test'), TRUE)
    super <- setRefClass('test')
    .Object <- new('extendedRefObjectGenerator', super)

    expect_is(.Object, 'extendedRefObjectGenerator')
    expect_identical(.Object@static, .Object@generator$static)

    expect_true(removeClass('test'))
})
