#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `ExtendedRefClass.R`')
#line 33 "R/ExtendedRefClass.R"
test_that('setClass("extendedRefClassDefinition", ...)', {#@testing
    bare <- new('extendedRefClassDefinition')
    expect_null(bare@private.library)
    expect_identical(bare@private.classes, character())
    expect_null(bare@static)
    expect_null(bare@static.const)
    expect_null(bare@static.methods)

    if (exists(classMetaName('test')))
        try(removeClass('test'), silent = TRUE)
    ref_generator <- setRefClass('test', fields = list(count = 'integer'))
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

    expect_true(removeClass(ref_generator@className))
})
#line 164 "R/ExtendedRefClass.R"
test_that('extendedRefObjectGenerator', {#@testing extendedRefObjectGenerator
    if (exists(classMetaName('test')))
        try(removeClass('test'), TRUE)
    super <- setRefClass('test')
    .Object <- new('extendedRefObjectGenerator', super)

    expect_is(.Object, 'extendedRefObjectGenerator')
    expect_identical(.Object@static, .Object@generator$static)

    expect_true(removeClass(super@className))
})
