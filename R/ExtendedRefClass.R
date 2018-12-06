#' @include private_methods.R
#' @include static_methods.R
#' @include static_variables.R
#' @include StaticTriad.R

# Optional Classes --------------------------------------------------------
make_class_optional('privateMethodsLibrary')

# extendedRefClassDefinition ----------------------------------------------
#' Extended Reference Definitions
#'
#' The definition for an extended reference class extends the
#' [`refClassRepresentation`][ReferenceClasses].  Of note is that the definition holds
#' the private methods library, the constant methods library, the
#' static constant environment and the static environment for all
#' objects of the class.
#'
setClass( 'extendedRefClassDefinition'
        , contains='refClassRepresentation'
        , slots  = c( private.classes = 'character'
                    , private.library = 'optional-privateMethodsLibrary'
                    , static.const    = 'optional-StaticConstEnv'
                    , static          = 'optional-classStaticEnv'
                    , static.methods  = 'optional-StaticMethods'
                    )
        , prototype = list( static.const      = NULL
                          , static            = NULL
                          , static.methods    = NULL
                          , private.classes   = character()
                          , private.library   = NULL
                          )
        )
if(FALSE){#@testing
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
}
if(FALSE){#@testing
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
}
setAs('extendedRefClassDefinition', 'StaticTriad', function(from){
    new('StaticTriad', const   = from@static.const
                     , vars    = from@static
                     , methods = from@static.methods
                     )
})

# ExtendedRefClass -------------------------------------------------------
setRefClass('ExtendedRefClass', contains='envRefClass')
setMethod('initialize', 'ExtendedRefClass', initialize <-
function(.Object, ...){
    Class <- class(.Object)
    classDef <- getClass(Class)
    .Object <- callNextMethod(.Object, ...)
    attr(.Object@.xData, 'name') <- paste(Class, 'object environment')
    parent <- parent.env(.Object@.xData)

    # if (length(classDef@refMethods))
    if (!is.null(classDef@static.const))
        parent <- insert_parent_env(.Object, classDef@static.const)
    if (!is.null(static <- classDef@static)){
        parent <- insert_parent_env(.Object, classDef@static)
        if (!static$static.initialized && is.function(static@initializer)) {
            static@initializer()
            static$static.initialized <- TRUE
        }
    }
    if (!is.null(classDef@static.methods)){
        parent <- insert_parent_env(.Object, classDef@static.methods)
        if (exists('static_initialize', envir = classDef@static.methods, inherits = FALSE)) {
            classDef@static.methods$static_initialize()
            if (is.environment(static) && exists('static.initialized', static))
                static$static.initialized <- TRUE
        }
    }
    if (length(classDef@private.classes) > 0L) {
        private.env <- new( 'TypedEnvironment'
                          , classDef@private.classes
                          , parent = parent
                          , self.name = 'private'
                          )
        attr(private.env@.xData, 'name') <- 'object private variables'
        parent <- insert_parent_env(.Object, private.env)
    }
    if (!is.null(classDef@private.library)) {
        private.methods <- private_methods(.Object)
        if (exists('private_initialize', envir = private.methods, inherits = FALSE))
            private.methods$private_initialize()
        if (exists('.__initialize__.', envir = private.methods, inherits = FALSE)) {
            init <- private.methods$.__initialize__.
            for (dep in init@mayCall)
                do.call(`$`, args = list(.Object, dep))
            init(...)
        }
    }
    attr(.Object, 'extended.initialized') <- TRUE
    validObject(.Object)
    return(.Object)
})

# extendedRefObjectGenerator --------
#' Extended Reference Generators
#'
#' A [refObjectGenerator][ReferenceClasses] is turned into a generator
#' for extended reference classes by replacing the `generator` slot
#' with an `extendedRefGeneratorSlot` object and the `def` field in the
#' `extendedRefGeneratorSlot` with a `extendedRefClassDefinition` object.
#'
#' The `extendedRefGeneratorSlot` adds the `static` field holding a
#' [StaticTriad] object to access the static variables and methods without
#' needing to access them through an instance of the class.
extendedRefGeneratorSlot <-
setRefClass('extendedRefGeneratorSlot', contains='refGeneratorSlot'
           , fields = c(static='StaticTriad'))
#' @rdname  extendedRefGeneratorSlot-class
setClass( 'extendedRefObjectGenerator', contains = c('refObjectGenerator')
        , slots = c( static = 'StaticTriad'))
setInitialize('extendedRefObjectGenerator', initialize <-
function(.Object, ...){
    .Object <- callNextMethod()
    .Object@generator <- new('extendedRefGeneratorSlot', .Object@generator)
    .Object@generator$static <- .Object@static
    .Object
})
if(FALSE){#@testing extendedRefObjectGenerator
    if (exists(classMetaName('test')))
        try(removeClass('test'), TRUE)
    super <- setRefClass('test')
    .Object <- new('extendedRefObjectGenerator', super)

    expect_is(.Object, 'extendedRefObjectGenerator')
    expect_identical(.Object@static, .Object@generator$static)

    expect_true(removeClass('test'))
}

