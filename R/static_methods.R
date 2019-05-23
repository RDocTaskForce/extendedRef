#' @include private_methods.R


#' @title Static Methods
#' @description
#' Static methods denote functions that are intended to act on
#' class static variables only.  They inherit from [`refMethodDef`][ReferenceClasses],
#' and use the same methods.
#'
#' @inheritParams MethodsLibrary
#'
setClass('StaticMethod', contains = 'refMethodDef')
if(FALSE){#@testing
    def <- function().count <<- 0L
    def <- function().count <<- 0L
    def <- function().count <<- 0L
    method <- new('StaticMethod', def, refClassName = 'test', name='reset')
    expect_equal(method@name, 'reset')
    expect_equal(method@refClassName, 'test')
    expect_equal(method@refClassName, 'test')
    expect_identical(environment(method), environment(def))
}

#' @rdname StaticMethod-class
static_methods <-
setClass('StaticMethods'
        , contains='MethodsLibrary'
        , prototype = list(method.type='StaticMethod')
        )
setMethod('initialize', 'StaticMethods',
function( .Object
        , methods=list()
        , parent=NULL
        , className=NULL
        , ...
        , .lock=TRUE){
    .Object <- callNextMethod(.Object
                             , methods=methods
                             , method.type='StaticMethod'
                             , ...)
    if (!is.null(parent) && assert_that(is.environment(parent)))
        parent.env(.Object@.xData) <- parent
    attr(.Object@.xData, 'name') <- if(is.null(className)) 'static methods library' else
        paste(className, 'static methods library')

    if (.lock) lockEnvironment(.Object@.xData, bindings = TRUE)
    return(.Object)
})
if(FALSE){#@testing
    expect_is(bare <- static_methods(), 'StaticMethods')
    expect_length(as.list(bare, all=TRUE), 0L)

    expect_is(lib <- static_methods(list(say_hi= function(){cat('hi\n')})), 'StaticMethods')
    expect_length(ls(lib, all.names = TRUE), 1L)

    expect_is(lib$say_hi, 'StaticMethod')
    expect_identical(environment(lib$say_hi), lib@.xData)
}
