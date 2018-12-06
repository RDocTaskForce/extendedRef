# Typed Environments ------
#' Typed Environments
#'
#' Typed environments are environments that enforce objects to have a specified class.
#' They essentially work similar to reference classes but without as much overhead.
#' They provide the foundation for static and private variables of the
#' extended reference classes.
#'
#'
#' @slot classes A named vector indicating the appropriate classes for objects.
#' @slot initializer an optional function that can be specified to initialize the values.
# @param parent  The parent of the environment
# @param initialize.content a logical flag indicating if the initializer function should be run immediately.
# @param self.name If specified will assign to this variable a copy of the resulting object.
#'
setClass( "TypedEnvironment", contains = 'environment'
        , slots=c( classes = 'character'
                 , initializer = 'OptionalFunction'
                 ))
# * Validity =====
setValidity('TypedEnvironment',
function(object){
    if (length(object@.xData) == 0L) return(TRUE)
    is.correct.class <-
      mapply( is
            , mget(names(object@classes), object@.xData)
            , object@classes)
    if (all(is.correct.class)) return(TRUE)
    which.bad <- which(!is.correct.class)
    msg <- ngettext( length(which.bad)
                   , "Object %s is not a %s."
                   , "Objects %s are not of the correct class."
                   )
    msg <- gettextf(msg, comma_list(sQuote(names(which.bad)))
                       , dQuote(object@classes[[which.bad[[1]] ]]))
    msg
})
# * Initialize =====
setMethod('initialize', 'TypedEnvironment',
function( .Object
        , classes = character(0)
        , parent = baseenv()
        , initializer = NULL
        , initialize.content = FALSE
        , self.name = character()
        ){
    assert_that( rlang::is_dictionaryish(classes)
               , is(initializer, 'OptionalFunction')
               )
    .Object <- callNextMethod(.Object)

    if (assert_that(is.environment(parent))) parent.env(.Object@.xData) <- parent

    for (i in seq_along(classes))
        assign(names(classes)[[i]], new(classes[[i]]), envir = .Object@.xData)

    if (!is.null(initializer)) {
        environment(initializer) <- .Object@.xData
        .Object@initializer <- initializer
    }
    .Object@classes <- classes

    if (length(self.name)) {
        assert_that( is.string(self.name)
                   , nchar(self.name) > 0L
                   , self.name %!in% names(classes)
                   )
        classes <- c(classes, setNames(class(.Object), self.name))
        assign(self.name, .Object, envir = .Object@.xData)
        lockBinding(self.name, .Object@.xData)
    }
    lockEnvironment(.Object@.xData, bindings=FALSE)
    if (initialize.content) {
        .Object@initializer()
    }
    return(.Object)
})
# @testing ####
if(FALSE){#@testing
    expect_is(bare <- new('TypedEnvironment'), 'TypedEnvironment')
    expect_identical(ls(bare, all=TRUE), character())
    expect_null(bare@initializer)
    expect_true(validObject(bare, test=TRUE))

    expect_is( typed <- new('TypedEnvironment', c(int = 'integer', char = 'character', fun = 'function'))
             , 'TypedEnvironment')
    expect_identical(ls(typed, all=TRUE), c('char', 'fun', 'int'))
    expect_identical(typed$int, integer())
    expect_identical(typed$char, character())
    expect_identical(typed$fun, new('function'))
    expect_null(typed@initializer)
    expect_true(validObject(typed, test=TRUE))

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

    expect_is( winit2<- new('TypedEnvironment'
                           , c(int = 'integer', char = 'character')
                           , self.name = '.self'
                           , initializer = initializer
                           , initialize.content = TRUE
                           )
             , 'TypedEnvironment')
    expect_identical(winit2$int, 101L)
    expect_identical(winit2$char, 'y')
    expect_identical(winit2$.self, winit2)
}
