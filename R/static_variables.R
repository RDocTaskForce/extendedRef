#' @include TypedEnvironment.R

# as refClassRepresentation -----------------------------------------------

setAs('character'             , 'classRepresentation', function(from)getClass(from))
setAs('character'             , 'refClassRepresentation', function(from)getRefClass(from))
setAs('refClass'              , 'refClassRepresentation', function(from)from$.refClassDef)
setAs('refObjectGenerator'    , 'refClassRepresentation', function(from)from$def)
setAs('classGeneratorFunction', 'classRepresentation', function(from)getClass(from@className))


# classStaticEnv ----------------------------------------------------------
#' Static and Static Const Variable Environments
#'
#' Static environments contain variables that are common to all
#' instances of a class.  Static Const variables are accessible to
#' all class objects but cannot be changed.
#'
#' @inheritParams TypedEnvironment
#' @param initialized.name Name of the variable which indicates if the environment has been initialized or not.
#' @param initialized.state The initial state of the initialized variable named in `initialized.name`
#' @param className is used in assigning the resulting environment name.
#' @param ... arguments to initialize.
#'
new_static_env <-
setClass( 'classStaticEnv', contains='TypedEnvironment')
setMethod('initialize', 'classStaticEnv', initialize <-
  function( .Object
          , classes = character(0)
          , ...
          , initializer = NULL
          , self.name = "static"
          , initialized.name = 'static.initialized'
          , initialized.state = is.null(initializer)
          , className = NULL
          ){
    classes = c(classes, setNames('logical', initialized.name))
    if (is.null(initializer))
        .Object@.xData[[initialized.name]] <- TRUE
    else {
        body(initializer) <-
        `c.{`( body(initializer)
             , substitute({
                              static.initialized <<- TRUE
                              lockBinding(initialized.name, static@.xData)
                          }
                         , list( static = as.name(self.name)
                               , static.initialized = as.name(initialized.name)
                               , initialized.name=initialized.name
                               )
                         )
             )
    }

    .Object <- callNextMethod( .Object, classes=classes,...
                             , self.name=self.name, initializer=initializer)
    if (!is.null(initialized.name) && assert_that(is.string(initialized.name)))
        assign(initialized.name, initialized.state, .Object@.xData)

    attr(.Object@.xData, 'name') <- if(is.null(className)) 'Static Environment' else
        paste(className, 'Static Environment')
    return(.Object)
})
if(FALSE){#@testing
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
}

# static_cost -----------------------------------------------------------------------
#' @rdname classStaticEnv-class
static_const <-
setClass('StaticConstEnv', contains='environment')
setMethod('initialize', 'StaticConstEnv',
function(.Object, content=list(), parent = NULL, className = NULL){
    .Object@.xData <- as.environment(content)
    if (!is.null(parent) && assert_that(is.environment(parent)))
        parent.env(.Object@.xData) <- parent
    attr(.Object@.xData, 'name') <- if(is.null(className)) 'Static Const Environment' else
        paste(className, 'Static Const Environment')
    lockEnvironment(.Object@.xData, TRUE)
    return(.Object)
})
if(FALSE){#@testing
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
}

