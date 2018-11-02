#' @include static_variables.R
#' @include static_methods.R

# StaticTriad ---------------------------------------------
#' The Static Triad
#' 
#' The static triad consists of a const environment for constants, 
#' a vars environment for variables, and a methods environment for methods.
#'
#' @param x       A StaticTriad Object
#' @param name    The name of the constant, variable, or method
#' @param value   The replacement value for variables only.
#'
#' @section Methods:
#'  * `$` is definded to make all variables, constants and 
#'        methods visible through the single object.
#'  * `$<-` Does the same but protects constants and methods 
#'        from being overwritten.
StaticTriad <- 
setRefClass( "StaticTriad"
           , fields = c( const    = 'optional-StaticConstEnv'
                       , vars     = 'optional-classStaticEnv'
                       , methods  = 'optional-StaticMethods'
                       )
           )
#' @rdname StaticTriad-class
setMethod('$', 'StaticTriad', function(x, name){
    tryCatch( eval(substitute(callNextMethod(x, name)))
            , error = function(e){
                if (grepl("is not a valid field or method name for reference class", e$message)){
                    if (!is.null(x$methods) && exists(name, envir = x$methods, inherits = FALSE)) return(get(name, envir = x$methods))
                    if (!is.null(x$vars   ) && exists(name, envir = x$vars   , inherits = FALSE)) return(get(name, envir = x$vars   ))
                    if (!is.null(x$const  ) && exists(name, envir = x$const  , inherits = FALSE)) return(get(name, envir = x$const  ))
                }
                stop(e)
            })
})
#' @rdname StaticTriad-class
setMethod('$<-', 'StaticTriad', function(x, name, value){
    what <- substitute(name)
    if (is.symbol(what)) 
        name <- as.character(what)
    if (name %in% names(x)){
        assign(name, value, envir = x@.xData)
        return (invisible(x))
    }
    if (!is.null(x$vars) && exists(name, envir = x$vars   , inherits = FALSE)){
        if (!is(value, . <- x$vars@classes[name]))
            stop(sQuote(name), "must be a ", .)
        assign(name, value, envir = x$vars)
        return(invisible(x))
    }
    if (!is.null(x$methods) && exists(name, envir = x$methods, inherits = FALSE))
        stop(sQuote(name), " is a static mathod and cannot be changed.")
    if (!is.null(x$const  ) && exists(name, envir = x$const  , inherits = FALSE)) 
        stop(sQuote(name), " is a static const value and cannot be changed.")
    stop(sQuote(name), "is not a valid static variable.")
})
if(FALSE){#@testing
    x <- new('StaticTriad')
    x$const <- static_const(list(a=1))
    x$vars  <- new_static_env(c(int = 'integer'))
    x$methods <- static_methods(list(say_hi = function()cat('hi\n')))
    expect_identical(x$a, 1)
    expect_identical(x$int, integer())
    
    
    expect_identical(x$int <- 3L, 3L)
    expect_identical(x$int, 3L)
    expect_is(x, 'StaticTriad')
    
    expect_error(x$say_hi <- 'hi', "is a static mathod and cannot be changed")
    expect_error(x$a <- 2, "is a static const value and cannot be changed")
    expect_error(x$b <- TRUE, "is not a valid static variable.")
}
#' @describeIn StaticTriad-class coerce to StaticTriad
setAs('extendedRefClassDefinition', 'StaticTriad', function(from){
    new('StaticTriad', const   = from@static.const
                     , vars    = from@static
                     , methods = from@static.methods
                     )
})
