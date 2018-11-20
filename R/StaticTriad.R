#' @include static_variables.R
#' @include static_methods.R

# Optional Classes --------------------------------------------------------
make_class_optional('classStaticEnv')
make_class_optional('StaticMethods')
make_class_optional('StaticConstEnv')


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
    const   <- get('const'  , x)
    vars    <- get('vars'   , x)
    methods <- get('methods', x)
    if (!is.null(methods) && exists(name, methods)) return(get(name, methods)) else
    if (!is.null(vars   ) && exists(name, vars   )) return(get(name, vars   )) else
    if (!is.null(const  ) && exists(name, const  )) return(get(name, const  )) else
    if (exists(name, x, inherits = FALSE)) return(get(name, x)) else
    tryCatch( callNextMethod(x, name=name)
                    # envRefInferField(x, what, getClass(class(x)), selfEnv)
            , error = function(e){
                if (grepl("^.name. is not a valid field or method name for reference class .StaticTriad."
                         , conditionMessage(e)))
                    pkg_error(._("%s is not a valid static variable or method.", sQuote(name) ))
            })
})
if(FALSE){#@testing
    x <- new('StaticTriad')
    const <- static_const(list(a=1))
    x$const <- const
    x$vars  <- new_static_env(c(int = 'integer'), parent=globalenv())
    x$methods <- static_methods(list(say_hi = function()cat('hi\n')), parent = globalenv())

    L <- list(x=x)

    expect_identical(x$const, const)
    expect_identical(L$x$const, x$const)

    expect_error(x$junk, ".junk. is not a valid static variable or method\\.")
}

#' @rdname StaticTriad-class
setMethod('$<-', 'StaticTriad', function(x, name, value){
    what <- substitute(name)
    if (is.symbol(what))
        name <- as.character(what)
    if (name %in% names(x)){
        assign(name, value, envir = x@.xData)
        return (invisible(x))
    }
    if (!is.null(methods <- get('methods', x, inherits = FALSE))
       && exists(name, envir = methods, inherits = FALSE))
        pkg_error(._("%s is a static method and cannot be changed.", sQuote(name)))
    if (!is.null(vars <- get('vars'   , x, inherits = FALSE))
       && exists(name, envir = vars, inherits = FALSE)
       && assert_that(is(value, vars@classes[name]))
       ){
        assign(name, value, envir = x$vars)
        return(invisible(x))
    }
    if (!is.null(const <- get('const'  , x, inherits = FALSE))
      && exists(name, envir = x$const  , inherits = FALSE))
        pkg_error(._(" is a static const value and cannot be changed.", sQuote(name)))
    pkg_error(._("%s is not a valid static variable.", sQuote(name)))
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

    expect_error(x$say_hi <- 'hi', "is a static method and cannot be changed")
    expect_error(x$a <- 2, "is a static const value and cannot be changed")
    expect_error(x$b <- TRUE, "is not a valid static variable.")
}
