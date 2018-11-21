
s <- function( .Data, ...){
    new.attr <- list(...)
    if (is.null(names(new.attr)))
        names(new.attr) <- as.character(substitute(c(...)))[-1]
    else if(any(. <- is.na(names(new.attr)) | names(new.attr) == ''))
        names(new.attr) <- ifelse(., as.character(substitute(c(...)))[-1], names(new.attr))

    for (a in names(new.attr))
        attr(.Data, a) <- new.attr[[a]]
    return(.Data)
}

ngettextf <-
    function( n
              , msg1
              , msg2
              , ...
              , domain=NULL
    ){
        if (n<=1) gettextf(msg1, ..., domain = domain)
        else gettextf(msg2, ..., domain = domain)
    }

set_environment <- function(fun, envir){
    environment(fun) <- envir
    return(fun)
}

._not_implemented <- function(object, ...)
    stop("not implimented for class", paste(class(object)), collapse='/')


`%!in%` <- Negate(`%in%`)


`c.{` <- function(x, ...){
    as.call(c(as.list(x), ...))
}


setInitialize <- function(...)setMethod(f = 'initialize', ...)

.checkFieldsInMethod <-
    get('.checkFieldsInMethod', asNamespace('methods'))

number_of_arguments <- function(f){
    args <- formals(f)
    if (is.null(args))
        return(length(utils::head(as.list(args(f)), -1)))
    if ('...' %in% names(args))
        return(Inf)
    else
        length(args)
}
if(FALSE){#@testing
    expect_equal(number_of_arguments(rnorm), 3L)
    expect_equal(number_of_arguments(paste), Inf)
    expect_equal(number_of_arguments(xtfrm), 1)
}
