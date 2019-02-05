
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
if(FALSE){#@testing
    val <- s(FALSE, msg="hello there")
    expect_identical(attributes(val), list(msg='hello there'))

    name = 'frosty'
    val <- s(FALSE, name)
    expect_identical(attributes(val), list(name=name))

    val <- s(FALSE, msg="hello there", name)
    expect_identical(attributes(val), list(msg='hello there', name=name))
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
if(FALSE){#@testing
    who_mad <- function(mad.bears)
        ngettextf(length(mad.bears), "%2$s the bear was mad."
                      , "The %1$d bears, %2$s, were mad."
                      , length(mad.bears), comma_list(mad.bears))

    expect_identical(who_mad('Baloo'), "Baloo the bear was mad.")
    expect_identical(who_mad(c('Papa Bear', 'Mama Bear', 'Baby Bear'))
                    , "The 3 bears, Papa Bear, Mama Bear, and Baby Bear, were mad." )
}


set_environment <- function(fun, envir){
    environment(fun) <- envir
    return(fun)
}

# nocov start
._not_implemented <- function(object, ...)
    stop("not implimented for class", paste(class(object)), collapse='/')
# nocov end

`%!in%` <- Negate(`%in%`)


`c.{` <- function(x, ...){
    as.call(c(as.list(x), ...))
}


setInitialize <- function(...)setMethod(f = 'initialize', ...) # nocov

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
