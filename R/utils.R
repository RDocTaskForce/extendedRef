#' @import assertthat
#' @import methods
#' @importFrom stats setNames
#' @importFrom rlang %||%

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


._ <- function(msg, ..., domain=NULL){
    if (...length())
        gettextf(msg, ..., domain = domain)
    else
        gettext(msg, domain = domain)
}
comma_list <-
function( x                 #< vector to make into a comma list
        , sep  = ", "       #< separator for multiple elements.
        , sep2 = " and "    #< separator for only two elements.
        , sep.last = ", and " #< separator between last and second to last for more than two elements.
        , terminator = ''   #< ends the list.
        ){
    #! Create a properly formatted comma separated list.
    if (length(x) == 1) return(paste(x))
    else if (length(x) == 2)
        return(paste(x, collapse=sep2))
    else
        return(paste(x, c(rep(sep, length(x)-2), sep.last, terminator), sep='', collapse=''))
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

all_inherit <- function(lst, what, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.character(what) || is.null(what) )
    if (all(. <- purrr::map_lgl(lst, inherits, what=what, which=FALSE)))
        return(TRUE)
    msg <- if (sum(!.) > 1L) {
        ._("%s has bad elements at %s which do not inherit from %s."
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
          ) } else {
        bad.class <- purrr::map_chr(lst[!.], class0)
        ._("%s has bad element at %s which does not inherit from %s. It is a %s"
           , act$lab
           , comma_list(which(!.))
           , comma_list(dQuote(what), sep2 = ' or ', sep.last = ' or ')
           , dQuote(bad.class)
        )
          }
    return(structure(FALSE, msg=msg, bad.elements = which(!.)))
}

class0 <- function(x)paste(class(x), collapse='/')
expect_valid <-
    function (object, complete=FALSE, info=NULL, label=NULL){
        act <- testthat::quasi_label(rlang::enquo(object), label)
        is.valid <- validObject(object, test=TRUE, complete=complete)
        testthat::expect(isTRUE(is.valid)
                         , ._("%s is not valid; %s", act$lab, dQuote(is.valid))
                         , info=info
        )
    }


set_environment <- function(fun, envir){
    environment(fun) <- envir
    return(fun)
}

is_valid <- function(object, complete=FALSE){
    valid <- validObject(object, test=TRUE, complete=complete)
    if(isTRUE(valid)) return(TRUE)
    else return(s(FALSE, msg=valid))
}
are_valid <-
    function(lst, complete=FALSE, simplify=NA){
        valid <- lapply(lst, is_valid, complete=complete)
        if (isFALSE(simplify)) return(valid) else
            if (isTRUE(simplify) || all(valid)) return(simplify2array(valid))
        else return(valid)
    }
are <- function(lst, class2){
    purrr::map_lgl(lst, is, class2)
}
if(FALSE){#@testing
    lst <- list('a', 1L, TRUE)
    
    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
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


