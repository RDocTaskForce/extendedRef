#' @include ExtendedRefClass.R
utils::globalVariables(c('.', 'equals', '.refClassDef'))

### Reference Vectors #####
#' Reference Vectors and Reference Sets
#' 
#' Reference vectors act similar to lists, but have the additional
#' restriction that all elements must be of the same type.
#' Reference Sets, Collections of unique objects.
#'
#' A reference set is defined as a collection of unique objects.
#' The difference from reference vectors in that they
#' do not allow duplicate objects.
#' 
#' @param element The type of element allowed.
#' @param Class   The Class name for the vector.
#' @param condition.already.contains The condition to raise if the
#'            set already contains the object being added.
#' @inheritParams setExtendedRefClass
#' @inheritDotParams setExtendedRefClass
#' 
setRefVector <-
function( element
        , Class = paste0("ReferenceVector<", element, ">")
        , fields = list()
        , private = character()
        , static.const = list()
        , methods = list()
        , ...
        , where = topenv(parent.frame())
        ){
    . <- NULL
    where <- where
    assert_that( is.string(element)
               , is.string(Class)
               , isNamespace(where) || identical(where, globalenv())
               )
    if (assert_that('.' %!in% names(private))){
        private <- c(private, '.' = 'list')
    }
    if ('initialize' %!in% names(methods)) 
        methods[["initialize"]] <- function(...) { if (nargs()) .self$add(...) }
    if ('validate' %!in% names(methods))
        methods[['validate']] <- function(){validate_that(all_inherit(., element))}
    if ('is_valid' %!in% names(methods))
        methods[['is_valid']] <- function(){
            valid <- .self$validate()
            if (isTRUE(valid)) return(valid) else
            return(s(FALSE, msg=valid))
        }
    if ('add' %!in% names(methods))
        methods[['add']] <- function(...){
            l <- list(...)
            for (i in rev(seq_along(l))) {
                if (!is(l[[i]], element))
                    try(l[[i]] <- as(l[[i]], element))
            }
            assert_that(all_inherit(l, element, "`...`"))
            . <<- c(., l)
            invisible(.self)
        }
    if ('get' %!in% names(methods)) 
        methods[['get']] <- function(...){.[[...]]}
    if ('set' %!in% names(methods))
        methods[['set']] <- function(..., value){.[[...]] <<- value; invisible(.self)}
    if ('length' %!in% names(methods)) 
        methods[['length']] <- function(x){if(missing(x)) base::length(.) else base::length(x)}
    if ('names' %!in% names(methods))
        methods[['names']]  <- function(x){if(missing(x)) base::names(.)  else base::names(x)}
    if (assert_that("element" %!in% names(static.const)))
        static.const <- c(static.const, element=element)
    if ('as.list' %!in% names(methods))
        methods[['as.list']] <- function().

    generator <-
        setExtendedRefClass( Class=Class
                           , static.const = static.const
                           , fields = fields
                           , methods = methods
                           , private=private
                           , ..., where=where)
    setValidity(Class, function(object){
        if (!isTRUE(attr(object@.xData, 'extended.initialized'))) return(TRUE)
        object$validate()
    }, where=where)
    
    setMethod('[['    , c(Class, 'ANY'), function(x, i, ...)x$get(i, ...), where = where)
    setMethod('[[<-'  , c(Class, 'ANY'), function(x, i, ..., value)x$set(i,..., value=value), where = where)
    setMethod('length', Class, function(x)x$length(), where = where)
    setMethod('names' , Class, function(x)x$names(), where = where)
    setMethod('as.list', Class, function(x, ...)x$as.list(), where = where)
    
    return(invisible(generator))
}
if(FALSE){#@testing
    expect_silent(test_vector <- setRefVector('logical'))
    expect_is(test_vector, 'extendedRefObjectGenerator')

    expect_is(def <- test_vector$def, 'extendedRefClassDefinition')
    expect_identical(def@private.classes, c(.='list'))
    expect_identical( ls(def@private.library, all=TRUE)
                    , c('.__initialize__.', '.private.methods.library'))
    
    bare <- test_vector()
    expect_is(bare, "ReferenceVector<logical>")
    expect_length(bare, 0L)
    expect_identical(get('element', bare), 'logical')

    val <- test_vector(a=TRUE, b=FALSE)
    expect_is(val, "ReferenceVector<logical>")
    expect_identical(get('element', val), 'logical')
    expect_length(val, 2L)
    expect_identical(val[[1]], TRUE)
    expect_identical(val[['a']], TRUE)
    expect_identical(val[[2]], FALSE)
    expect_identical(val[['b']], FALSE)

    new <- c(c=NA)
    val$add(c=NA)
    expect_length(val, 3L)
    expect_identical(val[[3]], NA)

    expect_true(val$is_valid())

    val$add(1)
    expect_length(val, 4L)
    expect_true(val$is_valid())

    with(val@.xData, . <<- as.list(letters[1:5]))
    expect_false(val$is_valid())
}
if(FALSE){#@testing
    refList <- setRefVector('ANY', 'refList')
    val <- refList('a', 1L, TRUE)
    expect_is(val, 'refList')
    expect_is(val, 'envRefClass')
    expect_is(val, 'ExtendedRefClass')

    expect_identical(val[[1]], 'a')
    expect_identical(val[[2]], 1L)
    expect_identical(val[[3]], TRUE)

    y <- val
    y[[4]] <- 'testing'
    expect_identical(val[[4]], 'testing')

    expect_length(val, 4L)
    expect_null(names(val))
}

### Reference Set #####
#' @rdname setRefVector
setRefSet <-
function( element
        , Class = paste0("ReferenceSet<", element, ">")
        , methods = list()
        , ...
        , static.const = list()
        , static.methods = list()
        , condition.already.contains = 
                c('message', 'warning', 'error', 'none')
                #< Type of condition to raise if object is
                #< already contained in the collection.
        , where = topenv(parent.frame())
        ){
    . <- NULL
    where <- where
    condition.already.contains <- match.arg( condition.already.contains)
    assert_that( is.string(element)
               , is.string(Class)
               , isNamespace(where) || identical(where, globalenv())
               )
    if (!('validate' %in% names(methods)))
        methods[['validate']] <- function(){
            validate_that( all_inherit(., element)
                         , !anyDuplicated(.)
                         )
        }
    if (!('add' %in% names(methods)))
        methods[['add']] <- function(...){
            l <- list(...)
            for (i in rev(seq_along(l))) {
                if (!is(l[[i]], element))
                    try(l[[i]] <- as(l[[i]], element))
                if (any(purrr::map_lgl(., equals, l[[i]]))) {
                    pkgcond::condition( ._('Set already contains the element given at position %d.', i)
                                      , condition.already.contains
                                      , type = "already.contains"
                                      , scope = c( .refClassDef@package
                                                 , .refClassDef@className
                                                 , "add")
                                      )
                    l <- l[-i]
                }
            }
            pkgcond::assert_that( all_inherit(l, element, "`...`")
                                , !anyDuplicated(l))
            l <- c(., l)
            . <<- l
            if ( exists('sort', .refClassDef@refMethods)
              && is(sort, 'refMethodDef')
               )
                sort()
            invisible(.self)
        }
    if (assert_that("condition.already.contains" %!in% names(static.const)))
        static.const[['condition.already.contains']]  <- condition.already.contains
    
    
    if ('equals' %in% names(static.methods)){
       assert_that( is.function(static.methods$equals)
                  , number_of_arguments(static.methods$equals) >= 2
                  )
    } else {
        static.const$equals <- base::identical
    }
    generator <-
        setRefVector( element, Class=Class
                    , methods = methods
                    , static.const = static.const
                    , static.methods = static.methods
                    , ..., where=where)
    return(invisible(generator))
}
if(FALSE){#@testing
    test_class <- setClass("test-class", list(name='character', age = 'numeric'))
    test_set <- setRefSet( 'test-class'
                         , where = globalenv()
                         , static.const = list(
                             equals = function(x, y) x@name == y@name
                            )
                         )

    expect_is(test_set, 'refObjectGenerator')

    my.set <- test_set()
    expect_is(my.set, 'ReferenceSet<test-class>')
    expect_length(my.set, 0L)

    my.set$add(elem <- test_class(name = 'object 1', age = 0L))
    expect_length(my.set, 1L)

    expect_message( my.set$add(test_class(name = 'object 1', age = 1L))
                  , class = "RefSet(Documentation)-message-already.contains"
                  )
    my.set$add(test_class(name='another'))
    expect_length(my.set, 2L)
}
