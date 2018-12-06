#' @include utils.R


# MethodsLibrary ----------------------------------------------------------


#' @title Methods Library
#' @description
#' A methods library is a container for methods of a specific type.
#' All objects in the library should be functions of type `method.type`.
#' One exception is made for objects that points back to the
#' library object, i.e. a self referential object.
#'
#' @slot method.type A string giving the type of method expected,
#'                   the type should inherit from 'function', and
#'                   preferably from [`refMethodDef`][ReferenceClasses].
# @slot  methods The list of methods to include in the library.
#                Methods will be converted to the appropriate type automatically.
# @slot method.parent The parent environment for methods within the library.
#                     Defaults to the library environment itself.
# @slot parent The parent environment for the library.
#              Defaults to the calling frame.
# @slot .lock A logical flag indicating if the library is to be locked and
#             prevent adding or changing definitions.
#             Default is to lock if methods are provided.
#'
MethodsLibrary <-
setClass( 'MethodsLibrary'
        , contains = 'environment'
        , slots = c(method.type = 'character')
        , prototype = list(method.type='refMethodDef')
        )
# * Validity =====
setValidity('MethodsLibrary', function(object){
  list.object <- as.list(object)
  if (length(list.object) == 0) return(TRUE)
  are.valid <- sapply(list.object, is, object@method.type) &
               sapply(lapply(list.object, validObject, test = TRUE), isTRUE)
  if (all(are.valid)) return(TRUE)
  for (i in which(!are.valid)){
      if ( is.environment(list.object[[i]])
        && identical(as.environment(list.object[[i]]), as.environment(object))
         ) are.valid[[i]] <- TRUE
  }
  if (all(are.valid)) return(TRUE)

  ngettextf( sum(!are.valid)
           , "Element %s is not a valid %s object."
           , "Elements %s are not valid %s objects."
           , comma_list(names(list.object)[[which(!are.valid)]])
           , object@method.type
           )
})
# * Initialize ----
setMethod('initialize', 'MethodsLibrary',
function(.Object
        , methods=list()
        , method.type='refMethodDef'
        , method.parent=as.environment(.Object)
        , parent = NULL
        , ... #< passed to `new(method.type, ...)`
        , .lock = length(methods) > 0L
        ){
    .Object <- callNextMethod(.Object)
    if (!is.null(parent) && assert_that(is.environment(parent)))
        parent.env(.Object@.xData) <- parent
    else
        parent.env(.Object@.xData) <- parent.frame(1L)
    assert_that(is.string(method.type))
    .Object@method.type <- method.type
    if (!is.list(methods)){
        methods <- if (is.environment(methods)) as.list(methods, all=TRUE) else as.list(methods)
        methods <- methods[!sapply(methods, is.environment)]
    }
    assert_that(all(sapply(methods, is.function)))
    if (length(methods)==0) return(.Object)
    else assert_that(rlang::is_dictionaryish(methods))

    if (is.null(method.parent)) method.parent <- .Object@.xData
    for (i in seq_along(methods)){
        method <- methods[[i]]
        if (!is(methods[[i]], method.type)) {
            name <- names(methods)[[i]]
            method <- new( method.type, method, name=name, ...)
        } else
            name <- method@name
        environment(method@.Data) <- method.parent
        assign(name, method, envir = .Object@.xData)
    }
    if (.lock) lockEnvironment(.Object@.xData, bindings = TRUE)
    return(.Object)
})
# __+ testing ####
if(FALSE){#@testing
    expect_is(lib <- new('MethodsLibrary'), 'MethodsLibrary')
    expect_true(validObject(lib, test=TRUE))

    parent <- s(new.env(), name = 'test methods parent')

    lib <- new('MethodsLibrary'
              , list(say_hi = function()cat('hi\n'))
              , method.parent = parent
              , .lock=FALSE
              )
    expect_length(ls(lib, all=TRUE), 1L)
    expect_identical(environment(lib$say_hi), parent)
    expect_true(validObject(lib, test=TRUE))

    expect_false(environmentIsLocked(lib))
    assign('.self', lib, envir=lib@.xData)
    expect_true(validObject(lib, test=TRUE))

    assign('say_goodby', 'goodby', envir=lib@.xData)
    expect_error( validObject(lib), "Element say_goodby is not a valid refMethodDef object")

    rm(say_goodby, envir = lib)
    copy <- new('MethodsLibrary'
               , methods = lib
               , method.parent = emptyenv()
               , parent = globalenv()
               )
    expect_identical(parent.env(copy), globalenv())
    expect_identical(environment(copy$say_hi), emptyenv())
    expect_false(exists('.self', copy))
    expect_true(environmentIsLocked(copy))
}
