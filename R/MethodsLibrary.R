#' @include utils.R


# MethodsLibrary ----------------------------------------------------------


#' @title Methods Library
#' @description
#' A methods library is a container for methods of a specific type.
#' All objects in the library should be functions of type `method.type`.
#' One exception is made for objects that points back to the 
#' library object, ie. a self referential object.
#' 
#' @slot method.type A string giving the type of method expected, 
#'                   the type should inherit from 'function', and 
#'                   preferrably from [refMethodDef][methods::refMethodDef-class].
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
#' @export
MethodsLibrary <- 
setClass( 'MethodsLibrary'
        , contains = 'environment'
        , slots = c(method.type = 'character')
        , prototype = list(method.type='refMethodDef')
        )
setValidity('MethodsLibrary', validity <- function(object){
  list.object <- as.list(object)
  are.valid <- are(list.object, object@method.type) &
               are_valid(list.object, simplify=TRUE)
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
setMethod('initialize', 'MethodsLibrary', initialize <-
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
    assert_that(all_inherit(methods, 'function'))
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
if(FALSE){#@testing 
    expect_is(lib <- new('MethodsLibrary'), 'MethodsLibrary')
    
    parent <- s(new.env(), name = 'test methods parent')
    
    trace('initialize', signature = 'MethodsLibrary', browser)
    lib <- new('MethodsLibrary'
              , list(say_hi = function()cat('hi\n'))
              , method.parent = parent
              )
    expect_length(ls(lib, all=TRUE), 1L)

    expect_identical(environment(lib$say_hi), parent)
}
