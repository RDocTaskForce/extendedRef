#' @include utils.R


# MethodsLibrary ----------------------------------------------------------
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
        ){
    .Object <- callNextMethod(.Object)
    assert_that(is.string(method.type))
    .Object@method.type <- method.type
    if (!is.list(methods)){
        methods <- as.list(methods)
        methods <- methods[!sapply(methods, is.environment)]
    }
    if (length(methods)==0) return(.Object)
    else assert_that(rlang::is_dictionaryish(methods))

    assert_that(all_inherit(methods, 'function'))
    for (i in seq_along(methods)){
        method <- methods[[i]]
        name <- names(methods)[[i]]
        if (!is(methods[[i]], method.type))
            method <- new(method.type, method, name=name)
        environment(method) <- method.parent
        assign(name, method, envir = .Object@.xData)
    }
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