#' @include utils.R
#' @include MethodsLibrary.R



# PrivateMethod -----------------------------------------------------------


#' @title Private Methods Library
#' 
#' @description 
#' Private methods are functions that are intended for internal use.
#' They do not appear when listing the contents of a reference object.
#' They are available to the publid methods to call but cannot be accessed
#' through the `object$method()` mechanism that public methods are.
#' 
#' Private methods are stored in a `privateMethodsLibrary` object
#' when stored as part of a class definition.  They are converted to
#' a `objectPrivateMethods` object when attached to a specific instance.
#' 
#' @inheritParams MethodsLibrary
#' @param thing an object of class `className`.
#' @param library a privateMethodsLibrary object.
#'
setClass('PrivateMethod', contains = 'refMethodDef'
        , slots = c( mayCall.private = 'character'
                   , mayCall.static  = 'character'
                   ))
setInitialize('PrivateMethod', 
    function( .Object
            , ...
            , className = character()
            , public.methods = character()
            , private.methods = character()
            , static.methods = character()
            ){
    check_entry <- function(l, name = deparse(substitute(l))) 
        if (is.null(l)) character(0) else
        if (is.list(l)) names(0) else
        if (is.environment(l)) ls(l, all.names = TRUE) else
        if (assert_that(is.character(l)
                       , msg = paste(name, "is not a character"))) l
        
    public.methods <- check_entry(public.methods)
    private.methods <- check_entry(private.methods)
    static.methods <- check_entry(static.methods)

    called <- codetools::findGlobals(.Object, FALSE)$functions
    callNextMethod( .Object
                  , ...
                  , refClassName = className
                  , mayCall         = intersect(called,  public.methods)
                  , mayCall.private = intersect(called, private.methods)
                  , mayCall.static  = intersect(called,  static.methods)
                  )

})
if(FALSE){#@testing
    flag <- setClass('flag', contains='logical'
                    , validity = function(object)validate_that(length(object)==1))
    setAs('logical', 'flag', function(from) flag(from))
    init <- function(...)append(...)
    flags <-{
        setExtendedRefClass( Class = "Vector<flag>"
                           , private = c( . = 'list')
                           , static.const = list(element='flag')
                           , methods ={list(
                              initialize = init,
                              validate = function()validate_that(is_valid()),
                              append = function(...){
                                  l <- list(...)
                                  for (i in seq_len(nargs())) {
                                      if (!is(l[[i]], element))
                                          l[[i]] <- as(l[[i]], element)
                                  }
                                  browser()
                                  . <<- c(., ...)
                                  invisible(.self)
                              },
                              get = function(i).[[i]],
                              length = function()base::length(.)
                              
                           )}
                           , private.methods ={list(
                               is_valid = function()see_if(all_inherit(., element))
                               )}
                           , where = globalenv()
                           )}

    fun <- function(...)append(...)
    method <- new('PrivateMethod', fun, name='test', className = 'test-class'
                 , public.methods = 'append'
                 )    
    expect_is(method, 'PrivateMethod')
    expect_is(method, 'refMethodDef')
    expect_identical(method@mayCall, 'append')
} 




# privateMethodsLibrary  --------------------------------------------------
#' @rdname PrivateMethod-class
privateMethodsLibrary <-
  setClass( 'privateMethodsLibrary'
          , contains='MethodsLibrary'
          , prototype = list(method.type='PrivateMethod')
          )
setMethod('initialize', 'privateMethodsLibrary', initialize <-
function( .Object
        , methods = list()  
        , ...
        , className=NULL
        , .lock=TRUE
        ){
    .Object <- callNextMethod( .Object
                             , methods = methods  
                             , ...
                             , private.methods = names(methods)
                             , method.type='PrivateMethod'
                             , .lock = FALSE
                             )
    .Object$.private.methods.library <- .Object
    attr(.Object@.xData, 'name') <- if(is.null(className)) 'private methods library' else
        paste(className, 'private methods library')
    if (.lock) lockEnvironment(.Object@.xData, bindings = TRUE)
    return(.Object)
})
if(FALSE){#@testing
    bare <- privateMethodsLibrary()
    expect_is(bare, 'privateMethodsLibrary')
    expect_identical(ls(bare, all=TRUE), '.private.methods.library')
    expect_identical(environmentName(bare), "private methods library")
    
    w.methods <- privateMethodsLibrary(methods = list(say_hello=function()cat('hello\n')))
    expect_is(w.methods, 'privateMethodsLibrary')
    expect_identical( ls(w.methods, all=TRUE)
                      , c('.private.methods.library', 'say_hello'))
    expect_identical(environmentName(w.methods), "private methods library")
    expect_identical( environment(w.methods$say_hello)
                      , as.environment(w.methods))
    
    dne <- privateMethodsLibrary(className = "Does not exist")
    expect_identical(environmentName(dne), "Does not exist private methods library")
}
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    expect_false(has_private_methods_library(classDef))
    
    private.library <- privateMethodsLibrary( className = test_class@className
                                              , methods = list(hw=function()print('hello world'))
                                              , .lock=TRUE)
    expect_is(private.library, 'privateMethodsLibrary')
    expect_identical( environmentName(private.library)
                      , "test-class private methods library"
    )
    expect_true(environmentIsLocked(private.library))
    
    expect_true(exists('hw', private.library))
    expect_identical(environment(private.library$hw), as.environment(private.library))
    
    # expect_true(has_private_methods_library(test_class$def))
    
    # expect_warning( lib2 <- privateMethodsLibrary( test_class )
    #               , "test-class already has a private methods library defined")
    # expect_identical(lib2, private.library)
    
    expect_true(removeClass(test_class$def@className))
}



# objectPrivateMethods ----------------------------------------------------
#' @rdname PrivateMethod-class
private_methods <- setClass('objectPrivateMethods', contains='MethodsLibrary')
setMethod('initialize', 'objectPrivateMethods', initialize <-
              function( .Object   #< the private methods container
                      ,  thing    #< the object the private methods are associated with
                      , library = NULL
                      ){
                  assert_that(is(thing, "refClass"))
                  if (is.null(library))
                      library <- thing$.refClassDef@private.library
                  assert_that(is(library, 'privateMethodsLibrary'))
                  thing.env <- thing@.xData
                  
                  .Object <- callNextMethod( .Object
                                           , methods=library
                                           , method.type = 'PrivateMethod'
                                           , method.parent = thing.env
                                           )
                  insert_parent_env(thing.env, .Object@.xData)
                  
                  attr(.Object@.xData, 'name') <- "object private methods"
                  
                  lockEnvironment(.Object, bindings = TRUE)
                  return(.Object)
              })
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    library <- privateMethodsLibrary()
    test.obj <- test_class()
    .Object <- private_methods(test.obj, library)

    expect_identical(ls(.Object, all=TRUE), character())
    expect_true(removeClass(test_class$def@className))
}
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    expect_is(classDef, "refClassRepresentation")
    
    private.library <- privateMethodsLibrary( className= test_class@className
                                            , list(hw=function()print('hello world'))
                                            , .lock=TRUE)
    test.obj <- test_class()
    test.methods <- private_methods(test.obj, private.library)
    
    expect_is(test.methods, 'objectPrivateMethods')
    expect_true(environmentIsLocked(test.methods))
    expect_identical(parent.env(test.obj@.xData), as.environment(test.methods))

    expect_equal( ls(test.methods, all=TRUE), c('hw'))
    expect_identical(environment(test.methods$hw), test.obj@.xData)

    expect_true(removeClass(test_class$def@className))
}
if (FALSE) {#@testing private_methods with .
    Class <- "test with ."
    gen <- setRefClass(Class, c(.='list'))
    library <- 
        privateMethodsLibrary( className= Class
                             , list( hw=function()print('hello world')
                                   , .__initialize__. = function(...)
                                       . <<- list(...)
                                   )
                             , .lock=TRUE)
    obj <- gen()
    methods <- private_methods(obj, library)
    
    expect_identical(ls(methods, all=TRUE), c('.__initialize__.', 'hw'))
}
