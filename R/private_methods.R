#' @include utils.R
#' @include MethodsLibrary.R



# PrivateMethod -----------------------------------------------------------
setClass('PrivateMethod', contains = 'refMethodDef')


# privateMethodsLibrary  --------------------------------------------------
privateMethodsLibrary <-
  setClass( 'privateMethodsLibrary'
          , contains='MethodsLibrary'
          , prototype = list(method.type='PrivateMethod')
          )
setMethod('initialize', 'privateMethodsLibrary', initialize <-
function( .Object
        , methods=list()
        , parent=NULL
        , className=NULL
        , .lock=TRUE
        ){
    .Object <- callNextMethod(.Object, methods=methods, method.type='PrivateMethod')
    if (!is.null(parent) && assert_that(is.environment(parent)))
        parent.env(.Object@.xData) <- parent
    
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


# insert_private_methods_library ------------------------------------------
setGeneric('insert_private_methods_library', function(Def, lib)._not_implemented(Def))
setMethod('insert_private_methods_library'
          , c('refClassRepresentation', 'privateMethodsLibrary'), 
          function(Def, lib){
              insert_parent_env(Def@refMethods, lib@.xData)
          })
if(FALSE){#@testing
    Def <- setRefClass('test')$def
    old.parent <- parent.env(Def@refMethods)
    
    lib <- privateMethodsLibrary()
    insert_private_methods_library(Def, lib)
    expect_identical(parent.env(Def@refMethods), lib@.xData)
    expect_identical(parent.env(lib@.xData), old.parent)
    expect_identical(lib$.private.methods.library, lib)
    
    expect_true(removeClass(Def@className))
}


# has_private_methods_library ---------------------------------------------
setGeneric('has_private_methods_library', ._not_implemented)
setMethod( 'has_private_methods_library'
           , 'refClassRepresentation'
           , function(object){
               public.methods <- object@refMethods
               methods.parent <- parent.env(public.methods)
               
               !isNamespace(methods.parent) &&
                   exists('.private.methods.library', methods.parent, inherits = FALSE) &&
                   is( get('.private.methods.library', methods.parent, inherits = FALSE)
                       , 'privateMethodsLibrary')
           })
if(FALSE){#@testing
    Def <- setRefClass('test')$def
    lib <- privateMethodsLibrary()
    insert_private_methods_library(Def, lib)
    
    expect_true(has_private_methods_library(Def))    
    
    expect_true(removeClass(Def@className))
}

# get_private_methods_library ---------------------------------------------
setGeneric('get_private_methods_library', ._not_implemented)
setMethod('get_private_methods_library', 'refClassRepresentation',
          function(object, ...){
              if (!has_private_methods_library(object))
                  stop(class(object), 'does not have private methods.')
              get('.private.methods.library', parent.env(object@refMethods))
          })
setMethod('get_private_methods_library', 'refClass',
          function(object, ...){
              get_private_methods_library(object$.refClassDef)
          })
setMethod('get_private_methods_library', 'refObjectGenerator',
          function(object, ...){
              get_private_methods_library(object$def)
          })
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    expect_is(classDef, "refClassRepresentation")
    expect_false(has_private_methods_library(classDef))
    private.library <- privateMethodsLibrary( className = test_class@className
                                              , methods = list(hw=function()print('hello world'))
                                              , .lock=TRUE)
    insert_private_methods_library(classDef, private.library)
    expect_true(has_private_methods_library(classDef))
    
    test.obj <- test_class()
    
    expect_identical(get_private_methods_library(classDef), private.library)
    expect_identical(get_private_methods_library(test.obj), private.library)
    expect_identical(get_private_methods_library(test_class), private.library)
    
    expect_true(removeClass(test_class$def@className))
}

# objectPrivateMethods ----------------------------------------------------
private_methods <- setClass('objectPrivateMethods', contains='MethodsLibrary')
setMethod('initialize', 'objectPrivateMethods', initialize <-
              function( .Object   #< the private methods container
                        ,  thing    #< the object the private methods are associated with
              ){
                  assert_that(is(thing, "refClass"))
                  library <- get_private_methods_library(thing)
                  thing.env <- thing@.xData
                  
                  .Object <- callNextMethod( .Object
                                             , methods=library
                                             , method.type = 'PrivateMethod'
                                             , method.parent = thing.env
                  )
                  insert_parent_env(thing.env, .Object@.xData)
                  
                  attr(.Object@.xData, 'name') <- "object private methods"
                  
                  # assign('private', .Object, envir = .Object)
                  lockEnvironment(.Object, bindings = TRUE)
                  return(.Object)
              })
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    library <- privateMethodsLibrary()
    insert_private_methods_library(test_class$def, library)
    test.obj <- test_class()
    .Object <- private_methods(test.obj)

    expect_length(ls(.Object, all=TRUE), 1L)
    expect_identical(ls(.Object, all=TRUE), 'private')
    expect_true(removeClass(test_class$def@className))
}
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    expect_is(classDef, "refClassRepresentation")
    expect_false(has_private_methods_library(classDef))

    private.library <- privateMethodsLibrary( className= test_class@className
                                            , list(hw=function()print('hello world'))
                                            , .lock=TRUE)
    insert_private_methods_library(classDef, private.library)
    expect_true(has_private_methods_library(classDef))

    test.obj <- test_class()
    expect_identical(get_private_methods_library(test.obj), private.library)
    
    test.methods <- private_methods(test.obj)
    expect_is(test.methods, 'objectPrivateMethods')
    expect_true(environmentIsLocked(test.methods))
    expect_identical(parent.env(test.obj), as.environment(test.methods))
    expect_identical(test.methods, test.methods$private)
    expect_identical(test.methods, get('private', test.obj))

    expect_equal( ls(test.methods, all=TRUE), c('hw', 'private'))
    expect_identical(environment(test.methods$hw), test.obj@.xData)

    expect_true(removeClass(test_class$def@className))
}

# get_object_private_methods ----------------------------------------------
get_object_private_methods <- function(object){
    if (has_private_methods_library(object$.refClassDef)) {
        env <- as.environment(object)
        while ( !isNamespace(env)
             && !identical(env, .BaseNamespaceEnv)
             && !identical(env, emptyenv())
              ){
            if (environmentName(env) == 'object private methods')
                return(env$private)
            env <- parent.env(env)
        }
    }
}
if(FALSE){#@testing
    test_class <- setRefClass('test-class')
    classDef <- test_class$def
    private.library <- privateMethodsLibrary( className = test_class@className
                                            , list(hw=function()print('hello world'))
                                            , .lock=TRUE)
    insert_private_methods_library(classDef, private.library)

    test.obj <- test_class()
    test.methods <- private_methods(test.obj)

    expect_identical(get_object_private_methods(test.obj), test.methods)

    expect_true(removeClass(test_class$def@className))
}



