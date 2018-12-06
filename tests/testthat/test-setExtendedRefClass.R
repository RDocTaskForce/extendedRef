#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `setExtendedRefClass.R`')
#line 121 "R/setExtendedRefClass.R"
test_that('static const (ExtendedRefClass)  #####', {#@testing static const (ExtendedRefClass)  #####
    name <- "test with const"
    gen <- setExtendedRefClass( name
                              , static.const = list(element='function')
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass(name), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_is(gen$def@static.const, 'StaticConstEnv')
    expect_identical( environmentName(gen$def@static.const)
                    , 'test with const Static Const Environment'
                    )
    expect_identical(parent.env(classDef@static.const), globalenv())
    expect_identical( gen$static$const@.xData
                    , classDef@static.const@.xData
                    )

    expect_null(gen$def@private.library)
    expect_identical(gen$def@private.classes, character(0))
    expect_null(gen$def@static)
    expect_null(gen$def@static.methods)

    expect_is(gen$static, 'StaticTriad')
    expect_is(gen$static$const, 'StaticConstEnv')
    expect_identical(gen$static$const, gen$def@static.const)

    object <- gen()
    expect_identical(const <- parent.env(object), gen$def@static.const@.xData)
    expect_identical(environmentName(const), "test with const Static Const Environment")
    expect_identical(parent.env(const), globalenv())

    expect_identical(get('element', object), 'function')

    expect_true(removeClass("test with const"))
})
#line 156 "R/setExtendedRefClass.R"
test_that('static (ExtendedRefClass) #####', {#@testing static (ExtendedRefClass) #####
    name <- "test with static"
    gen <- setExtendedRefClass( name
                              , static = c(count='integer')
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass(name), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_equal(gen$def@className, s(name, package ='.GlobalEnv'))
    expect_is(gen$def@static, 'classStaticEnv')
    expect_identical(environmentName(gen$def@static), 'test with static Static Environment')

    expect_null(gen$def@private.library)
    expect_identical(gen$def@private.classes, character())
    expect_null(gen$def@static.const)
    expect_null(gen$def@static.methods)

    object <- gen()
    expect_identical(static <- parent.env(object), gen$def@static@.xData)
    expect_identical(environmentName(static), "test with static Static Environment")
    expect_identical(parent.env(static), globalenv())
    expect_identical(get('count', object), integer())

    expect_true(removeClass("test with static"))
})
#line 181 "R/setExtendedRefClass.R"
test_that('All Static (ExtendedRefClass) #####', {#@testing All Static (ExtendedRefClass) #####
    gen <- setExtendedRefClass( "test with all static"
                              , static.const = list(allowed = 'integer')
                              , static = c( count='integer'
                                          , n.resets = 'integer' )
                              , static.methods = list(reset = function(){count <<- 0L;n.resets <<- n.resets+1L})
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass("test with all static"), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_equal(gen$def@className, s('test with all static', package ='.GlobalEnv'))
    expect_is(gen$def@static.const  , 'StaticConstEnv')
    expect_is(gen$def@static        , 'classStaticEnv')
    expect_is(gen$def@static.methods, 'StaticMethods')
    expect_null(gen$def@private.library)
    expect_identical(gen$def@private.classes, character())

    expect_identical(environmentName(gen$def@static.const  ), 'test with all static Static Const Environment')
    expect_identical(environmentName(gen$def@static        ), 'test with all static Static Environment')
    expect_identical(environmentName(gen$def@static.methods), 'test with all static static methods library')

    object <- gen()
    expect_identical(static.methods <- parent.env(object        ), gen$def@static.methods@.xData)
    expect_identical(static         <- parent.env(static.methods), gen$def@static@.xData)
    expect_identical(static.const   <- parent.env(static        ), gen$def@static.const@.xData)
    expect_identical(                  parent.env(static.const  ), globalenv()                   )
    expect_identical(get('count', object), integer())

    expect_true(removeClass("test with all static"))
})
#line 211 "R/setExtendedRefClass.R"
test_that('with private (ExtendedRefClass) #####', {#@testing with private (ExtendedRefClass) #####
    gen <- setExtendedRefClass( "test with private"
                              , private = c(var='character')
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass("test with private"), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_equal(gen$def@className, s('test with private', package ='.GlobalEnv'))

    expect_null(gen$def@private.library)
    expect_identical(gen$def@private.classes, c(var = 'character'))
    expect_null(gen$def@static)
    expect_null(gen$def@static.const)
    expect_null(gen$def@static.methods)

    object <- gen()
    expect_is(object, 'ExtendedRefClass')
    expect_is(private <- get('private', object), 'TypedEnvironment')
    expect_identical(parent.env(object), private@.xData)
    expect_identical(parent.env(private), globalenv())
    expect_identical(ls(private, all=TRUE), c('private','var'))

    expect_true(removeClass("test with private"))
})
#line 235 "R/setExtendedRefClass.R"
test_that('private methods (ExtendedRefClass)  #####', {#@testing private methods (ExtendedRefClass)  #####
    gen <- setExtendedRefClass( "test with private methods"
                              , private = c(greeting = 'character')
                              , private.methods = list(say_hi = function()cat(greeting, '\n'))
                              , methods = list(set_greeting = function(greet)greeting<<-greet)
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass("test with private methods"), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_equal(gen$def@className, s('test with private methods', package ='.GlobalEnv'))

    expect_is(gen$def@private.library, 'privateMethodsLibrary')
    expect_equal(ls(gen$def@private.library), 'say_hi')
    expect_identical(gen$def@private.classes, c(greeting = 'character'))
    expect_null(gen$def@static)
    expect_null(gen$def@static.const)
    expect_null(gen$def@static.methods)

    object <- gen()
    expect_is(object, 'ExtendedRefClass')
    expect_is(private <- get('private', object), 'TypedEnvironment')
    expect_equal( environmentName(pm <- parent.env(object))
                , "object private methods")
    expect_identical(parent.env(pm), private@.xData)
    expect_identical(parent.env(private), globalenv())
    expect_equal(ls(private), c('greeting', 'private'))
    expect_equal(ls(pm, all=TRUE), 'say_hi')
    object$set_greeting("Hello")
    expect_error(object$say_hi())
    expect_output(get('say_hi', object)(), 'Hello')

    expect_true(removeClass("test with private methods"))
})
#line 268 "R/setExtendedRefClass.R"
test_that('static.const & private vars #####', {#@testing static.const & private vars #####
    gen <- setExtendedRefClass( "static & private"
                              , static.const = list(who = "Santa")
                              , private = c(greeting = 'character')
                              , private.methods = list(say_hi = function()cat(greeting, who, '\n'))
                              , methods = list(set_greeting = function(greet)greeting<<-greet)
                              , where = globalenv()
                              )
    expect_is(classDef <- getClass("static & private"), 'extendedRefClassDefinition')
    expect_is(gen$def, 'extendedRefClassDefinition')
    expect_identical(gen$def, classDef)
    expect_equal(gen$def@className, s('static & private', package ='.GlobalEnv'))

    object <- gen()
    expect_equal(environmentName(pm <- parent.env(object)), 'object private methods')
    expect_identical(parent.env(pm), (private <- get('private', object))@.xData)
    expect_identical(parent.env(private), classDef@static.const@.xData)

    expect_true(removeClass(gen@className))
})
#line 288 "R/setExtendedRefClass.R"
test_that('with all (ExtendedRefClass) #####', {#@testing with all (ExtendedRefClass) #####
    generator <-{
        setExtendedRefClass( Class = "testStaticClass"
                           , fields ={list( . = 'list'
                                          , count = function().count
                                          )}
                           , static ={c( .count = 'integer'
                                       , n.resets = 'integer'
                                       )}
                           , private = c(var = 'character')
                           , static.const = list(element='logical')
                           , methods ={list(
                              initialize = function(...). <<- list(...),
                              validate = function()assertthat::validate_that(all(sapply(., are, element))),
                              append = function(...){
                                  l <- list(...)
                                  pkgcond::assert_that(all(sapply(l, is, element)))
                                  . <<- c(., ...)
                                  invisible(.self)
                              }
                           )}
                           , private.methods = list(private_initialize = function(){
                                   .count <<- .count+1L
                               })
                           , static.methods ={list(
                              static_initialize = function(){
                                  if(!static.initialized) {
                                      .count <<- 0L
                                      n.resets <<- 0L
                                  }
                              },
                              reset = function(){
                                  .count <<- 0L
                                  n.resets <<- n.resets + 1L
                              }
                           )}
                           , where = globalenv()
                           # , validity = function(object)object$validate()
                           )}
    expect_is( def <- generator$def, 'extendedRefClassDefinition')
    expect_identical(getClass('testStaticClass'), def)
    expect_identical(def@private.classes, c(var = 'character'))
    expect_is(const  <- def@static.const  , 'StaticConstEnv')
    expect_is(static <- def@static        , 'classStaticEnv')
    expect_is(sm     <- def@static.methods, 'StaticMethods')
    expect_is(plib   <- def@private.library, 'privateMethodsLibrary')

    expect_identical(parent.env(def@refMethods), plib@.xData)
    expect_identical(parent.env(plib  ), sm@.xData)
    expect_identical(parent.env(sm    ), static@.xData)
    expect_identical(parent.env(static), const@.xData)

    expect_false(generator$static$static.initialized)

    object <- generator(TRUE, FALSE)
    expect_is(object, 'testStaticClass')
    expect_is(object, 'ExtendedRefClass')
    expect_is(object, 'envRefClass')

    expect_identical( environmentName(pm <- parent.env(object))
                    , "object private methods")
    expect_identical( environmentName(parent.env(pm))
                    , "object private variables")
    expect_identical( parent.env(pm), (private <- get('private', object))@.xData)
    expect_identical( parent.env(private), sm@.xData)
    expect_identical( parent.env(sm), static@.xData)
    expect_identical( parent.env(static), const@.xData)
    expect_identical( get('static', object), static)

    expect_true(static$static.initialized)

    expect_identical(static$.count, 1L)
    expect_identical(object$count, 1L)

    expect_identical(get('element', object), 'logical')
    expect_true(validObject(object, test=TRUE))

    expect_identical( object$append(FALSE, FALSE, TRUE)@.xData
                 , invisible(object)@.xData
                 )
    expect_true(validObject(object, test=TRUE))

    expect_error(object$append(0L), "Elements 1 of sapply\\(l, is, element\\) are not true")
    expect_length(object$., 5L)

    expect_true(removeClass(generator@className, where = generator@package))
})
#line 375 "R/setExtendedRefClass.R"
test_that('relocated initialize', {#@testing relocated initialize
    flag <- setClass('flag', contains='logical'
                    , validity = function(object)validate_that(length(object)==1)
                    , where = globalenv())
    setAs('logical', 'flag', function(from) flag(from), where = globalenv())
    expect_is(as(FALSE, 'flag'), 'flag')

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
                                  . <<- c(., ...)
                                  invisible(.self)
                              },
                              get = function(i).[[i]],
                              length = function()base::length(.)

                           )}
                           , private.methods ={list(
                               is_valid = function()see_if(sapply(., is, element))
                               )}
                           , where = globalenv()
                           )}
    def <- flags$def
    default.init <- s(function(..., verbose=getOption('verbose', FALSE)){
                        if (verbose)
                            message("Initialization delayed until all environments are created.")
                    }, srcref = NULL)

    expect_identical( ls(def@private.library, all=TRUE)
                    , c('.__initialize__.', '.private.methods.library', 'is_valid')
                    )
    # expect_identical( set_environment(def@refMethods$initialize@.Data, environment())
    #                 , default.init
    #                 )
    # expect_identical( set_environment(def@private.library$.__initialize__.@.Data, environment())
    #                 , init
    #                 )

    expect_identical(def@private.library$.__initialize__.@mayCall, 'append')


    expect_is(object <- flags(flag(TRUE), flag(FALSE)), 'Vector<flag>')
    expect_identical( ls(parent.env(object), all=TRUE)
                    , c('.__initialize__.', 'is_valid')
                    )
    expect_identical(environment(object$initialize@.Data), object@.xData)
    # expect_identical( set_environment(s(object$initialize@.Data, srcfile=NULL, srcref=NULL), object@.xData)
    #                 , set_environment(s(default.init           , srcfile=NULL, srcref=NULL), object@.xData)
    #                 )
    expect_identical( parent.env(object)$.__initialize__.@.Data
                    , set_environment(init, object)
                    )
    expect_identical( environment(parent.env(object)$.__initialize__.@.Data)
                    , object@.xData
                    )


    expect_identical(parent.env(object)$.__initialize__.@mayCall, 'append')
    expect_true(exists('append', object))
    expect_equal(object$length(), 2L)

    expect_message({withr::with_options( list(verbose=TRUE)
                                       , obj <- flags(TRUE, FALSE)
                                       )
                   }, "Initialization delayed until all environments are created.")

    expect_true(removeClass("Vector<flag>"))
})
