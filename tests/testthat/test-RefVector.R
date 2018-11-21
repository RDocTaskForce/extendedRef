#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `RefVector.R`')
#line 95 "R/RefVector.R"
test_that('setRefVector', {#@testing
    test_vector <- setRefVector('logical')
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

    expect_true(removeClass(test_vector@className))
})
#line 134 "R/RefVector.R"
test_that('setRefVector', {#@testing
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

    expect_true(removeClass(refList@className))
})
#line 229 "R/RefVector.R"
test_that('setRefSet', {#@testing
    if (exists(classMetaName("test-element"), globalenv()))
        removeClass("test-element", globalenv())
    test_class <- setClass("test-element", list(name='character', age = 'numeric'), where = globalenv() )
    expect_is(elem <- test_class(name = 'object 1', age = 0L), 'test-element')

    test_set <- setRefSet( 'test-element'
                         , where = globalenv()
                         , static.methods = list(
                             equals = function(x, y) x@name == y@name
                            )
                         )

    expect_is(test_set, 'refObjectGenerator')

    my.set <- test_set()
    expect_is(my.set, 'ReferenceSet<test-element>')
    expect_length(my.set, 0L)

    expect_is(elem <- test_class(name = 'object 1', age = 0L), 'test-element')
    val <- my.set$add(elem)
    expect_equal(val, my.set)
    expect_length(my.set, 1L)

    expect_equal( body(get('equals', my.set))
                , rlang::expr(x@name == y@name)
                )

    expect_message( my.set$add(elem)
                  , class = "RefSet(Documentation)-message-already.contains"
                  )
    expect_message( my.set$add(test_class(name = 'object 1', age = 1L))
                  , class = "RefSet(Documentation)-message-already.contains"
                  )
    my.set$add(test_class(name='another'))
    expect_length(my.set, 2L)

    expect_true(removeClass("test-element", globalenv()))
    expect_true(removeClass("ReferenceSet<test-element>", globalenv()))
})
