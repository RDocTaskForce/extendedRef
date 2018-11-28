make_class_optional <-
function( className
        , name = paste0('optional-', className)
        , where = topenv(parent.frame())
        ){
    setClassUnion(name, className, where=where)
    setIs('NULL', name, where=where)
}
if(FALSE){#@testing
    if (exists(classMetaName('test'), where=globalenv()))
        removeClass('test', where=globalenv())
    new_test <- setClass('test', representation = list(a='logical'), where=globalenv())
    make_class_optional('test', 'optional-test', where=globalenv())

    expect_true(exists(classMetaName('optional-test'), where=globalenv()))

    expect_true(is(NULL, 'optional-test'))

    expect_is(x <- new_test(a=TRUE), 'test')
    expect_true(is(x, 'optional-test'))

    expect_true(removeClass('optional-test', where=globalenv()))
    expect_true(removeClass('test', where=globalenv()))
}
