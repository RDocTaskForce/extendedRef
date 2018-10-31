insert_parent_env <- function(env, parent){
    env <- as.environment(env)
    parent <- as.environment(parent)
    old.parent <- parent.env(env)
    parent.env(parent) <- old.parent
    parent.env(env) <- parent
    invisible(parent)
}
if(FALSE){#@testing
    a <- new.env(parent = baseenv())
    b <- new.env(parent = emptyenv())

    expect_identical(parent.env(a), baseenv())
    expect_identical(parent.env(b), emptyenv())

    insert_parent_env(a, b)

    expect_identical(parent.env(a), b)
    expect_identical(parent.env(b), baseenv())
}



