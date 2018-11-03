#' @importClassesFrom methods NULL
make_class_optional <-
function( className
        , name = paste0('optional-', className)
        , where = topenv(parent.frame())
        ){
    setClassUnion(name, className)
    setIs('NULL', name)
}
