#' Get an object from the repository
#'
#' This functions gets an regular object from the repository and
#' returns it
#'
#' @param name store object using thisname
#' @param verbose give some verbose output
#' @param ignoretype ignore the object type
#'
#' @return The R object as stored in the repository
#'
#' This function gets an regular object from the database. The object
#' is returned and is equal as the object when it was stored
#' So if you store a list in the repository,
#' this function wil return that list.
#'
#' In the repository different types of objects can be stored, like
#' regular objects, blobs or audit objects. This function checks the
#' type of the object and returns an error if the object is not of a
#' regular type. When ingnoretype=FALSE this type checking is skipped.
#' This is useful if you want to inspect an object for e.g. debugging.
#'
#' @export


pr_getObj <- function(name,verbose=FALSE,ignoretype=FALSE) {

    if(!is.character(name)) {
        stop("pr_store: name is not character")
    }

    if(!is.logical(verbose)) {
        stop("pr_store: verbose is not logical")
    }

    if(!is.logical(ignoretype)) {
        stop("pr_store: ignoretype is not logical")
    }

    if(!pr_testConnection()){
        stop("no database connection")
    }

    res <- pr_get(name)

    return(res)

}
