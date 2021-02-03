#' Adds a keyword - value list as meta data to an object
#'
#' Meta data is stored in keyword-value pairs. This meta data can be
#' stored in a list. This function adds the keyword - values in such
#' list to the meta data of an object
#'
#' @param name name of object
#' @param kv list with keyword-value pairs
#' @param overwrite such values be overwriten
#'
#' @return nothing
#'
#' Adding meta-data to an object is done by adding keyword-value
#' pairs to the object. For example, when getting metadata using `pr_getKeyObj` a
#' list is returned with these keyword-value pairs. 
#'
#' This function stores a list with keyword - value pairs as meta data
#' to an object. All keyword - value combinations in the list are
#' stored as seperate keyword - value combinations in the database.
#'
#' If a keyword allready exists, an error is returned. If overwrite is
#' TRUE, no error will be returned and the keyword will be
#' overwritten.
#'
#' A keyword-value list can be created as follows:
#' list(keyword1="value1",keyword2="value2"). The names in the list
#' are the keywords. Each item in te list, the keyword, can only
#' contain one value and this value must be character.
#'
#'
#' @export



pr_kvStore <- function(name,kv,overwrite=FALSE) {

    if(!pr_kvTest(kv)) {
        stop("kv is not valid")
    }

    if(!is.logical(overwrite)) {
        stop("pr_kvTest: verbose is not logical")
    }

    for (i in names(kv)) {
        pr_storeKey(name,key=i,val=kv[[i]],overwrite)
    }

}

