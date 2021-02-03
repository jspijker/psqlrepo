#' Test if keyword - value pairs are valid
#'
#' Meta data is stored in keyword-value pairs. This meta data can be
#' stored in a list. This function tests if this list is valid.
#'
#' @param kv list with keyword-value pairs
#' @param verbose be verbose and explain why test fails
#'
#' @return TRUE if list is valid, FALSE if otherwise
#'
#' Adding meta-data to an object is done by adding keyword-value
#' pairs to the object. When getting metadata using `pr_getKeyObj` a
#' list is returned with these keyword-value pairs. Such a list can
#' also be given to functions like pr_storeObj of pr_storeBlob
#'
#' A keyword-value can be created as follows:
#' list(keyword1="value1",keyword2="value2"). The names in the list
#' are the keywords. Each item in te list, the keyword, can only
#' contain one value and this value must be character.
#'
#' This fucntion tests if the keyword-value list is correct and that no empty
#' elements, numerical values et cetera, exists. If the test passed,
#' the return value will be TRUE. The return value will be FALSE if
#' otherwise.
#'
#' If the test fails, using the verbose option prints a explanation
#' why the test fails.
#'
#' If the kv is not a list, this function returns an error
#'
#' @export

pr_kvTest <- function(kv,verbose=FALSE) {

    if(!is.list(kv)) {
        stop("pr_kvTest: kv is not a list")
    }

    if(!is.logical(verbose)) {
        stop("pr_kvTest: verbose is not logical")
    }

    result <- TRUE

    # check names of kv
    kv.names <- names(kv)
    if(is.null(kv.names)) {
        result <- FALSE
        if (verbose) cat("kv list does not contain key values\n")
    }

    for (i in kv.names) {
        if(i=="") {
            result <- FALSE
            if (verbose) cat("kv list does not contain key values for key",i,"\n")
        }
        val <- kv[[i]]
        if(!is.character(val)) {
            result <- FALSE
            if (verbose) cat("kv listdoes not contain character value for key",i,"\n")
        }
        if(val=="") {
            result <- FALSE
            if (verbose)cat("kv list does not contain value for key",i,"\n")
        }
    }

    return(result)
}

