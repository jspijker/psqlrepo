#' Delete all keys from objects
#'
#' This function deletes all meta data (keys and values) from an
#' object
#'
#' @param name name of object
#'
#' This function deletes al meta data from an
#' object. All keys and associated values are removed from the
#' database. Be carefull with this function since the deletion is
#' permanent and there is now way back
#'
#'
#' @export


pr_deleteKeyObj <- function(name) {


	if (!is.character(name)) {
		stop("pr_getKeyObj: name is not character")
	}

    if(!pr_objectExists(name)) {
		stop(paste("pr_getKeyObj: object",obj,"not found"))
    }

    keys <- pr_getKeyObj(name)
    for(i in names(keys)) {
        pr_deleteKey(name,i)
    }

}
