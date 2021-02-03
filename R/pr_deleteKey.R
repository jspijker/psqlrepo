#' Delete meta data key for an object
#'
#' Deletes a meta data key, and value, for an given object
#'
#' @param name name of object
#' @param key name of key
#'
#' This function deletes a key for a given object. Both key and value
#' are removed from the database. 
#'
#' @return TRUE if successfull, FALSE otherwise
#'
#'


pr_deleteKey <- function(name,key) {

	if (!is.character(name)) {
		stop("pr_deleteKey: name is not character")
	}

	if (!is.character(key)) {
		stop("pr_deleteKey: key is not character")
	}

	s <- pr_getSchema()
	did <- pr_getObjId(name)
	# check if key exists
	x <- pr_keyExists(name,key)
	if(!x) {
		warning(paste("key",key,"does not exists"))
	} else {
		qry <- paste0("delete from ",s,".rkeyvalue where key='",
					 key, "' and did=",did,";")
					 pr_sql(qry)
	}
}

