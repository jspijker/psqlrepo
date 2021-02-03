#' Get meta data keys and values from specific object
#'
#' This function retrieves the mete data keys from an object in the
#' repository
#'
#' @param name name of object
#'
#' @return a list with key-value pairs
#'
#' This function retrieves all the keys for an object and the
#' associated values. This information is returned as a list. The
#' names of this list are equal to the keys, the values are stored as
#' list values for each key.
#'
#' @export


pr_getKeyObj <- function(name) {

	# get specific key, value pair for object
	# obj: object name

	if (!is.character(name)) {
		stop("pr_getKeyObj: name is not character")
	}

    if(!pr_objectExists(name)) {
		stop(paste("pr_getKeyObj: object",obj,"not found"))
    }

	# get did
	did <- pr_getObjId(name)

	# schema
	s <- pr_getSchema()

	if(is.na(did)) {
		stop(paste("getKeyvalObj: object",obj,"not found"))
	}

	qry <- paste0("select key,value from ",s,".rkeyvalue where did=",
				 did,";")
	d<-pr_sql(qry)

    if(nrow(d)>0) {
        dl <- unlist(d$value)
        names(dl) <- d$key
        dl <- as.list(dl)
    } else {
        dl <- list()
    }

    return(dl)

}
