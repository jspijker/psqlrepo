#' Check if a key exists for specific object.
#'
#' This function checks if a key exists for a specific object. 
#'
#'
#' @param name Name of the object.
#' @param key Key.
#'
#' @return TRUE if key exists, FALSE otherwise
#'
#'
#' @export
#'

pr_keyExists <- function(name,key) {

	if (!is.character(name)) {
		stop("pr_keyExists: name is not character")
	}

	if (!is.character(key)) {
		stop("pr_keyExists: key is not character")
	}

    if(!pr_objectExists(name)) {
		stop("pr_getkey: object does not exist")
    }

	# get did
	did <- pr_getObjId(name)

	# schema
	s <- pr_getSchema()

	qry <- paste0("select value from ",s,".rkeyvalue where did=",
				 did," and key='",key,"';")
	d<-pr_sql(qry)

    if(length(d)==0) {
		return(FALSE)
    } else {
        return(TRUE)
    }



}
