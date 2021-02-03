#' Get meta data key and values from object in  repository
#'
#' Meta data is stored as key,value pairs in the repository, This
#' function retrieves a key for a specific object
#'
#' @param name Name of the object.
#' @param key Key.
#'
#' @return A character object with the value of the key, 
#'
#' Meta data is stored as key, value pairs. This function associates a
#' key, value pair with the given object (objectname). Each object
#' can have an unlimited number of keys and it is up to the user to
#' organise the keys and possible values,
#'
#' Values are always stored as character strings, so numeric values
#' must by changed into strings before they can be stored.
#'
#' This function retrieves the value for an existing key from an
#' object stored in the repository. If the object or key does not
#' exists, it returns an error.
#'
#' @export


pr_getKey <- function(name,key) {

	if (!is.character(name)) {
		stop("pr_getkey: name is not character")
	}

	if (!is.character(key)) {
		stop("pr_getkey: key is not character")
	}


    if(!pr_objectExists(name)) {
		stop("pr_getkey: object does not exist")
    }

	# get did
	did <- pr_getObjId(name)

	# schema
	s <- pr_getSchema()

	qry <- paste("select value from ",s,".rkeyvalue where did=",
				 did," and key='",key,"';",sep='')
	d<-pr_sql(qry)
    if(is.na(d)) {
		stop("pr_getkey: key does not exist")
    }


    d <- d$value[1]
	return(d)

}

