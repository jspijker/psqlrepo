#' Store meta data key and value in repository
#'
#' Meta data is stored as key,value pairs in the repository, This
#' function stores a key, value pair for a specific object
#'
#' @param name Name of the object.
#' @param key Key.
#' @param value Value, must be of type character
#' @param overwrite If true, overwrite existing keys
#'
#'
#' @return nothing
#'
#' Meta data is stored as key, value pairs. This function associates a
#' key, value pair with the given object (objectname). Each object
#' can have an unlimited number of keys and it is up to the user to
#' organise the keys and possible values,
#'
#' Values are always stored as character strings, so numeric values
#' must by changed into strings before they can be stored.
#'
#' If a key allready exists then this function results in an error,
#' unless overwrite=TRUE
#'
#' @export

pr_storeKey <- function(name,key,value,overwrite=FALSE) {

	if (!is.character(name)) {
		stop("pr_storekey: name is not character")
	}

	if (!is.character(key)) {
		stop("pr_storekey: key is not character")
	}

	if (!is.character(value)) {
		stop("pr_storekey: value is not character")
	}

	if (!is.logical(overwrite)) {
		stop("pr_storekey: overwrite is not character")
	}

    if(!pr_objectExists(name)) {
		stop("pr_storekey: object does not exists")
	}


	# schema
    s <- pr_getSchema()

	# get object id
	did <- pr_getObjId(name)

	# check if key exists
	qry <- paste ("select key,value from ",s,".rkeyvalue where did=",did,
				  "and key='",key,"';",sep='');
	d <- pr_sql(qry)
    if(nrow(d)==0) {
        qry <- paste("insert into ",s,".rkeyvalue (did,key,value) values ('"
                     ,did,"','",key,"','",value,"');",sep='')
        pr_sql(qry)
    } else {
        if(overwrite) {
            qry <- paste("update ",s,".rkeyvalue set value='",value,
                         "' where did=",did," and key='",key,"';",sep='')
            pr_sql(qry)
        } else {
            stop(paste("key",key, "already exists and overwrite=FALSE"))
        }
    }
} 



