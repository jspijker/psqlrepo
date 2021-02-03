#' Store an regular object into the repository
#'
#' This functions stores an regular object into the repository. Such
#' object can be any R object from memory
#'
#' @param object object te store in repository
#' @param name store object using thisname
#' @param kv meta data list with keyword - value pairs
#' @param overwrite overwrite object if exists
#' @param verbose give some verbose output
#'
#' @return NULL if succesfull
#'
#' Any object existing in memory can be stored as object into the
#' psqlrepo repository. So you can store variables, data.frames,
#' functions etc. Some objects, like connections, can be stored but
#' this is less useful. Object which store data in external files,
#' like rasters or datatable objects, can't be stored into the
#' repository. For storing such objects the pr_storeBlob command can
#' be used. The name of the object can be any valid string.
#'
#' TODO: explain something about kv
#'
#' If an object allready exists and overwrite is FALSE this function
#' will return an error. If overwrite is TRUE then first it is checked
#' if the object exists and is equal to the object which will be
#' written, if so, the object is not writen to the database. If the
#' object to be written differs from the object in the database, the
#' object in the database is deleted and the new object is written.
#'
#'
#' @export


pr_storeObj <- function(object,name, kv=NULL, overwrite=FALSE,verbose=FALSE) {

	if(!is.character(name)) {
		stop("name is not character")
	}

	if(!is.logical(verbose)) {
		stop("verbose is not logical")
	}

	if(!is.logical(overwrite)) {
		stop("overwrite is not logical")
	}

    if(!pr_testConnection()){
        stop("no database connection")
    }

    if(!is.null(kv)) {
        if(!pr_kvTest(kv)) {
            stop("kv not valid")
        }
    }

	s <- pr_getSchema()
	hash <- digest::digest(object)
    	
    # test if object already exists
	if (pr_objectExists(name)) {
		if(overwrite) {
			dbhash <- pr_getObjHash(name)
			if(dbhash==hash) {
				if (verbose) cat("not storing data, object already in database\n")
				return()
			} else {
				pr_delete(name)
			}
		} else {
			stop(paste("pr_storeobj: object",name,
                       " already exists in database and overwrite=FALSE"))
		}
	}

    pr_store(name=name,obj=object,type=1,verbose=verbose)
    if(!is.null(kv)) pr_kvStore(name,kv) 
}
