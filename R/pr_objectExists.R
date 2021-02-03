#' Check if object exists in database
#'
#' This functions  checks if an object exists in the repository, if
#' so, it returns TRUE.
#'
#' @param objectname name of object
#'
#' @return TRUE if object exists, FALSE otherwise
#'
#' @export

pr_objectExists <- function(name) {

	if(!is.character(name)) {
		stop("name is not character")
	}

	s <- pr_getSchema()

	res<-FALSE
	d<-pr_sql(paste("select name from ",s,".robjects where
				 name='",name,"'",sep=''))
	if (nrow(d)>=1) {
		res <- TRUE
	}
	return(res)
}


