#' SQL wrapper function
#'
#' Wrapper function to execute sql queries on a postgres database
#'
#' @param query The SQL query
#' @param verbose If true give verbose output
#' @param errors If True give errors as warning
#' @param dbhandle The database Handle, see #' \code{\link{PgObjectsInit}}
#'
#' @return the result of the SQL query as a data.frame or NULL in case of
#' a non SELECT query. If the query results in an error, NA is
#' returned.

#' this function is the workhorse of the psqrepo package, it runs
#' sql queries at the RDBMS and returns the result. This function
#' tries to catch any error and then returns NA. It is up to the user 
#' to test te result of the function.
#'
#' @export



pr_sql <- function(query,verbose=FALSE,errors=TRUE) {

	w <- simpleError("meh")

# 
# 	# check dbhandle
# 	if(is.na(dbhandle)) {
# 		dbhandle=getOption("pgobject.dbhandle")
# 		if(is.null(dbhandle)) {
# 			stop("option pgobject.dbhandle does not exist (forgot
# 				PgobjectsInit?)")
# 		}
# 	} else {
# 		if (!is.character(dbhandle)) {
# 			stop("dbhandle is not character")
# 		}
# 	}
# 
# 
# 	# make global dbhandle local
# 	ldbh<-eval(parse(text=dbhandle))

    dbh <- get("dbhandle",envir=.psqlrepoConfig)

	# check input, barf if necessary
	if (!is.character(query)) {
		stop("query is not character")
	}

# 	if (!is.numeric(dbGetInfo(ldbh)$backendPId)) {
# 		stop("dbhandle is not valid")
# 	}

	if (!is.logical(verbose)) {
		stop("verbose is not logical")
	}
	if (!is.logical(errors)) {
		stop("error is not logical")
	}

	if (verbose) {
		cat("Executing SQL: ",query,"\n")
	}

	#if query returns error we return 0
	qry <- try(RPostgreSQL::dbSendQuery(dbh,query),silent=TRUE)

    if (class(qry)=="try-error") {
        # query returned error
        if (verbose) warning(paste(qry))
        return(NA)
    } 

	res <- try(RPostgreSQL::fetch(qry,n=-1),silent=TRUE)

    if (class(res)=="try-error") {
		# query is empty
		return(NULL)
	} 
	
	invisible(res)


}


