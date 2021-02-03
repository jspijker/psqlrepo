#' Check if database table exists
#'
#' @param table name of table
#'
#' @return True if table exists, false otherwise
#'
#' This function checks if a table exists within the current psql
#' schema. If so, it returns True. If the table does not exists, it
#' returns false.
#'
#' Existence of the table is tested using a SELECT query on that table
#'
#' @export

pr_tableExists <- function(table){
	# function checks if table exists, returns TRUE if so, FALSE if
	# otherwise

	# check input
	if(!is.character(table)) {
		stop("table is not character")
	}

	s <- pr_getSchema()
    res<-pr_sql(paste("select * from ",s,".",table," limit 0",sep=''));

    if(is.data.frame(res)) {
        return(TRUE)
    }else {
        return(FALSE)
    }
}



