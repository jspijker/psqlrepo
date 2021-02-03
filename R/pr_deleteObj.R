#' Delete a regular object form the repository
#'
#' This functions deletes a regular object from the repository,
#' including meta data. 
#'
#' @param name of te object to delete
#'
#' @return NULL if succesfull
#'
#' This function deletes stuf, but needs some more explaining in the
#' man-page
#'
#' @export

pr_deleteObj <- function(name) {

	if(!is.character(name)) {
		stop("name is not character")
	}

    if(!pr_objectExists(name)) {
		stop(paste("pr_getKeyObj: object",obj,"not found"))
    }

    s <- pr_getSchema()
    
	did <- pr_getObjId(name)
	pr_deleteKeyObj(name)

    qry<-paste("delete from ",s,".rdata where did=",did,sep="");
    qry<-paste(qry,";\n","delete from ",s,".robjects where name='",
	    name,"'",sep="");
    pr_sql(qry);




}

