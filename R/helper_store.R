######################################################################
# helper functions for storing and deleting objects
######################################################################
# 
# These are internal functions wich are generic for storing objects of
# any type. For each type of object an user function is exported which
# takes care of the the object specific tasks. It's these exported
# functions the user should use.


pr_store <- function(obj,name,type,chunksize=7168,verbose=FALSE) {

    if(!is.character(name)) {
        stop("pr_store: name is not character")
    }

    if(!is.numeric(type)) {
        stop("pr_store: type is not numeric")
    }

    if(!is.numeric(chunksize)) {
        stop("pr_store: chunksize is not numeric")
    }

    if(!is.logical(verbose)) {
        stop("pr_store: verbose is not logical")
    }

	objstr<-pr_objToStr(obj)
    chunks<-pr_splitStr(objstr,chunksize);
    s <- pr_getSchema()
	hash <- digest::digest(obj)

    nextobj<-pr_sql(paste("select did from ",s,".did;",sep=''))$did;

    qry<-paste("insert into ",s,".robjects (did,name,hash,type) values (",
	    nextobj,",'",name,"','",hash,"',",type,");",sep='');
    res <- pr_sql(qry,verbose=verbose);

	if(!is.null(res)&&is.na(res)) {
		stop("query returned error")
	}

	if (verbose) cat("storing data: ",name,"\n");
	for (i in 1:length(chunks)) {
		str<-chunks[i];
		qry<-paste("insert into ",s,".rdata (did,object,chunk) values ('",
				   nextobj,"','",str,"',",i,");",sep='');
		pr_sql(qry,verbose=verbose);
	}
}

######################################################################
pr_get <- function(name) {

    if(!is.character(name)) {
        stop("pr_store: name is not character")
    }

	s <- pr_getSchema()
	if(!pr_objectExists(name)) {
		msg<-paste("pr_get: object",name,"not found, stop.")
		stop(msg)
	}

    did<-pr_sql(paste("select did from ",s,".robjects where name='",name,
	    "'",sep=''))$did

    qry<-paste("select chunk, object from ",s,".rdata where did=",
	    did," order by chunk",sep='')
    d<-pr_sql(qry)


    d<-pr_unSplitStr(d$object)
    d<-pr_strToObj(d)
    return(d)
}

######################################################################
pr_delete  <- function(name) {

	if(!is.character(name)) {
		stop("name is not character")
	}

    if(!pr_objectExists(name)) {
        return();
    }

	s <- pr_getSchema()
	did <- pr_getObjId(name)
    # TODO: implement
	# deleteKeyObj(name)

    qry<-paste("delete from ",s,".rdata where did=",did,sep='');
    qry<-paste(qry,";\n","delete from ",s,".robjects where name='",
	    name,"'",sep='');
    pr_sql(qry);
}



######################################################################
pr_getObjId <- function(name) {

	s <- pr_getSchema()
	res<-FALSE
	d<-pr_sql(paste("select id from ",s,".robjects where name='",
				 name,"'",sep=''))
	if (!is.na(d) && nrow(d)==1) {
		res <- d$id[1]
	} else {
		res <- NA
	}
	return(res)
}


######################################################################
pr_getObjHash <- function(name) {

	if(!is.character(name)) {
		stop("name is not character")
	}

	s <- pr_getSchema()

	res<-FALSE
	d<-pr_sql(paste("select hash from ",s,".robjects where name='",
				 name,"'",sep=''))
	if (!is.na(d) && nrow(d)==1) {
		res <- d$hash[1]
	} else {
		res <- NA
	}

	return(res)
}


######################################################################
pr_getObjType <- function(name) {

	if(!is.character(name)) {
		stop("name is not character")
	}

	s <- pr_getSchema()

	d<-pr_sql(paste("select type from ",s,".robjects where name='",
				 name,"'",sep=''))

	if (!is.na(d) && nrow(d)==1) {
		res <- d$type[1]
	} else {
		res <- NA
	}

    return(res)

}

