#' Creates the psqlrepo database tables
#'
#' This function creates the necesarry psqlrepo database tables within
#' the current psqlrepo schema. It returns an error if the schema does not exists
#'
#' @param delete if true, existing tables are deleted
#'
#' @return NULL
#'
#' @export


pr_createTables <- function(delete=FALSE) {

	if(!is.logical(delete)) {
		stop("delete is not logical")
	}

    schema <- pr_getSchema()
    # test if schema exists
    res <- pr_sql(paste("SELECT schema_name FROM information_schema.schemata WHERE schema_name = '",
                        schema,"';",sep=''))

    if(nrow(res)==0) {
        stop(paste("database schema",schema,"does not exists"))
    }


	if(pr_tableExists("robjects")){
		if(delete) {
			pr_dropTables()
		} else {
			stop("Tables already exists and delete!=TRUE")
		}
	}



	createqry<-"
	begin;

	CREATE SEQUENCE public.robjects_seq;
	CREATE TABLE public.robjects (
								  id              integer PRIMARY KEY DEFAULT nextval('public.robjects_seq'),
								  did             integer UNIQUE,
								  name            varchar(250) UNIQUE NOT NULL,
								  hash            varchar (40) NOT NULL,
								  time            timestamp DEFAULT NOW(),
								  type            integer
								  );

	CREATE SEQUENCE public.rdata_seq;
	CREATE TABLE public.rdata (
							   id      integer PRIMARY KEY DEFAULT nextval('public.rdata_seq'),
							   did     integer REFERENCES public.robjects(did),
							   chunk   integer,
							   object  text
							   );

	CREATE SEQUENCE public.did_seq;
	CREATE VIEW public.did as SELECT nextval('public.did_seq') as did;


	CREATE SEQUENCE public.rkeyvalue_seq;
	CREATE TABLE public.rkeyvalue (
								  id            integer PRIMARY KEY DEFAULT nextval('public.rkeyvalue_seq'),
								  did			integer REFERENCES public.robjects(did),	
								  key			varchar(40) NOT NULL,
								  value			varchar(250) NOT NULL
								  );
	commit;
	"

	qry<-gsub("public",schema,createqry)
	qry<-gsub("\t"," ",qry)
	qry<-gsub("\n"," ",qry)
	qry <- gsub(" +"," ",qry,perl=TRUE)

	res <- pr_sql(qry)

	if(!is.null(res)) {
		res <- pr_sql("abort")
		stop("Creating of psqlrepo tables failed")
	}
    invisible(NULL)

}




#' Drop the psqlrepo database tables
#'
#' This function drops the psqlrepo database tables within
#' the current psqlrepo schema. It returns an error if the schema or
#' tables donot exists
#'
#' @return NULL
#'
#' @export


pr_dropTables <- function() {

    if(!pr_tableExists("robjects")) {
        stop("psqlrepo tables not found")
    }
    
    schema <- pr_getSchema()


	dropqry<-"
	drop TABLE public.rkeyvalue CASCADE;
	drop TABLE public.rdata CASCADE;
	drop TABLE public.robjects CASCADE;
    drop VIEW public.did;
	drop SEQUENCE public.robjects_seq CASCADE;
	drop SEQUENCE public.rdata_seq CASCADE;
	drop SEQUENCE public.rkeyvalue_seq CASCADE;
	drop SEQUENCE public.did_seq CASCADE;
	"

	qry<-gsub("public",schema,dropqry)
	qry<-gsub("\t"," ",qry)
	qry<-gsub("\n"," ",qry)
	qry <- gsub(" +"," ",qry,perl=TRUE)
	pr_sql(qry,verbose=FALSE)

}


