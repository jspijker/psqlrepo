#' Initialise psql repository
#' 
#' This function initialises the psql repository. It loads the
#' configuration, connects to the database and create a DBI connection
#' object.
#'
#' @param configfile filename of the configuration file
#' @param config name of configuration to read
#'
#' @return
#' TBD
#' 
#' This function also creates a database connection and stores the
#' handle in the package namespace. Only one connection is possible.
#' If pr_init is called while a database connection exists, the
#' current connection is closed and a new connection is created.
#'
#' @importFrom yaml read_yaml
#' @importFrom dbi dbConnect
#' @export


pr_init <- function(configfile="~/.psqlrepo.yml",configuration="default") {

    prinfo <- list(initSuccess=FALSE)

    cfg <- pr_readcfg(filename=configfile,
                      configuration)
    prinfo <- append(prinfo,list(cfg=cfg))

    if(pr_testConnection()){
        pr_closeConnection()
    }
    drv <- DBI::dbDriver("PostgreSQL")
    dbh <- try(DBI::dbConnect(drv,dbname=prinfo$cfg$dbname,
                              user=prinfo$cfg$dbuser,
                              host=prinfo$cfg$dbhost,
                              password=prinfo$cfg$dbpassw,
                              port=prinfo$cfg$dbport)
    )

    if(class(dbh)=="try-error") {
        stop("database connection failed")
    } else {
        prinfo$initSuccess <- TRUE
    }

    assign("dbhandle",dbh,envir=.psqlrepoConfig)
    assign("prinfo",prinfo,env=.psqlrepoConfig)

}


#' Check database connection
#' 
#' This function checks if a database connection exists
#'
#' @return
#' True, if connection exists, False otherwise
#' 
#' This function is used from other functions of this package. It
#' checks if a database connection exists and prints some information
#' about the connection. 
#'
#' @export

pr_testConnection <- function(verbose=FALSE) {
    handleExists <- FALSE
    res <- exists("dbhandle",where=.psqlrepoConfig)
    if(res){
        if(verbose) cat("handle exists: ")
        dbh <- get("dbhandle",envir=.psqlrepoConfig)
        coninfo <- try(RPostgreSQL::postgresqlConnectionInfo(dbh),silent=TRUE)

        if(class(coninfo)=="try-error") {
            if(verbose) cat("disconnected\n")
        } else {
            if(verbose) cat("connected to ",coninfo$host,"\n")
            handleExists <- TRUE
        }
    }
    return(handleExists)
}


#' Closes database connection
#' 
#' This function closes the current database connection
#'
#' @return
#' None
#' 
#' This function closes an existing database connection
#'
#' @export

pr_closeConnection <- function() {
    if(pr_testConnection()) {
    dbh <- get("dbhandle",envir=.psqlrepoConfig)
    RPostgreSQL::postgresqlCloseConnection(dbh)
    }
}
