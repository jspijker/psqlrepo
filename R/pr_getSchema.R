#' Get database schema 
#'
#' Retrieves the database schema of the psqlrepo tables

#' @return string with database schema
#'
#'
#' @export


pr_getSchema <- function() {
    cfg <- pr_getConfig()
    schema <- cfg$cfg$dbschema
    return(schema)
}
