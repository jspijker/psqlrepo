#' Get psqlrepo configuration
#' 
#' The configuration options are stored in a separate environment.
#' This function retrieves these options
#'
#' @return
#' A list with configuration options
#' 
#' This function is used from other functions of this package. It is
#' not intended to use directly. However, it can be useful in some
#' cases like debugging of the configuration. 
#'
#' @export

pr_getConfig <- function() {
    prinfo <- get("prinfo",env=.psqlrepoConfig)
    return(prinfo)
}


