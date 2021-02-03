#' Read psqlrepo configuration file
#' 
#' The psqlrepo configuration file is a YAML file with database
#' connection info. This function reads this file and returns a
#' configuration object.
#'
#' @param filename filename of the configuration file
#' @param config name of configuration to read
#'
#' @return
#' A configuration object, a list with the configuration options
#' 
#' This function is based on the config::get command from the config
#' package. While config::get is making assumptions about where to
#' find the configuration file and which configuration to read, the
#' pr_readcfg function requires that both filename and
#' configuration are given explicitly.
#' 
#'
#' @importFrom yaml read_yaml
#' @export


pr_readcfg  <- function(filename,configuration) {

    if(!is.character(filename)) {
        stop("pr:readcfg: filename is not character")
    }

    if(!is.character(configuration)) {
        stop("pr:readcfg: configuration is not character")
    }

    if(!file.exists(filename)) {
        stop(paste("pr:readcfg: file",filename, "does not exist"))
    }

    #cfg <- config::get(config=configuration,file=filename)
    cfg <- yaml::read_yaml(filename)

    if(is.null(cfg[[configuration]])) {
           stop(paste("pr_readcfg: configuration",configuration,"not found"))
    }

    return(cfg[[configuration]])

}

