######################################################################
# Cleanup after testing
######################################################################

message("excutimg teardown")
try(pr_closeConnection())

if(dbDoTests){
    message(paste("drop db",dbname))
    retval <- system2("dropdb",args=paste0("-h localhost -U postgres ",dbname))
    retval <- system2("dropuser", args=paste0("-h localhost -U postgres ",dbuser))
}

message(paste("remove config fileb",configfile))


