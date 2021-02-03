######################################################################
# Testing setup
######################################################################
message("executing setup")

# configuration options

dbname <- "psqlrepoTest"
dbuser <- "repotest"
dbschema <- "public"
dbschema2 <- "test"
dbpw <- paste(sample(c(LETTERS,letters),8),collapse="")
configfile <- tempfile()
filedir <- file.path(tempdir(),"files")
dir.create(filedir)
dbDoTests <- TRUE

#create config file with 2 configurations, default + psqlrepo:
f <- file(configfile,"w")
cat("default:\n",file=f)
cat("  dbhost: localhost\n",file=f)
cat("  dbport: 5432\n",file=f)
cat("  dbname:",dbname,"\n",file=f)
cat("  dbuser:",dbuser,"\n",file=f)
cat("  dbschema:",dbschema,"\n",file=f)
cat("  dbpasswd:",dbpw,"\n",file=f)
cat("  filedir:",filedir,"\n",file=f)
cat("\n",file=f)
cat("psqlrepo:\n",file=f)
cat("  dbhost: localhost\n",file=f)
cat("  dbport: 5432\n",file=f)
cat("  dbname:",dbname,"\n",file=f)
cat("  dbuser:",dbuser,"\n",file=f)
cat("  dbschema:",dbschema2,"\n",file=f)
cat("  dbpasswd:",dbpw,"\n",file=f)
cat("  test: testvalue\n",file=f)
close(f)

# create db
message("test for:",file.path(here::here(),"DoNotTestDatabase"))
if(file.exists(file.path(here::here(),"DoNotTestDatabase"))){
    message("file DoNotTestDatabase found, not testing database functions")
    dbDoTests <- FALSE
} else {
    message(paste("create db",dbname))
    retval <- system2(command="createdb", args=paste0("-h localhost -U postgres ",dbname))
    if(retval!=0) {
        dbDoTests <- FALSE
        message("creation of database failed, not testing db functions")
    } else {
        qry <- paste("create role ",dbuser," with superuser login password '",dbpw,"';",sep="")
        retval <- system2("psql",paste("-h localhost -U postgres -d ",dbname,"-c \"",qry,"\""))
        if(retval!=0) {
            message("could not create db user")
            dbDoTests <- FALSE
        }
    }

}


