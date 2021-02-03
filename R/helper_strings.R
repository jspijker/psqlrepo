######################################################################
# functions to serialize and encode objects into strings, and vice
# versa
######################################################################

pr_objToStr <- function(obj) {
    str<-serialize(obj,NULL,ascii=TRUE);
    str<-base64enc::base64encode(str);
    return(str);
}

pr_strToObj <- function(str) {
    str<-base64enc::base64decode(str);
    obj<-unserialize((str));
    return(obj);
}

######################################################################
# functions to split strings into chunks
######################################################################

pr_splitStr <- function(splitstr,chunksize) {

    s<-chunksize;
    n<-nchar(splitstr);
    chunks<-n/floor(s);
    splitlst<-list()
    for (i in 0:chunks) {
        i1<-i*s+1;
        i2<-(i+1)*s
        chunk<-substr(splitstr,i1,i2);
        splitlst[i+1]<-chunk;
    }
    return(splitlst);
}

pr_unSplitStr <- function(splitlst) {
    str<-paste(splitlst,collapse="")
    return(str)
}

