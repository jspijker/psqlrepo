context("pr_deleteObj")

test_that("pr_deleteObj, valid arguments", {
              expect_error(pr_deleteObj(1))
              expect_error(pr_deleteObj("nonexistingobject"))


})

test_that("pr_deleteObj, correct functioning", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(x,objname,type=1)
              pr_storeKey(name=objname,key="testkey",value="testvalue")

              s <- pr_getSchema()
              did <- pr_getObjId(objname)

              pr_deleteObj(objname)
	
              qry <- paste0("select key,value from ",s,".rkeyvalue where did=",did, 
                            ";");

              d <- pr_sql(qry,verbose=FALSE)

              expect_true(nrow(d)==0)

              d<-pr_sql(paste0("select name from public.robjects where
				 name='",objname,"'"))
              expect_equal(d$name,NULL)

              pr_dropTables()
              pr_closeConnection()

})

