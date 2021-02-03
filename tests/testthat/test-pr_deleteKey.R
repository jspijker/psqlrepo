context("pr_deleteKey")



test_that("valid arguments",{
              expect_error(pr_deleteKey(1,"a"))
              expect_error(pr_deleteKey("a",1))
              expect_error(pr_deleteKey(1,2))


})



test_that("proper functioning",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey",value="testvalue")

              value <- pr_keyExists(objname,"testkey")
              expect_true(value)

              pr_deleteKey(name=objname,key="testkey")

              value <- pr_keyExists(objname,"testkey")
              expect_false(value)

#               s <- pr_getSchema()
#               did <- pr_getObjId(objname)
# 
#               # check if key exists
#               qry <- paste ("select key,value from ",s,".rkeyvalue where did=",did, 
#                             ";",sep='');
# 
#               d <- pr_sql(qry)
# 
#               expect_false(is.null(d))
#               expect_false(any(is.na(d)))
# 
#               expect_equal(d$key[1],"testkey")
#               expect_equal(d$value[1],"testvalue")
# 
# 
              pr_dropTables()
              pr_closeConnection()

})
