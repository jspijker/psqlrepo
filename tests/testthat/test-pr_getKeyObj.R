context("pr_getKeyObj")


test_that("valid arguments",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              expect_error(pr_getKeyObj(1))
              expect_error(pr_getKeyObj("nonexistingobject"))

              pr_dropTables()
              pr_closeConnection()

})



test_that("proper functioning",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              x <- pr_getKeyObj(objname)
              expect_equal(length(x),0)

              pr_storeKey(name=objname,key="testkey1",value="testvalue1")
              pr_storeKey(name=objname,key="testkey2",value="testvalue2")

              x <- pr_getKeyObj(objname)

              expect_equal(length(x),2)
              expect_equal(x[["testkey1"]],"testvalue1")
              expect_equal(x[["testkey2"]],"testvalue2")


              pr_dropTables()
              pr_closeConnection()
})
