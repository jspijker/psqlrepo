context("pr_deleteKeyObj")


test_that("valid arguments",{
              expect_error(pr_deleteKeyObj(1))
              expect_error(pr_deleteKeyObj("nonExistingObject"))

})



test_that("proper functioning",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey1",value="testvalue1")
              pr_storeKey(name=objname,key="testkey2",value="testvalue2")

              value <- pr_keyExists(objname,"testkey1")
              expect_true(value)

              pr_deleteKeyObj(name=objname)

              value <- pr_keyExists(objname,"testkey1")
              expect_false(value)
              value <- pr_keyExists(objname,"testkey2")
              expect_false(value)

              pr_dropTables()
              pr_closeConnection()

})
