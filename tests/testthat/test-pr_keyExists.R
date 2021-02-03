context("pr_keyExists")


test_that("valid arguments",{

              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_keyExists(name=1,key="test"))
              expect_error(pr_keyExists(name=objname,key=1))

})

test_that("proper functioning",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)

              x1 <- pr_keyExists(objname,"testkey")
              pr_storeKey(name=objname,key="testkey",value="testvalue")
              x2 <- pr_keyExists(objname,"testkey")

              expect_false(x1)
              expect_true(x2)


              pr_dropTables()
              pr_closeConnection()

})

