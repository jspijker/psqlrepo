context("pr_getKey")

test_that("no database connection",{

              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_storeKey(objname=objname,key="test",value="test"))
})

test_that("valid arguments",{

              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_storeKey(name=1,key="test"))
              expect_error(pr_storeKey(name=objname,key=1))

})

test_that("proper functioning",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey",value="testvalue")

              value <- pr_getKey(name=objname,key="testkey")
              expect_equal(value,"testvalue")

              pr_dropTables()
              pr_closeConnection()

})

test_that("non existing object or key",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey",value="testvalue")
              
              expect_error(pr_getKey(name=objname,key="nonexistingkey"))
              expect_error(pr_getKey(name="nonexistingobject",key="testkey"))

              pr_dropTables()
              pr_closeConnection()

})


