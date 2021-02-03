
context("pr_storeObj")

test_that("valid arguments",{
              x <- 1

              expect_error(pr_storeObj(1,x))
              expect_error(pr_storeObj("x",x,verbose=1))
              expect_error(pr_storeObj("x",x,overwrite=1))

})

test_that("no database connection",{
              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_storeObj(object=x,name=objname,overwrite=1))
})

test_that("storing and overwriting object",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")

              expect_false(pr_objectExists(objname))
              pr_storeObj(x,objname)
              expect_true(pr_objectExists(objname))

              # test overwrite
              x <- rnorm(10)
              expect_error(pr_storeObj(objname,x))
              pr_storeObj(name=objname,object=x,overwrite=TRUE)
              hash1 <- digest::digest(x)
              hash2 <- pr_getObjHash(objname)
              expect_equal(hash1,hash2)

              pr_dropTables()
              pr_closeConnection()
})

test_that("storing object and meta data",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")

              kv <- list(key1="")
              expect_error(pr_storeObj(x,objname,kv))

              kv <- list(key1="value1",key2="value2",key3="value3")
              pr_storeObj(x,objname,kv)

              kv2 <- pr_getKeyObj(objname)

              expect_false(is.null(kv2$key1))
              expect_true(kv2$key1=="value1")

              pr_dropTables()
              pr_closeConnection()
})
