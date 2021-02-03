context("pr_kvStore")



test_that("Valid function arguments",{
              expect_error(pr_kvStore(kv=1))
              expect_error(pr_kvStore(kv=list(a="b"),overwrite="a"))
})

test_that("proper functioning",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)

              kv <- list(key1="value1",key2="value2",key3="value3")
              pr_kvStore(objname,kv)
              kv2 <- pr_getKeyObj(objname)

              expect_false(is.null(kv2$key1))
              expect_true(kv2$key1=="value1")
              expect_false(is.null(kv2$key2))
              expect_true(kv2$key2=="value2")
              expect_false(is.null(kv2$key3))
              expect_true(kv2$key3=="value3")


              expect_error(pr_kvStore(kv))

              kv <- list(key1="value1",key2="value2")
              pr_kvStore(objname,kv,overwrite=TRUE)
              kv2 <- pr_getKeyObj(objname)

              expect_false(is.null(kv2$key1))
              expect_true(kv2$key1=="value1")

              pr_dropTables()
              pr_closeConnection()
})

