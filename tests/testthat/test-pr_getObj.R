context("pr_getObj")


test_that("valid arguments",{
              expect_error(pr_getObj(name=1))
              expect_error(pr_getObj(name="x",verbose=1))
              expect_error(pr_getObj(name="x",ignoretype=1))

})

test_that("no database connection",{
              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_getObj(name=objname))
})

test_that("storing and getting object",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              x2 <- pr_getObj(objname)

              expect_equal(x1,x2)

              pr_dropTables()
              pr_closeConnection()
})


