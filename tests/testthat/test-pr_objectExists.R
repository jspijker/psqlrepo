context("pr_objectExists")

test_that("pr_objectExists",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)
              x <- 1
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(name=objname,obj=x,type=1)

              expect_true(pr_objectExists(objname))
              expect_false(pr_objectExists("nonexiststentobject"))
              

              pr_dropTables()
              pr_closeConnection()

})
