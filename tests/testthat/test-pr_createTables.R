context("pr_createTables")


test_that("Valid function arguments",{
              expect_error(pr_createTable(delete=1))
})

test_that("Proper functioning",{
              pr_init(configfile)
              pr_createTables()
              expect_true(pr_tableExists("robjects"))
              pr_closeConnection()
})

test_that("Proper functioning alternative config",{

              pr_init(configfile,configuration="psqlrepo")
              expect_error(pr_createTable())
              pr_sql("CREATE SCHEMA test;")
              pr_createTables()
              expect_true(pr_tableExists("robjects"))
              res<-pr_sql("select * from test.robjects limit 0");
              expect_true(is.data.frame(res))
              pr_closeConnection()
})

test_that("Deleting of tables",{

              pr_init(configfile,configuration="psqlrepo")

              try(pr_dropTables())

              expect_error(pr_dropTables())
              pr_createTables()
              expect_true(pr_tableExists("robjects"))
              expect_error(pr_createTables())
              pr_createTables(delete=TRUE)
              expect_true(pr_tableExists("robjects"))
              pr_dropTables()
              expect_false(pr_tableExists("robjects"))
              pr_closeConnection()
})
