context("pr_init")

test_that("Valid function arguments",{
             expect_error(pr_init(configfile=1))
             expect_error(pr_init(configuration=1))
             expect_error(pr_init(configfile="nonexistentfile"))
             expect_error(pr_init(configuration="nonexistentconfiguration"))
})

             
test_that("Test pr_testConnection and pr_closeConnection", {
              expect_false(pr_testConnection())
              pr_init(configfile)
              expect_true(pr_testConnection())
              pr_closeConnection()
              expect_false(pr_testConnection())
})

test_that("Init Success", {
              pr_init(configfile)
              prinfo <- get("prinfo",env=.psqlrepoConfig)
              expect_true(prinfo$initSuccess)
              pr_closeConnection()
})

test_that("Test alternative configuration",{
              pr_init(configfile,configuration="psqlrepo")
              prinfo <- get("prinfo",env=.psqlrepoConfig)
              expect_true(prinfo$initSuccess)
              pr_closeConnection()
})
