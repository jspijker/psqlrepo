context("pr_getConfig")


test_that("Test pr_getConfig", {
              pr_init(configfile)
              prinfo <- pr_getConfig()
              expect_true(!is.null(prinfo$initSuccess))
              pr_closeConnection()
})
 
