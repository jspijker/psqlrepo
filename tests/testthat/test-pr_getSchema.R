context("pr_getschema")

test_that("Proper results", {
              pr_init(configfile)
              schema <- pr_getSchema()
              expect_equal(schema,'public')
              pr_closeConnection()
              pr_init(configfile,configuration="psqlrepo")
              schema <- pr_getSchema()
              expect_equal(schema,'test')
              pr_closeConnection()
})



