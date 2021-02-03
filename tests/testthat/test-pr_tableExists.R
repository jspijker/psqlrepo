context("pr_tableExists")

test_that("Valid function arguments",{
              expect_error(pr_tableExists(table=1))
})

test_that("Proper results",{
             pr_init(configfile)
             pr_sql("create table testtable (id int);")
             expect_true(pr_tableExists("testtable"))
             expect_false(pr_tableExists("nonexistingtable"))
})
