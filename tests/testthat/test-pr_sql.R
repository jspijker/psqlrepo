context("pr_sql")


test_that("Valid function arguments",{
             pr_init(configfile)
             expect_error(pr_sql(query=1))
             expect_error(pr_sql(query="a",verbose=1))
             pr_closeConnection()

})

test_that("Proper return",{
             pr_init(configfile)
             expect_null(pr_sql("create table xxxx (id int);"))
             expect_warning(res <- pr_sql("select something from nonexistingtable",verbose=TRUE))
             expect_equal(res ,NA)
             expect_true(class(pr_sql("select * from xxxx"))=="data.frame")
             pr_closeConnection()
})
             
