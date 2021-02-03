context("pr_kvTest")

test_that("Valid function arguments",{
              expect_error(pr_kvTest(kv=1))
              expect_error(pr_kvTest(kv=list(a="b"),verbose="a"))
})


test_that("proper functioning",{
              kv <- list(a="a",b="b")
              expect_true(pr_kvTest(kv))

              kv <- list(a="a",b=1)
              expect_false(pr_kvTest(kv))

              kv <- list()
              expect_false(pr_kvTest(kv))

              kv <- list(a="",b="b")
              expect_false(pr_kvTest(kv))


})
