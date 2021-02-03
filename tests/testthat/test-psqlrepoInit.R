
context("psqlrepoInit")

test_that("Configuration environment", {
              expect_true(exists(".psqlrepoConfig"))
})
