context("test helper functions strings")

test_that("pr_objToStr and pr_strToObj",{

              x <- rnorm(10)

              v1 <- pr_objToStr(x)
              v2 <- pr_strToObj(v1)
              expect_equal(x,v2)
})

test_that("pr_splitStr and pr_unSplitStr",{


              x <- rnorm(10)
              v1 <- pr_objToStr(x)
              v2 <- pr_splitStr(v1,chunksize=10)
              v3 <- pr_unSplitStr(v2)
              expect_equal(v1,v3)
})
              

