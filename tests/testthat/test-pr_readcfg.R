context("pr-readcfg")


test_that("invalid arguments",{
             expect_error(pr_readcfg())
             expect_error(pr_readcfg(config=1,filename=configfile))
             expect_error(pr_readcfg(config="default",filename=1))
})


test_that("non existing config file or configuration",{
             expect_error(pr_readcfg(config="default",filename="reallunonexistingfile.yml"))
             expect_error(pr_readcfg(config="nonexistingconfiguration",filename=configfile))
})

test_that("proper returns",{
             x <- pr_readcfg(config="default",filename=configfile)
             expect_equal(x$dbname,dbname)
             expect_equal(x$dbuser,dbuser)
             x <- pr_readcfg(config="psqlrepo",filename=configfile)
             expect_equal(x$test,"testvalue")

})
