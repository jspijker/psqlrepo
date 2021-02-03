context("pr_storeKey")

test_that("no database connection",{

              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_storeKey(objname=objname,key="test",value="test"))
})

test_that("valid arguments",{

              objname <- paste(sample(c(letters),8),collapse="")
              expect_error(pr_storeKey(name=1,key="test",value="test"))
              expect_error(pr_storeKey(name=objname,key=1,value="test"))
              expect_error(pr_storeKey(name=objname,key="test",value=1))
              expect_error(pr_storeKey(name=objname,key="test",value="test",overwrite=1))

})

test_that("proper functioning",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey",value="testvalue")

              s <- pr_getSchema()
              did <- pr_getObjId(objname)

              # check if key exists
              qry <- paste ("select key,value from ",s,".rkeyvalue where did=",did, 
                            ";",sep='');

              d <- pr_sql(qry)

              expect_false(is.null(d))
              expect_false(any(is.na(d)))

              expect_equal(d$key[1],"testkey")
              expect_equal(d$value[1],"testvalue")


              pr_dropTables()
              pr_closeConnection()
})

test_that("test for non existing objects",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              expect_error(pr_storeKey(name="nonexistingobjects",
                                       key="testkey",value="testvalue"))
              pr_dropTables()
              pr_closeConnection()

})

test_that("overwrite key",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_storeObj(x1,objname)
              pr_storeKey(name=objname,key="testkey",value="testvalue")
              expect_error( pr_storeKey(name=objname,key="testkey",value="testvalue2") )
              pr_storeKey(name=objname,key="testkey",value="testvalue2",overwrite=TRUE)

              s <- pr_getSchema()
              did <- pr_getObjId(objname)

              # check if key exists
              qry <- paste ("select key,value from ",s,".rkeyvalue where did=",did, 
                            ";",sep='');

              d <- pr_sql(qry)

              expect_equal(d$value[1],"testvalue2")


              pr_dropTables()
              pr_closeConnection()
})

