context("Store helper functions")

test_that("pr_store valid arguments", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(3)
              objname <- "test"
              expect_error(pr_store(name=1,obj=x,type=1))
              expect_error(pr_store(name=objname,obj=x,type="a"))
              expect_error(pr_store(name=objname,obj=x,type=1,chunksize="a"))
              expect_error(pr_store(name=objname,obj=x,type=1,verbose=1))

              pr_dropTables()
              pr_closeConnection()

})

test_that("pr_store proper functioning", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(name=objname,obj=x,type=1)

	
              d<-pr_sql(paste("select name from public.robjects where
				 name='",objname,"'",sep=''))
              expect_equal(d$name,objname)
              pr_dropTables()
              pr_closeConnection()

})

test_that("pr_get valid arguments", {

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(3)
              objname <- "test"

              expect_error(pr_get(name=1))
              expect_error(pr_get(name="nonexistentobject"))

              pr_dropTables()
              pr_closeConnection()
})

test_that("pr_get proper functioning", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x1 <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(name=objname,obj=x1,type=1)
              x2 <- pr_get(objname)
              expect_equal(x1,x2)
              pr_dropTables()
              pr_closeConnection()

})


test_that("pr_delete", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(x,objname,type=1)
              pr_delete(objname)
	
              d<-pr_sql(paste("select name from public.robjects where
				 name='",objname,"'",sep=''))
              expect_equal(d$name,NULL)

              pr_dropTables()
              pr_closeConnection()

})

test_that("pr_getObjId", {
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(x,objname,type=1)
              v1 <- pr_getObjId(objname)
              expect_equal(v1,1)
              pr_dropTables()
              pr_closeConnection()
})

test_that("pr_getObjHash",{
              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(x,objname,type=1)
              hash1 <- digest::digest(x)
              hash2 <- pr_getObjHash(objname)
              expect_equal(hash1,hash2)

              pr_dropTables()
              pr_closeConnection()
})

test_that("pr_getObjType",{

              pr_init(configfile)
              pr_createTables(delete=TRUE)

              x <- rnorm(10)
              objname <- paste(sample(c(letters),8),collapse="")
              pr_store(name=objname,obj=x,type=1)
              expect_error(pr_getObjType(objname=1))
              x2 <- pr_getObjType(objname)
              expect_equal(x2,1)

              pr_dropTables()
              pr_closeConnection()
})
	
