% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr_kvTest.R
\name{pr_kvTest}
\alias{pr_kvTest}
\title{Test if keyword - value pairs are valid}
\usage{
pr_kvTest(kv, verbose = FALSE)
}
\arguments{
\item{kv}{list with keyword-value pairs}

\item{verbose}{be verbose and explain why test fails}
}
\value{
TRUE if list is valid, FALSE if otherwise

Adding meta-data to an object is done by adding keyword-value
pairs to the object. When getting metadata using `pr_getKeyObj` a
list is returned with these keyword-value pairs. Such a list can
also be given to functions like pr_storeObj of pr_storeBlob

A keyword-value can be created as follows:
list(keyword1="value1",keyword2="value2"). The names in the list
are the keywords. Each item in te list, the keyword, can only
contain one value and this value must be character.

This fucntion tests if the keyword-value list is correct and that no empty
elements, numerical values et cetera, exists. If the test passed,
the return value will be TRUE. The return value will be FALSE if
otherwise.

If the test fails, using the verbose option prints a explanation
why the test fails.

If the kv is not a list, this function returns an error
}
\description{
Meta data is stored in keyword-value pairs. This meta data can be
stored in a list. This function tests if this list is valid.
}
