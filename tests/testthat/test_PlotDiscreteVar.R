library(otvPlots)
library(proto)
context("Plot Continuous Variable")
load("../testthat/testData.rda")
setDT(testData)

test_that("PlotContVar returns a gtable", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotContVar("age", testData, NULL, "weeks", "months", 
                  skewOpt = 3, kSample = NULL)	
	expect_is(p, "gtable")
})

test_that("Incorrect skewOpt creates error", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	expect_error(PlotContVar("age", testData, NULL, "weeks", "months", 
                  skewOpt = "test", kSample = NULL))
})




