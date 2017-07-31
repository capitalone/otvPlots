library(otvPlots)
library(proto)
context("Plot Continuous Variable")
load("../testthat/testData.rda")
setDT(testData)

test_that("PlotNumVar returns a gtable", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotNumVar("age", testData, NULL, "weeks", "months", 
                  skewOpt = 3, kSample = NULL)	
	expect_is(p, "gtable")
})

test_that("Incorrect skewOpt creates error", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	expect_error(PlotNumVar("age", testData, NULL, "weeks", "months", 
                  skewOpt = "test", kSample = NULL))
})




