library(otvPlots)
library(proto)
context("Plot Continuous Variable")

test_that("PlotContVar returns a gtable", {
	load("../testthat/testData.rda")
	setDT(testData)
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotContVar("age", testData, NULL, "weeks", "months", 
                  skewOpt = NULL, kSample = NULL)	
	expect_is(p, "gtable")
})




