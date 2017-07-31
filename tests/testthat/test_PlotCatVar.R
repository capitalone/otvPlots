library(otvPlots)
library(proto)
context("Plot categorical variable")
load("../testthat/testData.rda")
setDT(testData)

test_that("PlotCatVar returns a gtable", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotCatVar("marital", testData, NULL, "weeks", "months")	
	expect_is(p, "gtable")
})






