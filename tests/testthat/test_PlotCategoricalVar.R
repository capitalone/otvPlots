library(otvPlots)
library(proto)
context("Plot categorical variable")
load("../testthat/testData.rda")
setDT(testData)

test_that("PlotCategoricalVar returns a gtable", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotCategoricalVar("marital", testData, NULL, "weeks", "months")	
	expect_is(p, "gtable")
})






