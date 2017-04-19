library(otvPlots)
library(proto)
context("Plot Discrete Variable")
load("../testthat/testData.rda")
setDT(testData)

test_that("PlotContVar returns a gtable", {
	PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "months")
	p <- PlotDiscreteVar("marital", testData, NULL, "weeks", "months")	
	expect_is(p, "gtable")
})






