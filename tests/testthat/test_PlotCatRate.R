library(otvPlots)
library(proto)
context("Plot Discrete Rates")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))
