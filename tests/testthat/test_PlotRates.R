library(otvPlots)
library(proto)
context("Plot Continuous Rates over Time")
load("../testthat/testData.rda")
testData <- setDT(testData)
testData <- testData[, .(balance, weight, date)]
testData[, weeks := round(date, "weeks")]
testDT = testData[, {list("zerorate" = mean(balance == 0),
                           "missingrate" = mean(is.na(balance)))}, 
                  by = "weeks"]
testMT = melt(testDT, id.vars = "weeks", 
               measure.vars = c("zerorate", "missingrate"))


test_that("Plot layers match expectations",{
  p <- PlotRates(testMT, "balance", "weeks")
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomLine")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
})

test_that("X axis is labelled 'weeks'",{
  p <- PlotRates(testMT, "balance", "weeks")
  expect_identical(p$labels$x, "weeks")
  expect_identical(p$labels$y, NULL)
})


test_that("Mapping layer contains expected elements", {
  p <- PlotRates(testMT, myVar = "balance", dateGp = "weeks")  
  expect_true( "colour" %in% names(p$mapping)) 
  expect_true( "group" %in% names(p$mapping)) 
  expect_true( "x" %in% names(p$mapping)) 
  expect_true( "y" %in% names(p$mapping)) 
  expect_length(setdiff(c("colour", "group", "x", "y"), names(p$mapping)), 0)
 })

