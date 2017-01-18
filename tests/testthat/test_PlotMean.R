library(otvPlots)
library(proto)
context("Plot Mean over Time")
load("../testthat/testData.rda")
testData <- setDT(testData)
testData <- testData[, .(balance, weight, date)]
testData[, weeks := round(date, "weeks")]

testDT = testData[, .(Mean = mean(balance)), by = "weeks"]
cl = testData[, c(mean(balance), sd(balance))]
cl = cl %*% matrix(c(1, 1, 1, -1), byrow = TRUE, nrow = 2) # mean +- 1 SD
testDT[, c("cl1", "cl2") := list(cl[1], cl[2])  ]
testMT = melt(testDT, id.vars = "weeks", 
          measure.vars = c("Mean", "cl1", "cl2"))

test_that("Plot layers match expectations",{
  p <- PlotMean(testMT, "balance", "weeks")
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomLine")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
})

test_that("X axis is labelled 'weeks'",{
  p <- PlotMean(testMT, "balance", "weeks")
  expect_identical(p$labels$x, "weeks")
  expect_identical(p$labels$y, NULL)
})

test_that("Scale is discrete",{
   p <- PlotMean(testMT, "balance", "weeks")
   expect_is(p$scales$scales[[1]], "ScaleDiscrete")
})

test_that("Mapping layer contains expected elements",{
  p <- PlotMean(testMT, "balance", "weeks") 
  expect_true( "group" %in% names(p$mapping)) 
  expect_true("linetype" %in% names(p$mapping))
  expect_true( "x" %in% names(p$mapping)) 
  expect_true( "y" %in% names(p$mapping)) 
  expect_length(setdiff(c("group", "linetype", "x", "y"), names(p$mapping)), 0)	
})