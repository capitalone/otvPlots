library(otvPlots)
library(proto)
context("Plot Quantiles over Time")
load("../testthat/testData.rda")
setDT(testData)

testData[, weeks := round(date, "weeks")]
testDT = testData[, {
   tmp1 = quantile(balance, p = c(.01, .5, .99));
   list("p1"  = tmp1[1] ,
        "p50" = tmp1[2] ,
        "p99" = tmp1[3]
   )}, by = "weeks"]

testMT = melt(testDT, id.vars = "weeks", 
              measure.vars = c("p99", "p50","p1"))
globalPct = testData[ , quantile(balance, p = c(.01, .5, .99) ) ]
globalDT = data.table("weeks" = rep(testMT[variable == "p99", "weeks", 
                      with = FALSE][[1]], 3))
globalDT[, c("variable", "value") := list(rep(c("p1_g", "p50_g", "p99_g"), 
                                              each = .N/3),
                                          rep(globalPct, each = .N/3))]
testMT = rbindlist(list( testMT, globalDT))


test_that("Plot layers match expectations",{
  p <- PlotQuantiles(testMT, myVar = "balance", dateGp = "weeks")  
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomLine")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
})

test_that("Mapping layer contains expected elements", {
  p <- PlotQuantiles(testMT, myVar = "balance", dateGp = "weeks")  
  expect_true( "colour" %in% names(p$mapping)) 
  expect_true( "linetype" %in% names(p$mapping)) 
  expect_true( "group" %in% names(p$mapping)) 
  expect_true( "x" %in% names(p$mapping)) 
  expect_true( "y" %in% names(p$mapping)) 
  expect_length(setdiff(c("colour", "linetype", "group", "x", "y"), names(p$mapping)), 0)
 })

