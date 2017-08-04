library(otvPlots)
context("Summary stats for numerical variables")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))

test_that("Numerical statistics are calculated correctly without weight", {
	mdx  = SummaryStats(myVar = "age", dataFl = testData, dateGp = "weeks")$meltdx
	Mean = mdx[variable=='Mean']
	p1   = mdx[variable=='p1']
	p99  = mdx[variable=='p99']
	zerorate    = mdx[variable=='zerorate']
	missingrate = mdx[variable=='missingrate']

	p99_g = unique(mdx[variable=='p99_g', value])
	p1_g = unique(mdx[variable=='p1_g', value])
	cl1  = unique(mdx[variable=='cl1', value])
	cl2  = unique(mdx[variable=='cl2', value])
	
	expect_equivalent(p99_g, quantile(testData[, age], p=.99))
	expect_equivalent(p1_g, quantile(testData[, age],  p=.01))
	expect_equivalent(cl1,  mean(testData[, age]) + sd(testData[,age]))
	expect_equivalent(cl2,  mean(testData[, age]) - sd(testData[,age]))
	
	mdx2 = mdx[weeks == "2008-05-06" & variable%in%c("p99", "p50", "p1", "mean", "zerorate", "missingrate")]
	
	expect_equivalent(mdx2[variable=="p99", value], quantile(testData[weeks==as.IDate("2008-05-06"),age], .99))
	expect_equivalent(mdx2[variable=="p50", value], quantile(testData[weeks==as.IDate("2008-05-06"),age], .5))
	expect_equivalent(mdx2[variable=="p1", value], quantile(testData[weeks==as.IDate("2008-05-06"),age], .01))
	expect_equivalent(mdx2[variable=="mean", value], mean(testData[weeks==as.IDate("2008-05-06"),age]))
	expect_equivalent(mdx2[variable=="zerorate", value], mean(testData[weeks==as.IDate("2008-05-06"),age]==0))
	expect_equivalent(mdx2[variable=="missingrate", value], mean(is.na(testData[weeks==as.IDate("2008-05-06"),age])))
})


test_that("Numerical statistics are calculated correctly with weight", {
	mdx  = SummaryStats(myVar = "age", dataFl = testData, dateGp = "weeks", weightNm = "weight")$meltdx
	Mean = mdx[variable=='Mean']
	p1   = mdx[variable=='p1']
	p99  = mdx[variable=='p99']
	zerorate    = mdx[variable=='zerorate']
	missingrate = mdx[variable=='missingrate']


	p99_g = unique(mdx[variable=='p99_g', value])
	p1_g = unique(mdx[variable=='p1_g', value])
	cl1  = unique(mdx[variable=='cl1', value])
	cl2  = unique(mdx[variable=='cl2', value])
	
	expect_equivalent(p99_g, Hmisc::wtd.quantile(testData[, age], testData[, weight], probs=.99, normwt=TRUE))
	expect_equivalent(p1_g, Hmisc::wtd.quantile(testData[, age], testData[, weight], probs=.01, normwt=TRUE))
	expect_equivalent(cl2, Hmisc::wtd.mean(testData[, age], testData[,weight], na.rm=TRUE, normwt=TRUE) -	
					 sqrt(Hmisc::wtd.var(testData[,age], testData[,weight], na.rm=TRUE,normwt=TRUE)))
    expect_equivalent(cl1, Hmisc::wtd.mean(testData[, age], testData[,weight], na.rm=TRUE, normwt=TRUE) +	
					 sqrt(Hmisc::wtd.var(testData[,age], testData[,weight], na.rm=TRUE,normwt=TRUE)))
					 
	mdx2 = mdx[weeks == "2008-05-06" & variable%in%c("p99", "p50", "p1", "mean", "zerorate", "missingrate")]
	testData2 = testData[weeks==as.IDate("2008-05-06")]
	
	expect_equivalent(mdx2[variable=="p99", value], Hmisc::wtd.quantile(testData2[, age],testData2[, weight], .99, normwt=TRUE))
	expect_equivalent(mdx2[variable=="p50", value], Hmisc::wtd.quantile(testData2[, age],testData2[, weight], .5, normwt=TRUE))
	expect_equivalent(mdx2[variable=="p1", value], Hmisc::wtd.quantile(testData2[, age],testData2[, weight], .01, normwt=TRUE))
	expect_equivalent(mdx2[variable=="mean", value], Hmisc::wtd.mean(testData2[,age], testData2[,weight]))
	expect_equivalent(mdx2[variable=="zerorate", value], Hmisc::wtd.mean((testData2[,age]==0), testData2[,weight]))
	expect_equivalent(mdx2[variable=="missingrate", value], Hmisc::wtd.mean(is.na(testData2[,age]), testData2[,weight]))
})

