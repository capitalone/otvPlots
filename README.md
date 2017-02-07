######Copyright 2016 Capital One Services, LLC

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License. 

# Over-time variable plots for predictive modeling (otvPlots)

## R data visualization package for predictive model building 

An important part of every new predictive model build is the "proc-eyeball" sanity check, used to identify severe drift which can cause poor model performance in market. It can be a painful part of model development when you are the data scientist tasked with creating and checking thousands of near-identical plots. \code{otvPlots} streamlines this process. Over-time variable plots for predictive modeling (\code{otvPlots}) is an R package that turns a dataset into a monitoring report with minimal interaction by the user, with the output optionally ordered by strength of linear association with date, so that any severely abnormal time series will tend to be placed at the top of the pdf. The only real requirement of the user, other than to specify the location of a reasonably formatted input dataset, is to specify which column contains the date variable. 

otvPlots is efficiently implemented using data.table and ggplot2. It has been tested on data sets up to 35gb in size, which requires about 60gb of memory. Plots are automatically labeled if a variable dictionary is provided, and a custom fuzzy matching algorithm can be provided by the user. Additional features are detailed in the documentation.

Discrete and numeric variables are handled automatically and given separate treatment. Numeric plots are customized for data with high missing and zero rates, and allow boxplots to be automatically transformed when there is extreme right skew. Binary data is always treated as categorical. Plots of discrete variables give information on the overall distribution of categories, as well as change in distribution over time. 


#### Here is an example of a numeric plot ![numeric plot](https://github.com/rebeccapayne/otvPlots/blob/master/vignettes/Figs/cntns_ex.png)

#### Here is an example of a discrete plot ![discrete plot](https://github.com/rebeccapayne/otvPlots/blob/master/vignettes/Figs/dscrt_ex.png)

#### Here is a binary plot ![binary plot](https://github.com/rebeccapayne/otvPlots/blob/master/vignettes/Figs/bin_ex.png)


## Install the otvPlots package

```
install.packages("devtools")
devtools::install_github("rebeccapayne/otvPlots")
```

## Load the library

```
library(otvPlots)
```

## Check out the documentation to see all of the options, the parameters and their definitions, and many usage examples
```
?PlotWrapper
?PlotVar
```

## Test it's working
```
example(PlotVar)
```

## Simple usage example
```

 data(bankData);  setDT(bankData)
 data(bankLabels);  setDT(bankLabels)
 
 PrepLabels(bankLabels)
 PlotWrapper(
   dataFl   = bankData, 
   labelFl  = bankLabels, 
   dateNm   = "date", 
   dateGp   = "months", 
   dateGpBp = "quarters", 
   outFl    = "bank.pdf", 
   prepData = TRUE
   )

```

## Contributors:
We welcome your interest in Capital One’s Open Source Projects (the “Project”). Any Contributor to the project must accept and sign a CLA indicating agreement to the license terms. Except for the license granted in this CLA to Capital One and to recipients of software distributed by Capital One, you reserve all right, title, and interest in and to your contributions; this CLA does not impact your rights to use your own contributions for any other purpose.

[Link to CLA](https://docs.google.com/forms/d/19LpBBjykHPox18vrZvBbZUcK6gQTj7qv1O5hCduAZFU/viewform)

This project adheres to the [Open Source Code of Conduct](https://developer.capitalone.com/single/code-of-conduct/). By participating, you are expected to honor this code.

