# Bug Reports
Velma is now being maintained by Yingbo Li (yingbo.li@capitalone.com)

# Installation
First, turn on the proxy. Then, open an R (or RStudio) console and run the 
following code:

1. Install the `devtools` package if not yet (You only need to do this once, so
feel free to skip this step if the `devtools` is already installed.)
```
install.packages("devtools")
```
You will be asked to select a CRAN mirror. Usually, any mirror near your area
is fine. I usually choose USA (CA 1). 

2. Install the `otvPlots` package (i.e., the Velma package).
```
devtools::install_github("YingboLi/Velma", host="github.kdc.capitalone.com/api/v3")
```

You can also download the latest build from the issues page, or build the package yourself by cloning the repo, setting your working directory to the otvPlots/Velma folder and running `devtools::build() `. 

Note that otvPlots/Velma does depend on R and several R packages to run. You can see a complete and up to date list of dependencies in the DESCRIPTION file in the otvPlots branch. 

For running Velma on aws, there is an [ionize playbook](https://github.kdc.capitalone.com/CharlesDrotar/ionize-playbooks/tree/master/anaconda-r-velma-playbook) that automatically installs Velma. If you find that there are any out of date dependencies in this playbook, please reach out to the creater Charles Drotar at charles.drotar@capitalone.com.

## R package for variable level monitoring

An important part of model building is the "proc eyeball" sanity check. It can also be a painful part of the process, when you are the data scientist tasked with creating and checking 10,000 or more near-identical plots. Velma is designed to streamline this process. Velma is an R function which takes a flat file as input and provides a pdf of VLM plots as output, optionally ordered so that any severely abnormal time series will be at the top of the pdf. The only strict requirement of the data scientist is to specify which column of the flat file contains the date variable. 

Velma is efficiently implemented using data.table and ggplot2. Plots are automatically labeled if a variable dictionary is provided. Important variables can be given a highlighted label. A custom fuzzy matching algorithm can be provided by the user. 

Discrete and numeric variables are handled automatically and given separate treatment. Plots are customized for credit-type variables such as DMS attributes. 

#### Here is an example of a numeric plot 
<img src="https://github.kdc.capitalone.com/sju922/Velma/blob/master/vignettes/Figs/cntns_ex.png" 
     alt="numeric plot" 
   width="1285" 
   height="700">


#### Here is an example of a discrete plot 
<img src="https://github.kdc.capitalone.com/sju922/Velma/blob/master/vignettes/Figs/dscrt_ex.png" 
     alt="discrete plot" 
   width="1285" 
   height="700">


## Installation on statgw 

Use of statgw for R development is not recommended. AWS or local R will be more stable. If you must, follow the tips [here](https://github.kdc.capitalone.com/FSGalaxyVulcan/Infrastructure/wiki/R-on-the-server) to fix some known issues with the statgw server.


# Getting started

### Velma is extensively documented using R's man pages, so please check out the documentation to see all of the options and many additional usage examples

```
library(otvPlots)
?PlotWrapper
```

## Test it's working
```
example(PlotVar)
```

## Simple usage example
```

 data(bankData);  setDT(bankData)
 data(bankLabels);  setDT(bankLabels)
 
 PlotWrapper(
   dataFl   = bankData, 
   labelFl  = bankLabels, 
   dateNm   = "date", 
   dateGp   = "months", 
   dateGpBp = "quarters", 
   outFl    = "bank.pdf", 
   sortFn   = "OrderByR2",
   prepData = TRUE
   )

```
## New changes
* For categorical variabls' rate trace plots over time, use percentage instead of fraction as y-axis label.

* Previously, for a categorial varaible with more than `kCategories` number of categories, no traceplots of categories' proportions are displayed. In the new version, 
trace plots of the `kCategories` most prevalent categories will be plotted. 

* Change quantile plots with color-blind friendly color palette

* Only apply log transformation for boxplots if the variable is all positive (no zeros).

* Change of terminology:
** continouous variable -> numerical variable
** discrete variable -> categorical variable
** histogram (for categorical variable) -> bar plot
