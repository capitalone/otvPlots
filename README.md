# R Package for Variable Level Monitoring
An important part of model building is the "proc eyeball" sanity check. It can
also be a painful part of the process, when you are the data scientist tasked 
with creating and checking 10,000 or more near-identical plots. The `otvPlots`
package (previous name: Velma) is designed to streamline this process. Velma is
an R package which takes a csv file as input and provides a pdf of VLM plots 
and **csv files of summary statistics (new!)** as output, optionally ordered so 
that *any severely abnormal time series will be at the top of the pdf*. The only
strict requirement of the data scientist is to specify which column of the input
data file contains the date variable. 

Velma is efficiently implemented using `data.table` and `ggplot2` packages in R.
Plots are automatically labeled if a variable dictionary is provided. Important
variables can be given a highlighted label. A custom fuzzy matching algorithm 
can be provided by the user. 

Discrete and numeric variables are handled automatically and given separate 
treatment. All binary variables are treated as categorical. 

## Output files geneated by this package

### A PDF file of plots, with each indivual page on one variable. 

For each numerical variable, the output plots include 
* side-by-side boxplots (left), 
* a trace plot of p1, p50 and p99 percentiles, 
* a trace plot of mean and +-1 SD control limits, and 
* a trace plot of missing and zerorates (bottom right).

#### Here is an example page of plots for a numerical variable 
<img src="https://github.kdc.capitalone.com/YingboLi/Velma/blob/dev3-add_summary_outputs/figures/sample_plots_numerical.png" 
     alt="numerical plot" 
   width="770" 
   height="560">

For each categorical variable (including a numerical variable with no more 
  than 2 unique levels not including NA), the output plots include 
* a frequency bar plot (left), and 
* a grid of trace plots on categories' proportions over time (right). 

#### Here is an example page of plots for a categorical variable 
<img src="https://github.kdc.capitalone.com/YingboLi/Velma/blob/dev3-add_summary_outputs/figures/sample_plots_categorical.png" 
     alt="categorical plot" 
   width="770" 
   height="560">

### CSV file(s) on summary statistics of variable, both globally and over time. 

The order of variables in the CSV files are the same as in the PDF file. 
* A CSV file for numerical varaibles, including number of observations 
     (counts), p1, p25, p50, p75, and p99 qunatiles, mean, SD, missing and 
     zerorates.
* A CSV file for categorical varaibles, including number of observations 
     (counts) and categories' proportions. Each row is a category of a 
     categorical (or binary) variable. The row whose `category == 'NA'`
     corresponds to missing. Categories among the same variable are ordered by
     global prevalence in a descending order.

# Installation
First, *turn on the proxy*. Then, open an R (or RStudio) console and run the 
following code:

1. Install the `devtools` package if not yet. You only need to do this once, so
feel free to skip this step if the `devtools` is already installed. You will be
asked to select a CRAN mirror. Usually, any mirror near your area is fine. I 
usually choose USA (CA 1). 

```
install.packages("devtools")
```

2. Install the `otvPlots` package (i.e., the Velma package).
```
devtools::install_github("YingboLi/Velma", host="github.kdc.capitalone.com/api/v3")
```

You can also build the package yourself by cloning the repo, setting your 
working directory to the otvPlots/Velma folder and running `devtools::build()`
in R, after installing the `devtools` package. 

Note that otvPlots/Velma does depend on R and several R packages to run. You can
see a complete and up to date list of dependencies in the Imports field in
the DESCRIPTION file.

For running Velma on aws, there is an [ionize playbook](https://github.kdc.capitalone.com/CharlesDrotar/ionize-playbooks/tree/master/anaconda-r-velma-playbook) that automatically installs Velma. If you find that there are any out of date dependencies in this playbook, please reach out to the creater Charles Drotar at charles.drotar@capitalone.com.

# Getting Started

### Load the package
Open an R console (or RStudio). Load the `otvPlots` pacakge first (all it 
dependent packages should be loaded automatically).

```
library(otvPlots)
``

The main function of the package is `vlm`. Before execuate this function, 
input data need to be prepared using the `PrepData` function. 
**Please check out the help files to see all options and many usage examples 
(highly recommended!)**

```
help(vlm)
help(PrepData)
```

### First example


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

# Bug Reports
Velma is now being maintained by Yingbo Li (yingbo.li@capitalone.com)


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
