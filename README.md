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

## Load the package
Open an R console (or RStudio). Load the `otvPlots` pacakge first (all it 
dependent packages should be loaded automatically).

```
library(otvPlots)
```

The main function of the package is `vlm`. Before execuate this function, 
input data need to be prepared using the `PrepData` function. 
**Please check out the help files to see all options and many usage examples 
(highly recommended!)**

```
help(vlm)
help(PrepData)
```

## Examples 

The data `bankData` and its labels `bankLables` are built-in datasets in the
`otvPlots` package. 

### The first example
After running the following code, a pdf file named "bank.pdf" and two csv files
named "bank_numerical_summary.csv" and "bank_categorical_summary.csv" will be
generated in the current working directory. 

```
## Load the datasets
data(bankData)
data(bankLabels)

## Prepare data and labels
bankData <- PrepData(bankData, dateNm = "date", dateGp = "months", 
                     dateGpBp = "quarters")
bankLabels <- PrepLabels(bankLabels)

## Generate a pdf file of vlm plots, and csv files of summary statistics
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
    sortFn = "OrderByR2", dateGp = "months", dateGpBp = "quarters", outFl = "bank")
```

### More examples on the `bankData` data
The `PrepData` function only needs to be run once on a dataset. After that `vlm`
can berun directly with the argument `dataNeedPrep = FALSE` (the default).

* If csv files of summary statistics are not need, set `genCSV = FALSE`.

```
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, genCSV = FALSE,
    sortFn = "OrderByR2", dateGp = "months", dateGpBp = "quarters", outFl = "bank2")
```     
* If weights are provided, they will be used in all statistical calculations

```
bankData[, weight := rnorm(.N, 1, .1)]
bankData[, weight := weight / mean(weight)]
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
    dateGp = "months", dateGpBp = "quarters", weightNm = "weight", outFl = "bank3")
```

* Customize plotting order by passing a vector of variable names to argument
`sortVars`, but the `"date"` column must be excluded from `sortVars`

```
sortVars <- sort(bankLabels[varCol!="date", varCol])
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
    dateGp = "months", dateGpBp = "quarters", outFl = "bank", 
    sortVars = sortVars)
```

* Create plots for a specific variable using the `varNms` argument

```
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
    dateGp = "months", dateGpBp = "quarters", outFl = "bank", 
    varNms = "age", sortVars = NULL)
```

### Example: input from a csv file
Suppose in the working directory, the data is in the format of a csv file
named "bd_efx_fc_mergable_sample_rand.csv". 

```
## Prep data first
bd_efx = PrepData('bd_efx_fc_mergable_sample_rand.csv', dateNm = "app_date", 
                   dateGp = "months", dateGpBp = "quarters")
                   
## Then run the vlm function
vlm(dataFl = bd_efx, dateNm = "app_date", sortFn = "OrderByR2", 
    dateGp = "months", dateGpBp = "quarters", outFl = "bd_efx")
```

# What's New in Version 0.2.0
* Creating summary statistics aggreated global and over time, and outputing them
as csv files separately for numerical and categorical variables. 

* The name of the main function is changed to `vlm` (from `PlotWrapper`).

* Change to a color-blind friendly color palette.

* For categorical variabls' trace plots of proportions (rates) over time, use 
percentage instead of fraction as the y-axis label.

* Previously, for a categorial varaible with more than `kCategories` number of 
categories, no traceplots of categories' proportions are displayed. In Version
0.2.0, trace plots of the `kCategories` most prevalent categories will be plotted. 

* For numerical variables' boxplots, only apply log transformation if the 
variable is all positive (no zeros).

* Change of terminology:
continouous variable -> numerical variable
discrete variable -> categorical variable
histogram (for categorical variable) -> bar plot


# Bug Reports
Velma is now being maintained by Yingbo Li (yingbo.li@capitalone.com)


