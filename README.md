# R Package for Variable Level Monitoring
An important part of model building is the "proc eyeball" sanity check. It can
also be a painful part of the process, when you are the data scientist tasked 
with creating and checking 10,000 or more near-identical plots. The `otvPlots`
package is designed to streamline this process. `otvPlots` is
an R package which takes a csv file as input and provides a pdf of VLM plots 
and csv files of summary statistics as output, optionally ordered so 
that any severely abnormal time series will be at the top of the pdf. The only
strict requirement of the data scientist is to specify which column of the input
data file contains the date variable. 

`otvPlots` is efficiently implemented using `data.table` and `ggplot2` packages in R.
Plots are automatically labeled if a variable dictionary is provided. Important
variables can be given a highlighted label. A custom fuzzy matching algorithm 
can be provided by the user. 

Discrete and numeric variables are handled automatically and given separate 
treatment. All binary variables are treated as categorical. 

## Output files generated by this package

### A PDF file of plots, with each individual page on one variable. 

For each numerical variable, the output plots include 
* side-by-side boxplots (left), 
* a trace plot of p1, p50, and p99 percentiles, 
* a trace plot of mean and +-1 SD control limits, and 
* a trace plot of missing and zero rates (bottom right).

#### Here is an example page of plots for a numerical variable 
<img src="/figures/sample_plots_numerical.png" 
     alt="numerical plot" 
   width="770" 
   height="560">

For each categorical variable (including a numerical variable with no more 
  than 2 unique levels not including NA), the output plots include 
* a frequency bar plot (left), and 
* a grid of trace plots on categories' proportions over time (right). 

#### Here is an example page of plots for a categorical variable 
<img src="/figures/sample_plots_categorical.png" 
     alt="categorical plot" 
   width="770" 
   height="560">

### CSV file(s) on summary statistics of variables, both globally and over time. 

The order of variables in the CSV files is the same as in the PDF file. 
* A CSV file for numerical variables, including the number of observations 
     (counts), p1, p25, p50, p75, and p99 quantiles, mean, SD, missing and 
     zero rates.
* A CSV file for categorical variables, including the number of observations 
     (counts) and categories' proportions. Each row is a category of a 
     categorical (or binary) variable. The row whose `category == 'NA'`
     corresponds to missing. Categories among the same variable are ordered by
     global prevalence in a descending order.

# Installation
Open an R (or RStudio) console and run the 
following code:

1. Install the `devtools` package if not yet. You only need to do this once, so
feel free to skip this step if the `devtools` is already installed. You will be
asked to select a CRAN mirror. 

```
install.packages("devtools")
```

2. Install the `otvPlots` package
```
devtools::install_github("capitalone/otvPlots")
```

You can also build the package yourself by cloning the repo, setting your 
working directory to the otvPlots folder and running `devtools::build()`
in R, after installing the `devtools` package. 

Note that otvPlots does depend on R and several R packages to run. You can
see a complete and up to date list of dependencies in the Imports field in
the DESCRIPTION file.


# Getting Started

## Load the package
Open an R console (or RStudio). Load the `otvPlots` pacakge first (all its 
dependent packages should be loaded automatically).

```
library(otvPlots)
```

The main function of the package is `vlm`. Before execute this function, 
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
can be run directly with the argument `dataNeedPrep = FALSE` (the default).

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
    dateGp = "months", dateGpBp = "quarters", outFl = "bank4", 
    sortVars = sortVars)
```

* Create plots for a specific variable using the `varNms` argument

```
vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
    dateGp = "months", dateGpBp = "quarters", outFl = "bank5", 
    varNms = "age", sortVars = NULL)
```


