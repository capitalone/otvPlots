#' Over time variable plots for predictive modeling (otvPlots)
#'
#' The \code{otvPlots} package uses \code{data.table} and \code{ggplot2} 
#' packages to efficiently plot time series aggregated from large datasets. 
#' Plots of numerial variables are optionally returned ordered by correlation 
#' with date -- a natural starting point for anomaly detection. Plots are 
#' automatically labeled if a variable dictionary is provided. 
#' 
#' Output files include:
#' \itemize{
#'  \item A PDF file of plots saved as \code{outFl}.pdf, with each indivual page 
#'  on one variable. Variables are plotted in the order indicated in the argument
#'  \code{sortVars} or \code{sortFn}. 
#'  For each numerical variable, the output plots include 
#'  \itemize{
#'    \item side-by-side boxplotx grouped by \code{dateGpBp} (left), 
#'    \item a trace plot of p1, p50 and p99 percentiles, grouped by \code{dateGp}
#'      (top right), 
#'    \item a trace plot of mean and +-1 SD control limits, grouped by 
#'      \code{dateGp}(middle right), and 
#'    \item a trace plot of missing and zerorates, grouped by \code{dateGp} 
#'      (bottom right).
#'   }
#'   For each categorical variable (including a numerical variable with no more 
#'   than 2 unique levels not including NA), the output plots include 
#'   \itemize{
#'     \item a frequency bar plot (left), and 
#'     \item a grid of trace plots on categories' proportions over time (right). 
#'       If the variable contains more than \code{kCategories} number of 
#'       categories, trace plots of only the largest \code{kCategories} will be 
#'       plotted. If the variable contains only two categories, then only the 
#'       trace plot of the less prevalent cateogy will be plotted.
#'   }
#'   \item CSV file(s) on summary statistics of variable, both globally and over
#'   time aggregated by \code{dateGp}. The order of variables in the CSV files
#'   are the same as in the PDF file. 
#'   \itemize{
#'     \item For numerical varaible, number of observations (counts), p1, p25, 
#'     p50, p75, and p99 qunatiles, mean, SD, missing and zerorates are saved
#'     as \code{outFl}_numerical_summary.csv.
#'     \item For categorical varaible, number of observations (counts) and 
#'     categories' proportions are saved as \code{outFl}_categorical_summary.csv. 
#'     Each row is a category of a categorical (or binary) variable \code{myVar}.
#'     The row whose \code{category == 'NA'} corresponds to missing. Categories
#'     among the same variable are ordered by global prevalence in a descending 
#'     order.
#'   }
#' }
#'
#' @seealso Main function: \code{\link{vlm}}.
#' @seealso Selected supporting functions: 
#'          \code{\link{PrepData}}, 
#'          \code{\link{PrepLabels}},
#'          \code{\link{OrderByR2}}.
#'          
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @docType package
#' @name otvPlots
#' @import data.table
#' @import ggplot2
#' @importFrom grid grid.draw grid.newpage unit unit.c textGrob gpar
#' @importFrom gridExtra arrangeGrob
#' @importFrom moments skewness
#' @importFrom Hmisc wtd.quantile wtd.mean wtd.var
#' @importFrom stringi stri_trans_general
#' @importFrom scales hue_pal
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom graphics par
#' @importFrom stats lm.fit lm.wfit quantile sd var
#' @importFrom utils tail
NULL



