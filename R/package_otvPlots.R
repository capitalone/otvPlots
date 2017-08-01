#' Over time variable plots for predictive modeling (otvPlots)
#'
#' The \code{otvPlots} package uses \code{data.table} and \code{ggplot2} packages to 
#' efficiently plot time series aggregated from large datasets. Plots of numerial
#' variables are optionally returned ordered by correlation with date -- a 
#' natural starting point for anomaly detection. Plots are automatically labeled
#' if a variable dictionary is provided. Categorical and numerical variables are
#' handled automatically. 
#'
#' @seealso \code{\link{PrepData}}
#' @seealso \code{\link{PrepLabels}}
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



