###########################################
#        Plots for Numerical Data         #
###########################################

#' Boxplots and overtime plots of a numerical variable
#'
#' @inheritParams PrepData
#' @param myVar The name of the variable to be plotted
#' @param skewOpt Either a numeric constant or \code{NULL}. If numeric, say 5,
#'   the box plots of variables whose skewness exceeds 5 will be on a log10
#'   scale if possible. Default is \code{NULL} (no transformation).
#' @param kSample Either \code{NULL} or an integer. If an integer, indicates the
#'   sample size for both drawing boxplots and ordering numerical graphs by 
#'   \eqn{R^2}. For large datasets, setting this to a reasonable (say 50K) value
#'   dramatically increases processing speed. In larger datasets (e.g. > 10 
#'   percent system memory), this parameter should not be set to NULL, or 
#'   boxplots may take a very long time to render. This setting has no impact on
#'   the accuracy of time series plots.
#' @export
#' @return a \code{grob} (i.e., \code{ggplot} grid) object, including a boxplot
#'   grouped by \code{dateGpBp}, a time series of p1, p50 and p99 grouped by 
#'   \code{dateGp}, a time series of mean and +-1 SD control limits grouped by
#'   \code{dateGp}, and a time series of missing and zerorates grouped by 
#'   \code{dateGp}.
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "years")
#' plot(PlotNumVar("balance", bankData, NULL, "months", "years", 
#'                  skewOpt = NULL, kSample = NULL))
#'
PlotNumVar <- function(myVar, dataFl, weightNm, dateGp, dateGpBp,
                        skewOpt = NULL, kSample = 50000) { #!# previous name: PlotContVar
  variable <- NULL
  if (inherits(myVar, "integer64")) {
    stop("Cannot plot integer64 type--cast to numeric")
  }
  meltdx <- SummaryStats(myVar = myVar, dataFl = dataFl, dateGp = dateGp,
                         weightNm = weightNm)
  
  # option for log10 transform of box plot y axis if skewness is high enough.
  # Invalid choices revert to 3.
  if (!is.null(skewOpt)) {
    stopifnot(is.numeric(skewOpt))
    if (skewOpt < 0) {
      skewOpt <- 3
    }
  }
  
  if (!is.null(kSample)) {
    # take a subsample of dataFl for boxplots
    p1 <- PlotDist(dataFl[sample(.N, min(.N, kSample))], myVar, dateGpBp,
                   weightNm, skewOpt)
  } else {
    p1 <- PlotDist(dataFl, myVar, dateGpBp, weightNm, skewOpt)
  }
  p2 <- PlotQuantiles(meltdx[variable %in% c("p99", "p50", "p1", "p99_g",
                                             "p50_g", "p1_g")], myVar, dateGp)
  p3 <- PlotMean(meltdx[variable %in% c("Mean", "cl1", "cl2")], myVar, dateGp)
  p4 <- PlotRates(meltdx, myVar, dateGp)
  p5 <- rbind(ggplot2::ggplotGrob(p2), ggplot2::ggplotGrob(p3),
              ggplot2::ggplotGrob(p4), size = "last")
  p  <- gridExtra::arrangeGrob(p1, p5,
                               layout_matrix = cbind(c(1, 1, 1), c(5, 5, 5)),
                               widths = 1:2)
  return(p)
}


###########################################
#     Continuous Plotting Functions       #
###########################################

#' Create summary statistics for a numerical variable
#' 
#' @inheritParams PrepData
#' @inheritParams PlotNumVar
#' @export
#' @return A \code{data.table} for use by the numerical variables' plotting 
#'   funtions \code{\link{PlotMean}}, \code{\link{PlotQuantiles}}, and 
#'   \code{\link{PlotRates}}.
#' @importFrom Hmisc wtd.mean
#' @importFrom Hmisc wtd.quantile
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "quarters", dateGpBp = "years")
#' mdx = SummaryStats(myVar = "age", dataFl = bankData, dateGp = "quarters")
#' plot(PlotQuantiles(mdx[variable %in% c("p99", "p50", "p1", "p99_g", "p50_g", "p1_g")], 
#'                    "age", "quarters"))
#' plot(PlotMean(mdx[variable %in% c("Mean", "cl1", "cl2")], "age", "quarters"))
#' plot(PlotRates(mdx, "age", "quarters"))

SummaryStats <- function(myVar, dataFl, dateGp, weightNm = NULL) {
  
  variable <- NULL
  
  if (!is.null(weightNm)) {   ## If weights are available
    ## Compute a data.table of the summary statistics, grouped by dataGp
    dx <- dataFl[, {
      tmp1 <- wtd_quantile_NA(get(myVar), get(weightNm), c(.01, .5, .99));
      list(
        "p1"   = tmp1[1],
        "p50"  = tmp1[2],
        "p99"  = tmp1[3],
        "Mean" = as.double(Hmisc::wtd.mean(get(myVar), get(weightNm),
                                           normwt = TRUE, na.rm = TRUE)),
        "zerorate"    = Hmisc::wtd.mean(get(myVar) == 0, get(weightNm),
                                        na.rm = TRUE, normwt = TRUE),
        "missingrate" = Hmisc::wtd.mean(is.na(get(myVar)),
                                        get(weightNm), normwt = TRUE)
      )}, by = c(dateGp)]
    
    ## Compute summary stats for the overall data (no group by)
    qq <- dataFl[, wtd_quantile_NA(get(myVar), get(weightNm),
                                   probs = c(.99, .5, .01))]
    cl <- dataFl[, c(Hmisc::wtd.mean(get(myVar), get(weightNm),
                                     na.rm = TRUE, normwt = TRUE),
                     sqrt(Hmisc::wtd.var(get(myVar), get(weightNm),
                                         na.rm = TRUE, normwt = TRUE)))]
  } else { ## If no weights are provided
    ## Compute a data.table of the summary statistics, grouped by dataGp
    dx <- dataFl[, {
      tmp1 <- quantile(get(myVar), probs = c(.01, .5, .99), na.rm = TRUE);
      list(
        "p1"  = tmp1[1],
        "p50" = tmp1[2],
        "p99" = tmp1[3],
        "Mean"        = as.double(mean(get(myVar), na.rm = TRUE)),
        "zerorate"    = mean(get(myVar) == 0, na.rm = TRUE),
        "missingrate" = mean(is.na(get(myVar)))
      )}, by = c(dateGp)]

    ## Compute summary stats for the overall data (no group by)
    qq <- dataFl[, quantile(get(myVar), probs = c(.99, .5, .01), na.rm = TRUE)]
    cl <- dataFl[, c(mean(get(myVar), na.rm = TRUE),
                     sd(get(myVar), na.rm = TRUE))]
  }
  
  ## Melt the dx table to have only 3 columns: dateGp, variable, and value
  meltdx <- data.table::melt(dx,
                             id.vars = c(dateGp),
                             measure.vars = c("p99", "p50", "p1", "Mean",
                                              "zerorate", "missingrate")
  )
  ## Mean +- 1 SD
  cl <- cl %*% matrix(c(1, 1, 1, -1), byrow = TRUE, nrow = 2) 

  ## The gloabl summary qq now has 0.99, 0.5, 0.1 quantiles, mean+SD, and mean-SD
  qq <- c(qq, cl)
  ## 5 copies of dateGp for 5 global summary variables
  globaldx <- data.table(dateGp = rep(meltdx[variable == "p99", dateGp,
                                             with = FALSE][[1]], 5))
  ## Make the global stats in the same format of meltdx
  globaldx[, c("variable", "value") := list(rep(c("p99_g", "p50_g",
                                                  "p1_g", "cl1", "cl2"),
                                                each = .N / 5),
                                            rep(qq, each = .N / 5))]
  
  ## Merge stats by dateGp (meltdx) and global stats in one table
  meltdx <- rbindlist(list(meltdx, globaldx))
  return(meltdx)
}



#' Plot 01, 50, and 99 percentile for a numerical variable
#'
#' @param meltdx A data.table with p1, p50, and p99 in long format, produced by
#' \code{\link{SummaryStats}}
#' @inheritParams PrepData
#' @inheritParams PlotNumVar
#' @return A \code{ggplot2} object with \code{dateGp} on the x axis, 
#'   \code{value} on the y axis, and variables \code{p01}, \code{p50}, and 
#'   \code{p99} plotted on the same graph, with grouped and global percentiles 
#'   differentiated by linetype.
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @export

PlotQuantiles <- function(meltdx, myVar, dateGp) {
  
  variable <- gp <- group <- NULL
  
  ## Add a column 'group' to indictor whether each stat is 'by dateGp' or 'global'.
  meltdx[, "group" := as.factor(ifelse(variable %in% c("p99", "p50", "p1"),
                                       gsub('.{1}$', '', paste("by", dateGp, sep = " ")), 
                                       "global"))]
  meltdx[, variable := droplevels(variable)]
  ## Change variable names to just quantiles
  levels(meltdx$variable) <- list(p99 = c("p99", "p99_g"),
                                  p50 = c("p50", "p50_g"),
                                  p1 = c("p1", "p1_g"))
  meltdx[, gp := paste(variable, group)]
  

  ## Create a ggplots2 object
  ggplot2::ggplot(meltdx, ggplot2::aes_string(x = dateGp, y = "value",
                                              colour = "variable", lty = "group",
                                              group = "gp")) +
    ggplot2::geom_line() + ggplot2::ylab(NULL) + 
    ggplot2::scale_colour_manual(values = cbbPalette)
  
}


#' Plot mean with {Mean +- 1SD} control limits for a numerical variable
#' 
#' @param meltdx A \code{data.table} with Mean and 1SD control limits in long format, 
#' produced by \code{\link{SummaryStats}}
#' @inheritParams PrepData
#' @inheritParams PlotNumVar
#' @return A \code{ggplot2} object with \code{dateGp} on the x axis, 
#'   \code{value} on the y axis, and variables \code{Mean}, \code{cl1}, and 
#'   \code{cl2} plotted on the same graph, with mean and control limits 
#'   differentiated by linetype.
#' @export
#' @section License: 
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.

PlotMean <- function(meltdx, myVar, dateGp){
  
  variable <- NULL
  
  ## Modify the column variable to be either 'mean' or '1SD CL'
  setnames(meltdx, "variable", "var")
  meltdx[, variable := as.factor(ifelse(var != "Mean", "1SD CL", "mean"))]
  
  ## Create a ggplots2 object
  ggplot2::ggplot(meltdx,
                  ggplot2::aes_string(x = dateGp, y = "value", group = "var",
                                      linetype = "variable")) +
    ggplot2::geom_line(colour = "black") +
    ggplot2::scale_linetype_manual(values = c(2, 1),
                                   breaks = c("mean", "1SD CL")) +
    ggplot2::ylab(NULL)
}


#' Plot zero and missing rates for a numerical variable
#'
#' @param meltdx A \code{data.table} with missing rate and zero rate in long
#'   format, produced by \code{\link{SummaryStats}}
#' @inheritParams PrepData
#' @inheritParams PlotNumVar
#' @export
#' @return A \code{ggplot2} object with a \code{missingrate} and
#'   \code{zerorate} grouped by \code{dateGp}.
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.

PlotRates <- function(meltdx, myVar, dateGp) {
  
  variable <- NULL
  ggplot2::ggplot(meltdx[variable %in% c("zerorate", "missingrate")],
                  ggplot2::aes_string(x = dateGp,
                                      y = "value",
                                      colour = "variable",
                                      group = "variable")) +
    ggplot2::geom_line() + ggplot2::ylab(NULL) +
    ggplot2::scale_colour_manual(values = cbbPalette)
}

#' Side-by-side box plots, for a numerical variable,  grouped by \code{dateGpBp}
#' 
#' For a variable is all positive (no zeros) and has larger than 50 all distinct
#' values, if it is highly skewed, then all box plots can be plotted under the 
#' log base 10 transformation. See the argument \code{skewOpt} for details.
#'
#' @inheritParams PrepData
#' @inheritParams PlotNumVar
#' @return A \code{ggplot2} object with a box plot of \code{myVar} grouped by 
#'   \code{dateGpBp}
#' @export
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(dataFl = bankData, dateNm = "date", dateGp = "months", 
#'          dateGpBp = "quarters")
#' PlotDist(dataFl = bankData, myVar = "balance", dateGpBp = "quarters")
#' # The following attempt to log transform will fail due to negative values,
#' # and the untransformed version will be returned
#' PlotDist(dataFl = bankData, myVar = "balance", dateGpBp = "quarters", 
#'          skewOpt = 3)
#' # This attempt should succeed, as the skew exceeds 3 and there are no 
#' # negative values
#' PlotDist(dataFl = bankData, myVar = "duration", dateGpBp = "quarters",
#'          skewOpt = 3)
          
PlotDist <- function(dataFl, myVar, dateGpBp, weightNm = NULL, skewOpt = NULL){
  
  ## Set key
  setkeyv(dataFl, dateGpBp)
  
  ## First layer of the graph
  if (is.null(weightNm)) {
    p <- ggplot2::ggplot(dataFl, ggplot2::aes_string(x = dateGpBp,
                                                     y = myVar,
                                                     group = dateGpBp))
  } else {
    p <- ggplot2::ggplot(dataFl, ggplot2::aes_string(
      x = dateGpBp, y = myVar, group = dateGpBp, weight = weightNm))
  }
  
  ## Create side-by-side box plots, with a rug plot in the margin
  p <- p + ggplot2::geom_boxplot() + ggplot2::ylab(myVar) +
    ggplot2::scale_y_continuous() +
    ggplot2::geom_rug(data = dataFl,
                      mapping = ggplot2::aes_string(x = dateGpBp,
                                                    y = myVar),
                      sides = "l", position = "jitter", inherit.aes = FALSE,
                      colour = "#F8766D", alpha = .4)
  
  # log10 transform of highly skewed variable, only if variable is all positive
  # and has a large number (>50) of unique values
  if (!is.null(skewOpt)) {
    M <- min(dataFl[, myVar, with = FALSE], na.rm = TRUE)
    if (M <= 0) {
      message("The Range of ", myVar, " includes negative values or zero, 
              returning untransformed boxplots.")
    } else {
      if (dim(unique(dataFl[, myVar, with = FALSE]))[1] > 50){
        if (moments::skewness(dataFl[, myVar, with = FALSE],
                              na.rm = TRUE) > skewOpt){
          p2 <- try(p + ggplot2::scale_y_log10() +
                      ggplot2::ylab(paste(myVar, " (log10)")))
          if (inherits(p2, "try-error")) {
            message(paste("Log transform failed, returning untransformed boxplot
                          of ", myVar))
          } else {
            p <- p2
          }
        }
        }
      }
    }
  return(p)
}