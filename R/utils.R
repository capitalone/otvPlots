###########################################
#           Utility Functions             #
###########################################

#is.cntns <- function(x)  inherits(x, "cntns")
#is.dscrt <- function(x)  inherits(x, "dscrt")

wtd_quantile_NA <- function(x, weights, probs = c(.0, .25, .5, .75, 1),
                            ...) { #!# previous name: wtd.quantile_NA
  tryCatch(as.double(Hmisc::wtd.quantile(x, weights, probs,
                                         normwt = TRUE, na.rm = TRUE, ...)),
           error = function(e) rep(NA_real_, length(probs)))
}

## The color-blind friendly color palette
## Source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbbPalette <- c("#D55E00", "#009E73", "#0072B2", "#000000", "#E69F00", "#56B4E9",  "#F0E442",  "#CC79A7")
