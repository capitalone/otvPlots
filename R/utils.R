###########################################
#           Utility Functions             #
###########################################

#is.cntns <- function(x)  inherits(x, "cntns")
#is.dscrt <- function(x)  inherits(x, "dscrt")

wtd.quantile_NA <- function(x, weights, probs = c(.0, .25, .5, .75, 1),
                            ...) {
  tryCatch(as.double(Hmisc::wtd.quantile(x, weights, probs,
                                         normwt = TRUE, na.rm = TRUE, ...)),
           error = function(e) rep(NA_real_, length(probs)))
}
