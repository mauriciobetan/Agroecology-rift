## useful_functions.R

#Put any functions that might be used across multiple scripts here so that they can be properly sourced in.
## Function for texreg

convertpanelAR <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(summary(model)$coef),
    coef = summary(model)$coef[,1],
    se = summary(model)$coef[,2],
    pvalues = summary(model)$coef[,4],
    gof.names = c("R-squared", "n"),
    gof = c(model$r2, length(model$resid))
  )
}
