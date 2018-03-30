## R script for chapter 1, section 2: multiple linear regression
## Created 30 March 2018 by Brian Williamson

## --------------------------------------------------------------------
## Examples of confounders, precision variables, and effect modifiers
## --------------------------------------------------------------------

## data generating mechanism for two predictors, one is a confounder
data_func_confound <- function(beta0, beta1, beta2, n) {
  ## generate some X data: X1 is continuous, X2 is binary
  x1 <- runif(n, 0, 1)
  x2 <- sample(0:1, n, replace = TRUE)
  
  ## the line in those with x2 == 0
  y_0 <- beta0 + beta2*x1[x2 == 0] + rnorm(sum(x2 == 0), 0, 1)
  ## for x2 == 1
  y_1 <- beta0 + beta2*x1[x2 == 1] + rnorm(sum(x2 == 1), 0, 1)
  
  ## create the full outcome, put in the correct order
  y <- c(y_0, y_1)
}
## data generating mechanism for two predictors, one is a precision variable

## data generating mechanism for two predictors, one is an effect modifier

