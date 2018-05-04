## ----code-for-3d, eval = TRUE, echo = TRUE-------------------------------
## load dataset
inflamm <- read.table('inflamm.txt',header=T)

## load uwIntroStats package
#### set warn.conflicts E so it doesn't print message about tabulate
library(uwIntroStats, warn.conflicts = FALSE)

## fit linear regression
regress('mean', diab2 ~ bmi, data = inflamm)

## ----code-for-3e,echo=T,eval=T-------------------------------------------
#### prediction by hand ####
# BMI = 15
(yhat15 <- -0.2297 + 0.01463*15)

# BMI = 30
(yhat30 <- -0.2297 + 0.01463*30)

#### prediction using the predict() function ####
# first, use lm to fit regression model
mod <- lm(diab2~bmi, data = inflamm)

# then use the predict function on new data with bmi = 15 and 30
yhats <- predict(mod, newdata = data.frame(bmi=c(15,30)))
print(yhats)

## ----define-file-names, include = FALSE----------------------------------
##### STEP 1: Update these file names #####
rmd.name <- 'key5.Rmd' # name of this file; update if you changed the name of this file
code.name <- 'hw5.R' # name of R code file; you can change if desired

## ----extract-code, eval = FALSE, include = FALSE-------------------------
## ##### STEP 2: Run these commands #####
## library('knitr')
## purl(input = rmd.name, output = code.name) # send your R code to code.name

