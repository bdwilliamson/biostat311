## ----code-for-prob-1b, include = TRUE------------------------------------
## load the FEV data
fev <- read.delim(file = "fev.txt", sep = "", header = TRUE)
## look at the first five rows
head(fev, 5)

## ----install-uwIntroStats, eval = FALSE----------------------------------
## install.packages("uwIntroStats")

## ----code-for-prob-1c, eval = TRUE, warning = FALSE, message = FALSE-----
# load uwIntroStats package
library('uwIntroStats')
# create descrip table for FEV
descrip(fev$fev, strata=fev$smoke)

## ----code-for-prob-1d, fig.height=3, fig.width=6, eval = TRUE------------
# tell R to arrange plots in one row, 2 columns
par(mfrow=c(1,2)) 
# histogram of fev among smokers (smoke==1)
hist(subset(fev,smoke==1)$fev, xlab='FEV (l/sec)', main="Histogram (smokers)") 
# histogram of fev among nonsmokers (smoke==2)
hist(subset(fev,smoke==2)$fev, xlab='FEV (l/sec)', main="Histogram (nonsmokers)") 

## ----code-for-prob-1e, eval=TRUE-----------------------------------------
ttest(fev$fev, by = fev$smoke, var.eq = TRUE)

## ----codde-for-prob-1f, eval=TRUE----------------------------------------
ttest(fev$fev, by = fev$smoke, var.eq = FALSE)

## ----define-file-names, include = FALSE----------------------------------
##### STEP 1: Update these file names #####
rmd.name <- 'key2.Rmd' # name of this file; update if you changed the name of this file
code.name <- 'hw2.R' # name of R code file; you can change if desired

## ----extract-code, eval = FALSE, include = FALSE-------------------------
## ##### STEP 2: Run these commands #####
## library('knitr')
## purl(input = rmd.name, output = code.name) # send your R code to code.name

