#### Chapter 2 R Code
#### Updated: 5-3-18

#### Define functions ########
logit <- function(x) log(x/(1-x))
expit <- function(x) exp(x)/(1+exp(x))
  
#### Simulate Data ###########
set.seed(1)
x <- runif(200,1,10)
logit.p <- -4 + 1*x
p <- expit(logit.p)
y <- rbinom(length(logit.p),1,p)

plot(y~x,ylim=c(-0.2,1.4),xlim=c(0,11))

## linear regression
mod1 <- lm(y~x)
abline(mod1,col='darkred',lwd=2)

## logistic regression
mod2 <- glm(y~x,family=binomial)
pred2 <- data.frame(x=seq(0,11,length=1000))
pred2$y <- predict(mod2,newdata=pred2,type='response')
fit2 <- data.frame(x=x,yhat=fitted(mod2))
fit2 <- fit2[order(x),]
lines(pred2$y~pred2$x,col='blue',lwd=2)



#### Analyze Pregnancy Data ####

## read data
setwd("P:/Documents/2017-2018/Spring 2018/BIOST 311/Lectures/Chapter 2 (Logistic Regression)")
preg <- read.table('data/pregout.txt',header=T,stringsAsFactors=F)

## create new LBW variable (<2500 g)
preg$lbw <- ifelse(preg$bweight < 2500,1,0)

## re-code smoker (1 = yes, 2 = no)
preg$smoker <- ifelse(preg$smoker==1,1,0)

## re-code infant sex (1 = boy, 2 = girl)
preg$boy <- ifelse(preg$sex==1,1,0)

## fit regression model
library(uwIntroStats)
regress('odds',lbw~smoker+age+ht+parity+boy,data=preg)
