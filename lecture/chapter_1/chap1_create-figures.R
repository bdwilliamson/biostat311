#### Chapter 1 Examples
#### Updated: 4-3-18

#### load FEV data ####
fev <- read.table('http://www.emersonstatistics.com/datasets/fev.txt',header=T)

#### FEV vs Age ####
plot(fev$fev ~ fev$age, xlab='Age (years)',
     ylab='FEV (liters per sec)',
     main = 'Scatterplot of FEV vs Age',
     ylim=c(0,6),xlim=c(0,20),
     yaxs="i",xaxs="i")
abline(with(fev,lm(fev~age)),col='darkred',lwd=2)

#### Plot line without data ####
mod <- lm(fev~age, data=fev)
x <- seq(0,20,5)
y <- predict(mod,newdata = data.frame(age=x))
plot(y~x,type='l',lwd=2,col='darkred',
    ylim=c(0,6),xlim=c(0,20),
    yaxs='i',xaxs='i')
## add points to data
points(fev$fev~fev$age)


#### first plot: y vs x, line only ####
plot(y~x,type='l',lwd=2,col='darkred',
     ylim=c(0,6),xlim=c(0,20),
     yaxs='i',xaxs='i')
