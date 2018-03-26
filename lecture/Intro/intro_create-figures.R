### Day 1 Examples

#### load FEV data ####
fev <- read.table('http://www.emersonstatistics.com/datasets/fev.txt',header=T)

#### create age category ####
fev$teen <- with(fev,ifelse(age >= 13,'Teen (13 +)','Pre-teen (< 13)'))

#### boxplot FEV vs teen ####
par(mar=c(5,4,2,1))
with(fev,boxplot(fev~teen,ylab='FEV (liters per second)',xlab='Age Group'))

#### scatterplot FEV vs age ####
with(fev,plot(fev~jitter(age,1.5),ylab='FEV (liters per second)',
              xlab='Age (years)',ylim=c(0,8),pch=20))
with(fev,abline(lm(fev~age),col='darkred',lwd=2))
