#### Chapter 0 Examples
#### Updated: 3-27-18

#### load FEV data ####
fev <- read.table('http://www.emersonstatistics.com/datasets/fev.txt',header=T)

#### histogram of age ####
hist(fev$age, xlab="Age (years)", ylab="Frequency", main="Histogram of Age")

#### boxplot of age ####
boxplot(fev$age, xlab="", ylab="Age (years)", main="Boxplot of Age")

#### barplot of smoking ####
(smoke_count <- table(fev$smoke))
barplot(smoke_count,xlab='Smoking (1 = yes, 2 = no)',ylab='Count',main='Bar Plot of Smoking')

#### stratified age vs smoking ####
# boxplot
boxplot(fev$age~fev$smoke, xlab='Smoking (1=yes, 2=no)', ylab='Age (years)', main = 'Boxplot of Age by Smoking')

# histogram
par(mfrow=c(1,2))
hist(subset(fev,smoke==1)$age, xlab="Age (years)", ylab="Frequency", main="Histogram of Age (Smokers)")
hist(subset(fev,smoke==2)$age, xlab="Age (years)", ylab="Frequency", main="Histogram of Age (Non-Smokers)")
par(mfrow=c(1,1))

#### scatterplot ####
plot(fev$fev ~ fev$age, xlab='Age (years)',ylab='FEV (liters per sec)', main = 'Scatterplot of FEV vs Age')
