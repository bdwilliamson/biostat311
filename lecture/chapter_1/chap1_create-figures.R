#### Chapter 1 Examples
#### Updated: 4-3-18

#### load FEV data ####
fev <- read.table('http://www.emersonstatistics.com/datasets/fev.txt',header=T)

#### FEV vs Age ####
plot(fev$fev ~ fev$age, xlab='Age (years)',
     ylab='FEV (liters per sec)',
     main = 'FEV vs Age',
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
par(mar=c(5,4,0.5,0.5))
plot(y~x,type='l',lwd=2,col='darkred',
     ylim=c(0,6),xlim=c(0,20),
     yaxs='i',xaxs='i')

#### second plot: FEV vs x, with points ####
plot(y~x,type='l',lwd=2,col='darkred',
     ylim=c(0,6),xlim=c(0,20),
     yaxs='i',xaxs='i',
     xlab="Age (years)",
     ylab="FEV (liters per second)")
points(fev$fev~fev$age)



#### third plot: where is there an associaiton? ####
plot(y~x,type='l',lwd=2,col='darkred',
     ylim=c(0,6),xlim=c(0,20),
     yaxs='i',xaxs='i',
     xlab="Age (years)",
     ylab="FEV (liters per second)")
y1 <- 0 + 0*x
lines(y1~x,col='blue',lwd=2,lty=2)
y2 <- 3 + 0*x
lines(y2~x,col='orange',lwd=2,lty=3)
y3 <- 4.5-0.1*x
lines(y3~x,col='darkgreen',lwd=2,lty=4)
legend('top',legend=paste('Option',1:4),
       col=c('darkgreen','orange','darkred','blue'),
       lty=c(4,3,1,2),lwd=2)


#### homoskedasticity vs hetero ####
x <- rep(1:30,each=10)
## simulate constant variance
set.seed(1)
y1 <- rnorm(length(x),mean=x,sd=4)
par(mfrow=c(1,2))
plot(y1~x,xlab='X',ylab='Y',
     main='Sample from population with \n constant variance',
     pch=20)
## simulate non-constant variance
set.seed(1)
y2 <- rpois(length(x),lambda=x)
plot(y2~x,xlab='X',ylab='Y',
     main='Sample from population with \n non-constant variance',
     pch=20)
par(mfrow=c(1,1))


#### FEV vs age ###########################
library('uwIntroStats')
regress('mean', fev ~ age, data = fev)

#### Plot of simple, multiple linear regression lines ################
fig_width <- 1024
fig_height <- 1024
fig_res <- 200
png("~/Documents/Teaching/biostat311/lecture/chapter_1/plots/simple_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
plot(1, type="n", xlab="X", ylab="Y", 
     main = "Simple linear regression",
     xlim=c(0, 10), ylim=c(0, 10))
abline(a = 0, b = 1)
dev.off()

png("~/Documents/Teaching/biostat311/lecture/chapter_1/plots/multiple_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
plot(1, type="n", xlab="X", ylab="Y", 
     main = "Multiple linear regression",
     xlim=c(0, 10), ylim=c(0, 10))
mtext("(interested in X-Y association, within levels of Z)", 1, line = 4)
abline(a = 0, b = 1)
mtext("Z = 0", side = 4, at = 10.5, las = 1)
abline(a = 6, b = 1)
mtext("Z = 6", side = 3, at = 4.5, las = 1)
abline(a = 3, b = 1)
mtext("Z = 3", side = 3, at = 7.5, las = 1)
abline(a = -3, b = 1)
mtext("Z = -3", side = 4, at = 7.5, las = 1)
abline(a = -6, b = 1)
mtext("Z = -6", side = 4, at = 4.5, las = 1)
dev.off()
