# code for chap 3, meditation example

## parameters
n <- 200 # sample size
study_end <- 40 # number of weeks to observe all events
shape_placebo <- 2
scale_placebo <- 20
shape_tx <- shape_placebo
scale_tx <- 15
rate_dropout <- 30
n_weeks_in_study <- 30

## generate some times to first severe panic attack, along with treatment assignment
gen_data <- function(n, study_end, shape_placebo, scale_placebo, shape_tx, rate_tx, rate_dropout, n_weeks_in_study) {
  ## randomly assign treatment or placebo
  rows <- sample(1:n, n/2)
  tx <- rep(0, n)
  tx[rows] <- 1
  
  ## calculate event times
  event_times_placebo <- rweibull(n/2, shape_placebo, scale_placebo)
  event_times_tx <- rweibull(n/2, shape_tx, scale_tx)
  event_times <- rep(NA, n)
  event_times[rows] <- event_times_tx
  event_times[-rows] <- event_times_placebo
  
  ## calculate censoring times
  censor_times <- rexp(n, 1/rate_dropout)
  
  ## calculate enrollment times
  enroll_times <- sample(1:10, n, replace = TRUE)
  
  event_times_2 <- event_times + enroll_times
  censor_times_2 <- censor_times + enroll_times
  censor_times_3 <- pmin(censor_times_2, n_weeks_in_study)

  obs_time <- pmin(censor_times_2, event_times_2, n_weeks_in_study) - enroll_times
  
  ## compare to get events
  events <- as.numeric(event_times == obs_time)
  return(data.frame(tx = tx, event_times = event_times_2, censor_times = censor_times_3, enroll_times = enroll_times, event = events, obs_time = obs_time))
}

## generate some data
set.seed(4747)
dat <- gen_data(n, study_end, shape_placebo, scale_placebo, shape_tx, scale_tx, rate_dropout, n_weeks_in_study)
head(dat)

## get a subset of the data, for plotting
sub_tx <- subset(dat, tx == 1)[1:10, ]
sub_placebo <- subset(dat, tx == 0)[1:10, ]
sub_dat <- data.frame(tx = c(rep(1, 10), rep(0, 10)), event_times = c(sub_tx$event_times, sub_placebo$event_times),
                             censor_times = c(sub_tx$censor_times, sub_placebo$censor_times),
                             enroll_times = c(sub_tx$enroll_times, sub_placebo$enroll_times),
                            obs_times = c(sub_tx$obs_time, sub_placebo$obs_time),
                            events = c(sub_tx$event, sub_placebo$event))
## look at only some of them
paste(round(subset(sub_dat, tx == 1)$obs_times[1:8], 2), collapse = " & ")
paste(round(subset(sub_dat, tx == 0)$obs_times[1:8], 2), collapse = " & ")

fig_width <- 1024
fig_height <- 1024
fig_res <- 200


## plot with enrollment shift
png("lecture/chapter_3/figs/meditation_observed_study_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(sub_dat$event_times, 1:20, pch = 4, col = ifelse(sub_dat$tx == 1, "blue", "black"),
     axes = FALSE, ylab = "", xlab = "time since beginning of study (weeks)",
     xlim = c(0, 100))
# make lines from enrollment time to now
arrows(x0 = sub_dat$enroll_times, y0 = 1:20,
       x1 = sub_dat$event_times, y1 = 1:20,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
arrows(x0 = sub_dat$enroll_times, y0 = 1:20 - 0.35,
       x1 = sub_dat$enroll_times, y1 = 1:20 + 0.35,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
axis(side = 1, at = seq(0, 100, 10))
box()
legend("topright", legend = c("meditation", "placebo"), col = c("blue", "black"), pch = 15)
dev.off()

# plot with time since randomization
png("lecture/chapter_3/figs/meditation_observed_rand_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(sub_dat$event_times - sub_dat$enroll_times, 1:20, pch = 4, col = ifelse(sub_dat$tx == 1, "blue", "black"),
     axes = FALSE, ylab = "", xlab = "time since randomization (weeks)",
     xlim = c(0, 100))
# make lines from enrollment time to now
arrows(x0 = 0, y0 = 1:20,
       x1 = sub_dat$event_times - sub_dat$enroll_times, y1 = 1:20,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
arrows(x0 = 0, y0 = 1:20 - 0.35,
       x1 = 0, y1 = 1:20 + 0.35,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
axis(side = 1, at = seq(0, 100, 10))
box()
legend("topright", legend = c("meditation", "placebo"), col = c("blue", "black"), pch = 15)
dev.off()

## plot with censoring (circle at censoring time); since beginning of study
png("lecture/chapter_3/figs/meditation_censored_study_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
## plot event times
plot(sub_dat$event_times, 1:20, pch = 4, col = ifelse(sub_dat$tx == 1, "blue", "black"),
     axes = FALSE, ylab = "", xlab = "time since beginning of study (weeks)",
     xlim = c(0, 100))
## plot censoring times, only for those people who had one before the event
censor_first <- sub_dat$censor_times < sub_dat$event_times
points(sub_dat$censor_times[censor_first], (1:20)[censor_first], col = ifelse(sub_dat$tx == 1, "blue", "black")[censor_first])
# make lines from enrollment time to now; solid until first one
arrows(x0 = sub_dat$enroll_times, y0 = 1:20,
       x1 = pmin(sub_dat$event_times, sub_dat$censor_times), y1 = 1:20,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
arrows(x0 = sub_dat$enroll_times, y0 = 1:20 - 0.35,
       x1 = sub_dat$enroll_times, y1 = 1:20 + 0.35,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
# add dashed lines from censoring to event
arrows(x0 = sub_dat$censor_times[censor_first], y0 = (1:20)[censor_first],
       x1 = sub_dat$event_times[censor_first], y1 = (1:20)[censor_first],
       col = ifelse(sub_dat$tx == 1, "blue", "black")[censor_first],
       length = 0, lty = 2)
# line at week 30 for administrative censoring
abline(v = 30, lty = 3, lwd = 2)
axis(side = 1, at = seq(0, 100, 10))
box()
legend("topright", legend = c("meditation", "placebo"), col = c("blue", "black"), pch = 15)
dev.off()

## plot with censoring (circle at censoring time); since randomization
## plot with censoring (circle at censoring time); since beginning of study
png("lecture/chapter_3/figs/meditation_censored_rand_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
## plot event times
plot(sub_dat$event_times - sub_dat$enroll_times, 1:20, pch = 4, col = ifelse(sub_dat$tx == 1, "blue", "black"),
     axes = FALSE, ylab = "", xlab = "time since randomization (weeks)",
     xlim = c(0, 100))
## plot censoring times, only for those people who had one before the event
censor_first <- sub_dat$censor_times < sub_dat$event_times
points(sub_dat$censor_times[censor_first] - sub_dat$enroll_times[censor_first], (1:20)[censor_first], col = ifelse(sub_dat$tx == 1, "blue", "black")[censor_first])
# make lines from enrollment time to now; solid until first one
arrows(x0 = 0, y0 = 1:20,
       x1 = pmin(sub_dat$event_times, sub_dat$censor_times) - sub_dat$enroll_times, y1 = 1:20,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
arrows(x0 = 0, y0 = 1:20 - 0.35,
       x1 = 0, y1 = 1:20 + 0.35,
       col = ifelse(sub_dat$tx == 1, "blue", "black"),
       length = 0)
# add dashed lines from censoring to event
arrows(x0 = sub_dat$censor_times[censor_first] - sub_dat$enroll_times[censor_first], y0 = (1:20)[censor_first],
       x1 = sub_dat$event_times[censor_first] - sub_dat$enroll_times[censor_first], y1 = (1:20)[censor_first],
       col = ifelse(sub_dat$tx == 1, "blue", "black")[censor_first],
       length = 0, lty = 2)
# line at week 30 for administrative censoring
axis(side = 1, at = seq(0, 100, 10))
box()
legend("topright", legend = c("meditation", "placebo"), col = c("blue", "black"), pch = 15)
dev.off()

## look at only some of them
sub_dat$survobj <- survival::Surv(sub_dat$obs_times, sub_dat$events)
library(survival)
paste(print(subset(sub_dat, tx == 1)$survobj[1:8], digits = 2), collapse = " & ")
paste(print(subset(sub_dat, tx == 0)$survobj[1:8], digits = 2), collapse = " & ")

## density curve for overall, uncensored, censored
set.seed(5555)
target_pop <- rweibull(100000, 2, 20)
censoring <- rexp(100000, 1/rate_dropout) 

f_uncens<-function(x) return(dweibull(x,2,20)*(1-pexp(x,1/rate_dropout))/integrate(function(u) dweibull(u,2,20)*(1-pexp(u,1/rate_dropout)),0,+Inf)$value)
f_cens<-function(x) return(dweibull(x,2,20)*pexp(x,1/rate_dropout)/integrate(function(u) dweibull(u,2,20)*pexp(u,1/rate_dropout),0,+Inf)$value)

overall_mean <- 20*gamma(1+1/2)
uncensored_mean <- integrate(function(x) x*f_uncens(x),0,Inf)$value
censored_mean <- integrate(function(x) x*f_cens(x),0,Inf)$value

png("lecture/chapter_3/figs/meditation_density_versus_obs_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
curve(f_uncens,0,60,col=2,lwd=1.5,
      xlab="time from recruitment until severe panic attack (in weeks)",
      ylab="density function",cex.lab=0.8,cex.axis=0.8,ylim=c(0,0.05))
curve(f_cens,add=T,col=4,lwd=1.5,cex=0.8)
curve(dweibull(x,2,20),add=T,lwd=1.5,cex=0.8)
legend("topright", 
       legend = c("target population", "uncensored participants", "censored participants", "mean value"),
       col = c("black", "red", "blue", "black"),
       lty = c(1, 1, 1, 2))
abline(v = overall_mean, lty = 2)
abline(v = censored_mean, lty = 2, col = "blue")
abline(v = uncensored_mean, lty = 2, col = "red")
dev.off()


## plot of survival density function
png("lecture/chapter_3/survival_density.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1), mfrow = c(2, 1))
curve(dweibull(x,2,20),0,60,ylab="f(t)",xlab="",ylim=c(0,0.05),lwd=2.5,cex.axis=0.8,cex.lab=0.8)

curve(dweibull(x,2,20),0,60,ylab="f(t)",xlab="",ylim=c(0,0.05),lwd=2.5,cex.axis=0.8,cex.lab=0.8)
points(c(20,20),c(0,dweibull(20,2,20)),type='l',lwd=2.5,col=2)
points(c(20,20),c(dweibull(20,2,20),1),type='l',lty=3,lwd=2.5,col=2)
m<-50; grid<-seq(20,60,length.out=m);
for(i in 1:m){
  points(c(grid[i],grid[i]),c(0,dweibull(grid[i],2,20)),type='l',lwd=2,col=2)	
  
}
dev.off()

## plot of Survival function
png("lecture/chapter_3/survival_function.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
curve(1-pweibull(x,2,20),0,60,ylab="S(t)",xlab="t",ylim=c(0,1),lwd=2.5,cex.axis=0.8,cex.lab=0.8)
points(c(20,20),c(-1,0.37),type='l',lwd=2,col=2,lty=3)
points(c(-5,20),c(0.37,0.37),type='l',lwd=2,col=2,lty=3)
dev.off()

## plot of hazard
png("lecture/chapter_3/hazard_function.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1), mfrow = c(2, 1))
curve(dweibull(x,2,20)/(1-pweibull(x,2,20)),0,60,ylab=expression(paste(lambda,"(t)")),xlab="",lwd=2.5,cex.axis=0.8,cex.lab=0.8)
curve(-log(1-pweibull(x,2,20)),0,60,ylab=expression(paste(Lambda,"(t)")),xlab="t",ylim=c(0,10),lwd=2.5,cex.axis=0.8,cex.lab=0.8)
dev.off()

## plots of risk set
delta <- sub_dat$events
yobs <- sub_dat$obs_times
y <- sub_dat$event_times - sub_dat$enroll_times
t <- sub_dat$tx

png("lecture/chapter_3/figs/risk_set_movie_0.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(0,0)*1,c(-1,23),type='l',lty=1,lwd=2,col="red")
text(32.2,12,"R(0) = all",cex=0.8,col = "red")
text(32.2,10.5,"size of R(0) = 20",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_1.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(3,3)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(3) = all but {5,8}",cex=0.8,col = "red")
text(32.2,10.5,"size of R(3) = 18",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_2.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(5,5)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(5) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(5) = ......",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_3.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(8,8)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(8) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(8) = ......",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_4.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(12,12)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(12) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(12) = ......",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_5.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(20,20)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(20) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(20) = ......",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_6.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(25,25)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(25) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(25) = ......",cex=0.8,col = "red")
dev.off()

png("lecture/chapter_3/figs/risk_set_movie_6.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(NULL,xlim=c(0,40),ylim=c(0,22),xlab="time since treatment assignment (in weeks)",ylab="",yaxt='n',cex.lab=0.8,cex.axis=0.8)
for(i in 1:length(y)){
  if(t[i]==1){
    if(delta[i]==1){
      points(c(0,y[i]),c(i,i),type='l',lty=1,col="blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(0+y[i],i,type='p',pch=4,cex=.8)
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "blue")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "blue")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "blue")
      points(y[i],i,type='p',pch=4,cex=.8,col = "blue")
      points(yobs[i],i,type='p',pch=1,cex=.8)			
    }
  }
  if(t[i]==0){
    if(delta[i]==1){
      points(0+c(0,y[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(0+y[i],i,type='p',pch=4,cex=.8,col = "black")
    }
    if(delta[i]==0){
      points(c(0,yobs[i]),c(i,i),type='l',lty=1,col = "black")
      points(c(yobs[i],y[i]),c(i,i),type='l',lty=3,col = "black")
      points(c(0,0),c(i,i)+c(-0.2,0.2),type='l',lty=1,col = "black")
      points(y[i],i,type='p',pch=4,cex=.8,col = "black")
      points(yobs[i],i,type='p',pch=1,cex=.8,col = "black")			
    }
  }
}
for(i in 20:11){
  text(-0.9,i,i,cex=0.5)	
}
for(i in 10:1){
  text(-0.9,i,i,cex=0.5)	
}
legend("topright",legend=c("meditation","placebo"),fill=c(4,1),cex=0.8)

points(c(30,30)*1,c(-1,23),type='l',lty=1,lwd=2,col = "red")
text(32.2,12,"R(30) = ................................",cex=0.8,col = "red")
text(32.2,10.5,"size of R(30) = ......",cex=0.8,col = "red")
dev.off()

## risk set
png("lecture/chapter_3/figs/risk_set.png", width = fig_width, height = fig_height/5*4, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
riskset<-function(u) mean(yobs>=u)
riskset<-Vectorize(riskset)
curve(riskset,0,60,n=10000,ylab="fraction-at-risk at time t",xlab="t",ylim=c(0,1),lwd=2,cex.axis=0.8,cex.lab=0.8,col=4)
Sy<-function(x) (0.5*pweibull(x,2,15,lower.tail=FALSE)+0.5*pweibull(x,2,20,lower.tail=FALSE))*pexp(x,1/40,lower.tail=FALSE)
curve(Sy,add=TRUE,col=1,lwd=2)
legend("topright",legend=c("observed fraction-at-risk ","expected fraction-at-risk "),fill=c(4,1),cex=0.8)
dev.off()

## survival function
png("lecture/chapter_3/figs/survival_function.png", width = fig_width*1.5, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
curve(1-pweibull(x,2,20),0,60,ylab="S(t)",xlab="t",ylim=c(0,1),lwd=2.5,cex.axis=0.8,cex.lab=0.8)
points(c(20,20),c(-1,0.37),type='l',lwd=2,col=2,lty=3)
points(c(-5,20),c(0.37,0.37),type='l',lwd=2,col=2,lty=3)
dev.off()

## hazard and cumulative hazard
png("lecture/chapter_3/figs/hazard_function.png", width = fig_width*1.5, height = fig_height*2/3, units = "px", res = fig_res)
par(mar = c(2, 4, 0.1, 0.1))
curve(dweibull(x,2,20)/pweibull(x,2,20,lower.tail=FALSE),from=0,to=60,ylab="h(t)",cex.axis=0.8,cex.lab=0.8,lwd=2,xlab="")
dev.off()

png("lecture/chapter_3/figs/cumulative_hazard_function.png", width = fig_width*1.5, height = fig_height*2/3, units = "px", res = fig_res)
par(mar = c(2, 4, 0.1, 0.1))
curve(-log(pweibull(x,2,20,lower.tail=FALSE)),from=0,to=60,ylab="H(t)",cex.axis=0.8,cex.lab=0.8,lwd=2,ylim=c(0,10),xlab="")
dev.off()