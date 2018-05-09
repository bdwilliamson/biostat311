# code for chap 3, meditation example

## parameters
n <- 200 # sample size
study_end <- 40 # number of weeks to observe all events
rate_placebo <- 20 # panic attacks/week
rate_tx <- rate_placebo*2
rate_dropout <- 30
n_weeks_in_study <- 30

## generate some times to first severe panic attack, along with treatment assignment
gen_data <- function(n, study_end, rate_placebo, rate_tx, rate_dropout, n_weeks_in_study) {
  ## randomly assign treatment or placebo
  rows <- sample(1:n, n/2)
  tx <- rep(0, n)
  tx[rows] <- 1
  
  ## calculate event times
  event_times <- rexp(n, (1/rate_placebo)^(1-tx)*(1/rate_tx)^(tx)) # weeks
  
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
dat <- gen_data(n, study_end, rate_placebo, rate_tx, rate_dropout, n_weeks_in_study)
head(dat)

## look at only some of them
paste(round(subset(dat, tx == 1)$event_times[1:8] - subset(dat, tx == 1)$enroll_times[1:8], 2), collapse = " & ")
paste(round(subset(dat, tx == 0)$event_times[1:8] - subset(dat, tx == 0)$enroll_times[1:8], 2), collapse = " & ")

## get a subset of the data, for plotting
sub_tx <- subset(dat, tx == 1)[1:10, ]
sub_placebo <- subset(dat, tx == 0)[1:10, ]
sub_dat <- data.frame(tx = c(rep(1, 10), rep(0, 10)), event_times = c(sub_tx$event_times, sub_placebo$event_times),
                             censor_times = c(sub_tx$censor_times, sub_placebo$censor_times),
                             enroll_times = c(sub_tx$enroll_times, sub_placebo$enroll_times),
                            obs_times = c(sub_tx$obs_time, sub_placebo$obs_time),
                            events = c(sub_tx$event, sub_placebo$event))

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
rate_target <- (1/rate_tx + 1/rate_placebo)/2
target_pop <- rexp(100000, rate_target)
censoring <- rexp(100000, 1/rate_dropout) 

f_uncens<-function(x) return(dexp(x,rate_target)*(1-pexp(x,1/rate_dropout))/integrate(function(u) dexp(u,rate_target)*(1-pexp(u,1/rate_dropout)),0,+Inf)$value)
f_cens<-function(x) return(dexp(x,rate_target)*pexp(x,1/rate_dropout)/integrate(function(u) dexp(u,rate_target)*pexp(u,1/rate_dropout),0,+Inf)$value)

overall_mean <- 1/((1/rate_tx + 1/rate_placebo)/2)
censored_mean <- integrate(function(x) x*f_uncens(x),0,Inf)$value
uncensored_mean <- integrate(function(x) x*f_cens(x),0,Inf)$value

png("lecture/chapter_3/figs/meditation_density_versus_obs_time.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
curve(f_uncens,0,60,col=2,lwd=1.5,xlab="time from recruitment until severe panic attack (in weeks)",
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
