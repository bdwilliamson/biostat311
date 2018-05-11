## code for HW 7

## generate data 
gen_data <- function(n, rate_placebo, rate_tx, rate_dropout, n_days_in_study) {
  ## randomly assign treatment or placebo
  rows <- sample(1:n, n/2)
  tx <- rep(0, n)
  tx[rows] <- 1
  
  ## calculate event times
  event_times_placebo <- rexp(n/2, 1/rate_placebo)*365
  event_times_tx <- rexp(n/2, 1/rate_tx)*365
  event_times <- rep(NA, n)
  event_times[rows] <- event_times_tx
  event_times[-rows] <- event_times_placebo

  ## calculate censoring times
  censor_times <- rexp(n, 1/rate_dropout)*365 + 180
  
  ## calculate enrollment times
  enroll_times <- sample(1:90, n, replace = TRUE)
  
  event_times_2 <- event_times + enroll_times
  censor_times_2 <- censor_times + enroll_times
  censor_times_3 <- pmin(censor_times_2, n_days_in_study)
  
  obs_time <- pmin(censor_times_2, event_times_2, n_days_in_study) - enroll_times
  
  ## compare to get events
  events <- as.numeric(event_times < censor_times_3 - enroll_times)
  return(data.frame(tx = tx, event_times = event_times_2, censor_times = censor_times_3, enroll_times = enroll_times, event = events, obs_time = obs_time))
}

## parameters for leukemia example
n <- 1000
rate_tx <- 1
rate_placebo <- 0.5
rate_dropout <- 0.2
n_days_in_study <- 365*3

set.seed(4747)
dat <- gen_data(n, rate_placebo, rate_tx, rate_dropout, n_days_in_study)

## save the relevant information
leukemia <- dat[, c("tx", "obs_time", "event")]
leukemia$death_in_6_months <- ifelse(leukemia$obs_time < 180, 1, 0)
write.table(leukemia, "datasets/leukemia.txt")
