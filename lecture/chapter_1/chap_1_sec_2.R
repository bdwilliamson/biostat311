## R script for chapter 1, section 2: multiple linear regression
## Created 30 March 2018 by Brian Williamson

## --------------------------------------------------------------------
## Examples of confounders, precision variables, and effect modifiers
## --------------------------------------------------------------------

## data generating mechanism for two predictors, one is a confounder
data_func_confound <- function(beta0, beta1, beta2, n) {
  ## generate some X data: X1 is continuous, X2 is binary
  x2 <- sample(0:1, n, replace = TRUE)
  ## generate two clouds of x1 points
  x1_0 <- runif(sum(x2 == 0), 0, 0.55)
  x1_1 <- runif(sum(x2 == 1), 0.45, 1)
  X <- matrix(NA, ncol = 2, nrow = n)
  X[x2 == 0, 1] <- x1_0
  X[x2 == 1, 1] <- x1_1
  X[, 2] <- x2
  
  ## create the outcome
  y <- beta0 + beta1*X[, 1] + beta2*X[, 2] + rnorm(n, 0, 1)
  
  return(data.frame(x1 = X[, 1], x2 = X[, 2], y = y))
}
## data generating mechanism for two predictors, one is a precision variable
data_func_precision <- function(beta0, beta1, beta2, n) {
  ## generate some X data: X1 is continuous, X2 is binary
  x2 <- sample(0:1, n, replace = TRUE)
  x1 <- runif(n, 0, 1)
  
  ## generate the outcome
  y <- beta0 + beta1*x1 + beta2*x2 + rnorm(n, 0, 1)
  return(data.frame(x1 = x1, x2 = x2, y = y))
}
## data generating mechanism for two predictors, one is an effect modifier
data_func_em <- function(beta0, beta1, beta2, beta3, n) {
  ## generate some X data: X1 is continuous, X2 is binary
  x2 <- sample(0:1, n, replace = TRUE)
  ## generate two clouds of x1 points
  x1_0 <- runif(sum(x2 == 0), 0, 1)
  x1_1 <- runif(sum(x2 == 1), 0, 1)
  X <- matrix(NA, ncol = 2, nrow = n)
  X[x2 == 0, 1] <- x1_0
  X[x2 == 1, 1] <- x1_1
  X[, 2] <- x2
  
  ## create the outcome
  y <- beta0 + beta1*X[, 1] + beta2*X[, 2] + beta3*X[, 1]*X[, 2] + rnorm(n, 0, 1)
  
  return(data.frame(x1 = X[, 1], x2 = X[, 2], y = y))
}


## --------------------------
## Set up plot arguments
## --------------------------
fig_width <- 1024
fig_height <- 1024
fig_res <- 200

beta0 <- 6
beta1 <- -2
beta2 <- 2
## --------------------------
## Confounding
## --------------------------
## generate confounded data
set.seed(4747)
dat_c <- data_func_confound(beta0, beta1, beta2, n = 100)
## make a plot, without coloring
png("lecture/chapter_1/plots/confounding_simple.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_c$x1, dat_c$y, main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     xlim = c(0, 1), ylim = c(0, max(dat_c$y) + 0.5))
dev.off()
## fit a linear regression, plot best fitting line
mod_c_simple <- lm(y ~ x1, data = dat_c)
x_range <- cbind(1, seq(0, 1, by = 0.01))
png("lecture/chapter_1/plots/confounding_simple_with_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_c$x1, dat_c$y, main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     xlim = c(0, 1), ylim = c(0, max(dat_c$y) + 0.5))
lines(x_range[, 2], x_range%*%mod_c_simple$coefficients)
text(0.25, 1, labels = paste0("estimated slope = ", round(mod_c_simple$coefficients[2], 3)))
dev.off()

## make a plot, with coloring
png("lecture/chapter_1/plots/confounding_colored.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_c$x1[dat_c$x2 == 0], dat_c$y[dat_c$x2 == 0], main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     col = "red", xlim = c(0, 1), ylim = c(0, max(dat_c$y) + 0.5))
points(dat_c$x1[dat_c$x2 == 1], dat_c$y[dat_c$x2 == 1], pch = 18, col = "blue")
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()
png("lecture/chapter_1/plots/confounding_colored_with_simple_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_c$x1[dat_c$x2 == 0], dat_c$y[dat_c$x2 == 0], main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     col = "red", xlim = c(0, 1), ylim = c(0, max(dat_c$y) + 0.5))
points(dat_c$x1[dat_c$x2 == 1], dat_c$y[dat_c$x2 == 1], pch = 18, col = "blue")
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
lines(x_range[, 2], x_range%*%mod_c_simple$coefficients)
dev.off()
## fit adjusted regression, plot best fitting line 
mod_c_multiple <- lm(y ~ x1 + x2, data = dat_c)
x_range_0 <- cbind(x_range[1:which(x_range[, 2] == 0.55), ], 0)
x_range_1 <- cbind(x_range[which(x_range[, 2] == 0.45):length(x_range[, 2]), ], 1)
png("lecture/chapter_1/plots/confounding_colored_with_lines.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_c$x1[dat_c$x2 == 0], dat_c$y[dat_c$x2 == 0], main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     col = "red", xlim = c(0, 1), ylim = c(0, max(dat_c$y) + 0.5))
points(dat_c$x1[dat_c$x2 == 1], dat_c$y[dat_c$x2 == 1], pch = 18, col = "blue")
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
lines(x_range[, 2], x_range%*%mod_c_simple$coefficients)
lines(x_range_0[, 2], x_range_0%*%mod_c_multiple$coefficients, col = "red")
lines(x_range_1[, 2], x_range_1%*%mod_c_multiple$coefficients, col = "blue")
dev.off()


## -------------------
## Effect modification
## -------------------
beta0_em <- 6
beta1_em <- 2
beta2_em <- 3
beta3_em <- 1

set.seed(4747)
dat_e <- data_func_em(beta0_em, beta1_em, beta2_em, beta3_em, n = 1000)

## take a subsample to plot
dat_e_samp <- dat_e[sample.int(1000, 100), ]

## make a plot, without coloring
png("lecture/chapter_1/plots/effect_modification_simple.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_e_samp$x1, dat_e_samp$y, main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     xlim = c(0, 1), ylim = c(0, max(dat_e_samp$y) + 0.5))
dev.off()

## fit best fitting line
mod_e <- lm(y ~ x1, data = dat_e)

## plot with best fitting line
png("lecture/chapter_1/plots/effect_modification_simple_with_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_e_samp$x1, dat_e_samp$y, main = "", xlab = expression(X[1]), ylab = "Y", pch = 16,
     xlim = c(0, 1), ylim = c(0, max(dat_e_samp$y) + 0.5))
lines(x_range[, 2], x_range%*%mod_e$coefficients)
text(0.25, 2, labels = paste0("estimated slope = ", round(mod_e$coefficients[2], 3)))
dev.off()

## plot without lines, but colored by x2 status
png("lecture/chapter_1/plots/effect_modification_colored.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_e_samp$x1, dat_e_samp$y, main = "", xlab = expression(X[1]), ylab = "Y", 
     pch = ifelse(dat_e_samp$x2 == 1, 17, 16),
     xlim = c(0, 1), ylim = c(0, max(dat_e_samp$y) + 0.5),
     col = ifelse(dat_e_samp$x2 == 1, "blue", "red"))
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
                             pch = c(16, 17), col = c("red", "blue"))
dev.off()

## fit multiple regression with em
mod_e_multiple <- lm(y ~ x1*x2, data = dat_e)

## with simple line
png("lecture/chapter_1/plots/effect_modification_colored_with_simple_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_e_samp$x1, dat_e_samp$y, main = "", xlab = expression(X[1]), ylab = "Y", 
     pch = ifelse(dat_e_samp$x2 == 1, 17, 16),
     xlim = c(0, 1), ylim = c(0, max(dat_e_samp$y) + 0.5),
     col = ifelse(dat_e_samp$x2 == 1, "blue", "red"))
lines(x_range[, 2], x_range%*%mod_e$coefficients)
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()


x_range_0_em <- cbind(x_range[, ], 0, 0)
x_range_1_em <- cbind(x_range[, ], 1, x_range[, 2])

## with multiple lines
png("lecture/chapter_1/plots/effect_modification_colored_with_lines.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_e_samp$x1, dat_e_samp$y, main = "", xlab = expression(X[1]), ylab = "Y", 
     pch = ifelse(dat_e_samp$x2 == 1, 17, 16),
     xlim = c(0, 1), ylim = c(0, max(dat_e_samp$y) + 0.5),
     col = ifelse(dat_e_samp$x2 == 1, "blue", "red"))
lines(x_range[, 2], x_range%*%mod_e$coefficients)
lines(x_range_0_em[, 2], x_range_0_em%*%mod_e_multiple$coefficients, col = "red")
lines(x_range_1_em[, 2], x_range_1_em%*%mod_e_multiple$coefficients, col = "blue")
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()

## ---------------
## Precision
## ---------------

beta0_p <- 6
beta1_p <- 2
beta2_p <- 1

set.seed(4747)
dat_p <- data_func_precision(beta0_p, beta1_p, beta2_p, n = 1000)
dat_p_samp <- dat_p[sample.int(1000, 100), ]

## plot the points
png("lecture/chapter_1/plots/precision_simple.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_p_samp$x1, dat_p_samp$y, main = "", xlab = expression(X[1]), ylab = "Y",
     pch = 16, xlim = c(0, 1), ylim = c(0, max(dat_p_samp$y) + 0.5))
dev.off()

## fit a line
mod_p <- lm(y ~ x1, data = dat_p)

## plot with the line
png("lecture/chapter_1/plots/precision_simple_with_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_p_samp$x1, dat_p_samp$y, main = "", xlab = expression(X[1]), ylab = "Y",
     pch = 16, xlim = c(0, 1), ylim = c(0, max(dat_p_samp$y) + 0.5))
lines(x_range[, 2], x_range%*%mod_p$coefficients)
text(0.2, 2, labels = paste0("estimated slope = ", round(mod_p$coefficients[2], 3)))
dev.off()

## color the points
png("lecture/chapter_1/plots/precision_colored.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_p_samp$x1, dat_p_samp$y, main = "", xlab = expression(X[1]), ylab = "Y",
     pch = ifelse(dat_p_samp$x2 == 1, 17, 16), xlim = c(0, 1), 
     ylim = c(0, max(dat_p_samp$y) + 0.5),
     col = ifelse(dat_p_samp$x2 == 1, "blue", "red"))
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()

## colored points, with line
png("lecture/chapter_1/plots/precision_colored_with_line.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_p_samp$x1, dat_p_samp$y, main = "", xlab = expression(X[1]), ylab = "Y",
     pch = ifelse(dat_p_samp$x2 == 1, 17, 16), xlim = c(0, 1), 
     ylim = c(0, max(dat_p_samp$y) + 0.5),
     col = ifelse(dat_p_samp$x2 == 1, "blue", "red"))
lines(x_range[, 2], x_range%*%mod_p$coefficients)
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()

## multiple regression
mod_p_multiple <- lm(y ~ x1 + x2, data = dat_p)
x_range_0_p <- cbind(x_range, 0)
x_range_1_p <- cbind(x_range, 1)

## colored points, with line
png("lecture/chapter_1/plots/precision_colored_with_multiple_lines.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0, 2) + 0.1)
plot(dat_p_samp$x1, dat_p_samp$y, main = "", xlab = expression(X[1]), ylab = "Y",
     pch = ifelse(dat_p_samp$x2 == 1, 17, 16), xlim = c(0, 1), 
     ylim = c(0, max(dat_p_samp$y) + 0.5),
     col = ifelse(dat_p_samp$x2 == 1, "blue", "red"))
lines(x_range[, 2], x_range%*%mod_p$coefficients)
lines(x_range_0_p[, 2], x_range_0_p%*%mod_p_multiple$coefficients, col = "red")
lines(x_range_1_p[, 2], x_range_1_p%*%mod_p_multiple$coefficients, col = "blue")
legend("bottomright", legend = c(expression(paste(X[2],  " = 0", sep = "")), expression(paste(X[2], " = 1", sep = ""))),
       pch = c(16, 17), col = c("red", "blue"))
dev.off()

