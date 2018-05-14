## plots etc. for chapter 3
fig_width <- 1024
fig_height <- 1024
fig_res <- 200

## create a plot of the survival probability for the example on 3.42
mat_3.42 <- data.frame(time = c(4, 5, 8, 12, 13, 18, 23, 30),
                       d_k = c(1, 1, 1, 0, 1, 0, 2, 1),
                       n_k = c(10, 9, 7, 6, 5, 4, 3, 1))
mat_3.42$d_k_over_n_k <- mat_3.42$d_k/mat_3.42$n_k
mat_3.42$one_minus <- 1 - mat_3.42$d_k_over_n_k
mat_3.42$s_hat <- cumprod(mat_3.42$one_minus)
png("lecture/chapter_3/figs/km_small_example.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
## plot the points where someone drops out
plot(mat_3.42$time[-c(4, 6)], mat_3.42$s_hat[-c(4, 6)], pch = 16, ylim = c(0, 1), xlim = c(0, 40),
     ylab = "survival probability", xlab = "time")
## plot the discontinuities
points(mat_3.42$time[-c(4, 6)], c(1, mat_3.42$s_hat[1:7][-c(4, 6)]))
## plot the lines to these points
segments(x0 = c(0, mat_3.42$time[1:7][-c(4, 6)]), y0 = c(1, mat_3.42$s_hat[1:7][-c(4, 6)]),
         x1 = mat_3.42$time[-c(4, 6)], y1 = c(1, mat_3.42$s_hat[1:7][-c(4, 6)]))
segments(x0 = mat_3.42$time, y0 = c(1, mat_3.42$s_hat[1:7]),
         x1 = mat_3.42$time, y1 = mat_3.42$s_hat, lty = 2)
segments(x0 = 30, y0 = 0, x1 = 50, y1 = 0)
dev.off()

## survival function, for median
png("lecture/chapter_3/figs/survival_function_median.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
curve(1-pweibull(x,2,20),0,60,ylab="S(t)",xlab="t",ylim=c(0,1),lwd=2.5,cex.axis=0.8,cex.lab=0.8)
points(c(17,17),c(-1,0.5),type='l',lwd=2,col=2,lty=3)
points(c(-5,17),c(0.5,0.5),type='l',lwd=2,col=2,lty=3)
mtext(expression(t[0.5]), col = "red", at = 17, side = 1, line = 1, cex = 0.8)
mtext("0.5", col = "red", at = 0.5, side = 2, line = 1, cex = 0.8)
dev.off()

png("lecture/chapter_3/figs/km_small_example_median.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
## plot the points where someone drops out
plot(mat_3.42$time[-c(4, 6)], mat_3.42$s_hat[-c(4, 6)], pch = 16, ylim = c(0, 1), xlim = c(0, 40),
     ylab = "survival probability", xlab = "time")
## plot the discontinuities
points(mat_3.42$time[-c(4, 6)], c(1, mat_3.42$s_hat[1:7][-c(4, 6)]))
## plot the lines to these points
segments(x0 = c(0, mat_3.42$time[1:7][-c(4, 6)]), y0 = c(1, mat_3.42$s_hat[1:7][-c(4, 6)]),
         x1 = mat_3.42$time[-c(4, 6)], y1 = c(1, mat_3.42$s_hat[1:7][-c(4, 6)]))
segments(x0 = mat_3.42$time, y0 = c(1, mat_3.42$s_hat[1:7]),
         x1 = mat_3.42$time, y1 = mat_3.42$s_hat, lty = 2)
segments(x0 = 30, y0 = 0, x1 = 50, y1 = 0)
abline(h = 0.5, lty = 3, col = "red")
points(c(23, 23), c(-1, 0.5), col = "red", type = "l", lty = 3)
mtext(expression(hat(t)[0.5]), col = "red", at = 23, side = 1, line = 1)
mtext("0.5", col = "red", at = 0.5, side = 2, line = 1)
dev.off()

## plot the hazard function for the inflamm data, based on CRP
inflamm <- read.table("datasets/inflamm.txt", header = TRUE)
## load the survival package
library("survival")
## create a survival object out of observed time and event indicator
inflamm$surv_obj <- Surv(time = inflamm$ttodth, event = inflamm$death)
## create survival curves based on two groups of crp (normal is <= 10, abnormal > 10)
surv_fit <- survfit(surv_obj ~ I(crp <= 10), data = inflamm)
png("lecture/chapter_3/figs/cum_haz_inflamm_crp.png", width = fig_width, height = fig_height, units = "px", res = fig_res)
par(mar = c(5, 4, 0.1, 0.1))
plot(surv_fit, fun = "cumhaz", ylab = "cumulative hazard", 
     xlab = "time from recruitment until death (days)",
     col = c("blue", "red"))
legend("topleft", legend = c("CRP <= 10", "CRP > 10"), col = c("blue", "red"), lty = 1)
dev.off()