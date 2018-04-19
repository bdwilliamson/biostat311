## code for exam 1 prep activity

## read the full salary data
salary_0 <- read.table("datasets/salary.txt", header = TRUE, stringsAsFactors = FALSE)

## keep only 1995
salary_1 <- subset(salary_0, salary_0$year == 95)

## get rid of some columns
salary_1 <- salary_0[, c("id", "sex", "deg", "field", "rank", "admin", "salary")]

## re-code sex as male
salary_1$male <- ifelse(salary_1$sex == "M", 1, 0)

## save off the data
saveRDS(salary_1, "datasets/salary_for_exam_1_prep.rds")
