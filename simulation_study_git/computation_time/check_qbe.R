library(MittagLeffleR)
library(microbenchmark)

# quantile based estimator ------------------------------------------------

mlqbe <- function(data, q){
  quants <- quantile(data, probs = q)
  a <- function(params) {
    sum((q - pml(quants, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B",
             lower = c(10^(-6),10^(-6)), 
             upper = c(1, 10^7))
  return(b)
}

data <- rml(100, 0.8, 100)

check_tim_qbe_1 <- microbenchmark( mlqbe(data, q = c(0.1, 0.3, 0.5, 0.8, 0.925)),
                                 times = 100)

data_500 <- rml(500, 0.8, 100)

check_tim_qbe_2 <- microbenchmark( mlqbe(data_500, q = c(0.1, 0.3, 0.5, 0.8, 0.925)),
                                   times = 100)

data_2000 <- rml(2000, 0.8, 100)

check_tim_qbe_3 <- microbenchmark( mlqbe(data_2000, q = c(0.1, 0.3, 0.5, 0.8, 0.925)),
                                   times = 100)

summary(check_tim_qbe_1)
summary(check_tim_qbe_2)
summary(check_tim_qbe_3)

count1 <- numeric(100)
count2 <- numeric(100)
count3 <- numeric(100)

for (i in 1:100) {
  set.seed(i)
  data <- rml(100, 0.8, 100)
  data_500 <- rml(500, 0.8, 100)
  data_2000 <- rml(2000, 0.8, 100)
  count1[i] <- mlqbe(data, q = c(0.1, 0.3, 0.5, 0.8, 0.925))$counts[1]
  count2[i] <- mlqbe(data_500, q = c(0.1, 0.3, 0.5, 0.8, 0.925))$counts[1]
  count3[i] <- mlqbe(data_2000, q = c(0.1, 0.3, 0.5, 0.8, 0.925))$counts[1]
}

mean(count1)
mean(count2)
mean(count3)
