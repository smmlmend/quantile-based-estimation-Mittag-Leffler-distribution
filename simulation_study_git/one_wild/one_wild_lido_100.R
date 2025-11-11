##### one wild study: 


# Packages ----------------------------------------------------------------

library(MittagLeffleR)


# estimators  -------------------------------------------------------------

### CMD 

mlcmd <- function(data){
  n <- length(data)
  a <- function(params) {
    sum(( ((1:n)-0.5)/n - pml(sort(data), params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B", 
             lower = c(10^-7,10^-7), 
             upper = c(1, 10^6))
  return(b$par)
}

### QB 

mlqbe <- function(data, q = c(0.1, 0.3, 0.5, 0.8, 0.925)){
  quants <- quantile(data, probs = q)
  a <- function(params) {
    sum((q - pml(quants, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B",
             lower = c(10^(-6),10^(-6)), 
             upper = c(1, 10^7))
  return(b)
}


### QLS

mlqls <- function(data, q = seq(0.1, 0.9, length = 5)){
  quants <- quantile(data, probs = q)
  a <- function(params) {
    sum((quants - qml(q, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B",
             lower = c(10^(-2),10^(-6)), 
             upper = c(1, 10^5))
  return(b)
}


set.seed(8825)

data <- rml(100, 0.9, 500)

new <- seq(qml(0.001, 0.9, 500), 
           qml(0.9995, 0.9, 500), 
           length = 200)


estimates <- matrix(nrow = 201, ncol = 10)
colnames(estimates) <- c("beta_lm", "sigma_lm",
                         "beta_ml", "sigma_ml",
                         "beta_qb", "sigma_qb",
                         "beta_qls", "sigma_qls",
                         "beta_cm", "sigma_cm")

estimates[1, ] <- c(logMomentEstimator(data)[1:2], 
                    mlmle(data)$par, 
                    mlqbe(data)$par, 
                    mlqls(data)$par, 
                    mlcmd(data))

for (i in 1:200) {
  print(i)
  data_new <- c(data[-which.min(data)], new[i])
  estimates[i+1, ] <- c(logMomentEstimator(data_new)[1:2], 
                        mlmle(data_new)$par, 
                        mlqbe(data_new)$par, 
                        mlqls(data_new)$par, 
                        mlcmd(data_new))
  
}


save(estimates, file = "one_wild_0.9_500_0.9995_n100.RData")




