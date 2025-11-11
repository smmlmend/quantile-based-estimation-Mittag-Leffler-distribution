#### computation time study



# package and settings ----------------------------------------------------


library(microbenchmark)
library(MittagLeffleR)

beta <- seq(0.6, 1, 0.05)
sigma <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)
n <- c(200, 500, 1000, 5000)

settings <- expand.grid(beta, sigma, n)

j <-  as.integer(Sys.getenv("PBS_ARRAYID"))


# functions ---------------------------------------------------------------


mlqbe <- function(data, q){
  quants <- quantile(data, q)
  a <- function(params) {
    sum((q - pml(quants, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B", 
             lower = c(10^-7,10^-7), 
             upper = c(1, 10^6))
  return(b$par)
}


mlqls <- function(data, q){
  quants <- quantile(data, q)
  a <- function(params) {
    sum((quants - qml(q, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B", 
             lower = c(10^-2,10^-7), 
             upper = c(1, 10^6))
  return(b$par)
}


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




# computation times -------------------------------------------------------


set.seed((j* 11)^2)

data <- rml(settings[j,3], settings[j,1], settings[j,2])

check_time <- microbenchmark(logMomentEstimator(data)[1:2],
               mlmle(data)$par,
               mlcmd(data),
               mlqbe(data, q = c(0.1, 0.3, 0.5, 0.8, 0.925)),
               mlqls(data, q = seq(0.1, 0.9, by = 0.2)),
               times = 1000)


filename <- paste("time_set_", j ,".RData", sep = "")
save(check_time, file = filename)

