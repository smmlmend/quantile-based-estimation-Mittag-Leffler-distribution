###### quantiles

q1 <- seq(0.05, 0.15, 0.025)
q2 <- seq(0.2, 0.3, 0.025)
q3 <- seq(0.45, 0.55, 0.025)
q4 <- seq(0.7, 0.8, 0.025)
q5 <- seq(0.85, 0.95, 0.025)


quantiles <- expand.grid(q1, q2, q3, q4, q5)

k <- as.integer(Sys.getenv("PBS_ARRAYID"))


# packages etc. -----------------------------------------------------------

library("MittagLeffleR")

# settings ---------------------------------------------------------------

### looking at a total of 288 settings

### tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)

combs <- rbind(combs, expand.grid(tail, 25, n))

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

# simulation study --------------------------------------------------------

data <- list()
res <- data.frame(matrix(nrow = 324*1000, ncol = 5))
res[,1] <- rep(combs[,1], each = 1000)
res[,2] <- rep(combs[,2], each = 1000)
res[,3] <- rep(combs[,3], each = 1000)

for (i in 289:324) {
  print(i)
  for (j in 1:1000) {
    set.seed(j)
    data[[j]] <- rml(combs[i,3] ,combs[i,1], combs[i,2])
    res[(i-1)*1000 + j,4:5] <- mlqbe(data[[j]], quantiles[k,])$par
    
  }
}

# Save the object to the file
filename <- paste("res_qb5_289_324_", k ,".RData", sep = "")
save(res, file = filename)