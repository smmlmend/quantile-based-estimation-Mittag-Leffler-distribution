

# packages etc. -----------------------------------------------------------

library("MittagLeffleR")

mlqls <- function(data, q){
  quants <- quantile(data, probs = q)
  a <- function(params) {
    sum((quants - qml(q, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B",
             lower = c(10^(-2),10^(-6)), 
             upper = c(1, 10^5))
  return(b)
}

# settings ---------------------------------------------------------------

### looking at a total of 288 settings

### tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)


k <- as.integer(Sys.getenv("PBS_ARRAYID"))

q_comb <- list(c(0.05, 0.2, 0.475, 0.8, 0.925),
               seq(0.1, 0.9, length = 5),
               seq(0.05, 0.95, length = 5))


# simulation study --------------------------------------------------------

data <- list()
res_qls1 <- data.frame(matrix(nrow =  1000, ncol = 5))
res_qls1[,1] <- rep(combs[k,1], each = 1000)
res_qls1[,2] <- rep(combs[k,2], each = 1000)
res_qls1[,3] <- rep(combs[k,3], each = 1000)



  for (j in 1:1000) {
    print(j)
    set.seed(j)
    data[[j]] <- rml(combs[k,3] ,combs[k,1], combs[k,2])
    res_qls1[j,4:5] <- mlqls(data[[j]], q_comb[[1]])$par
  }


# Save the object to the file
filename <- paste("res_qls_opt_setting_", k ,".RData", sep = "")
save(res_qls1, file = filename)