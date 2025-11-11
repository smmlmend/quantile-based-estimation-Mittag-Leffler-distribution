

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



# simulation study --------------------------------------------------------

data <- list()
res_lm <- data.frame(matrix(nrow = 324 * 1000, ncol = 5))
res_lm[,1] <- rep(combs[,1], each = 1000)
res_lm[,2] <- rep(combs[,2], each = 1000)
res_lm[,3] <- rep(combs[,3], each = 1000)


for(k in 1:324){
  print(k)
for (j in 1:1000) {
  set.seed(j)
  data[[j]] <- rml(combs[k,3] ,combs[k,1], combs[k,2])
  res_lm[(k-1)*1000+j,4:5] <- logMomentEstimator(data[[j]])[1:2]
}
}

# Save the object to the file
save(res_lm, file = "res_lm_324.RData")