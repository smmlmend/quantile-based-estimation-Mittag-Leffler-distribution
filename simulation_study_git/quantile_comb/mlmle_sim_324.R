

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

k <- as.integer(Sys.getenv("PBS_ARRAYID"))


# simulation study --------------------------------------------------------

data <- list()
res <- data.frame(matrix(nrow = 1000, ncol = 5))
res[,1] <- rep(combs[k,1], 1000)
res[,2] <- rep(combs[k,2], 1000)
res[,3] <- rep(combs[k,3],  1000)


for (j in 1:1000) {
    set.seed(j)
    print(j)
    data[[j]] <- rml(combs[k,3] ,combs[k,1], combs[k,2])
    res[j,4:5] <- mlmle(data[[j]])$par
}

# Save the object to the file
filename <- paste("res_mlmle_setting_", k ,".RData", sep = "")
save(res, file = filename)