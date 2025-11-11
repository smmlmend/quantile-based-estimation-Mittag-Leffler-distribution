############## Analysis of 5 quantile combinations


setwd("~/paper2/simulation_study/quantile_comb/results")


# Uploading the data sets ------------------------------------------------------


########## do in three parts, 1:5000, 5001:10000 and 10001:16807
for(k in 1:3125){
  print(k)
  filename <- paste("res_qb5_", k, ".RData", sep = "")
  load(filename)
  assign(paste("res_", k, sep = ""), res)
}



# considered scenarios ----------------------------------------------------

### tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)


# Calculating the MSEs ----------------------------------------------------


mse_beta <- data.frame(combination = rep(1:3125, each = 288), tail = rep(combs[,1], 3125), 
                       mse = numeric(288*3125))

mse_sigma <- data.frame(combination = rep(1:3125, each = 288), tail = rep(combs[,2], 3125), 
                        mse = numeric(288*3125))

for(k in 1:3125){
  print(k)
  for (j in 1:288) {
    mse_beta[(k-1)*288+j, 3] <- mean((get(paste("res_", k, sep = ""))[(1+(j-1)*1000):(j*1000),4]-combs[j,1])^2)
    mse_sigma[(k-1)*288+j, 3] <- mean((get(paste("res_", k, sep = ""))[(1+(j-1)*1000):(j*1000),5]-combs[j,2])^2)
  }
  
}

save(mse_beta, file = "mse_beta_total.RData")
save(mse_sigma, file = "mse_sigma_total.RData")


