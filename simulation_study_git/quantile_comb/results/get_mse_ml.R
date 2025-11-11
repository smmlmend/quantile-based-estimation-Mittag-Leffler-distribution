############## Analysis of 5 quantile combinations


setwd("~/paper2/simulation_study/quantile_comb/results")


# Uploading the data sets ------------------------------------------------------


########## do in three parts, 1:5000, 5001:10000 and 10001:16807
for(k in 1:324){
  print(k)
  filename <- paste("res_mlmle_setting_", k, ".RData", sep = "")
  load(filename)
  res$X4[which(res$X4 > 1)] <- 1
  assign(paste("res_mlmle_", k, sep = ""), res)
}

# considered scenarios ----------------------------------------------------

### tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)
combs <- rbind(combs, expand.grid(tail, 25, n))


# Calculating the MSEs ----------------------------------------------------

mse_ml_beta <- data.frame(combination = rep("ml", 324), tail = combs[,1], 
                       mse = numeric(324))

mse_ml_sigma <- data.frame(combination = rep("ml",  324), scale = combs[,2], 
                        mse = numeric(324))

for (j in 1:324) {
    mse_ml_beta[j, 3] <- mean((get(paste("res_mlmle_", j, sep = ""))[,4]-combs[j,1])^2)
    mse_ml_sigma[j, 3] <- mean((get(paste("res_mlmle_", j, sep = ""))[,5]-combs[j,2])^2)
    }
  

save(mse_ml_beta, file = "mse_ml_beta_324.RData")
save(mse_ml_sigma, file = "mse_ml_sigma_324.RData")
