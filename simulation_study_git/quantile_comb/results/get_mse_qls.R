############## Analysis of 5 quantile combinations


setwd("~/paper2/simulation_study/quantile_comb/results")


# Uploading the data sets ------------------------------------------------------


##########
for(k in 1:324){
  print(k)
  filename <- paste("res_qls1_setting_", k, ".RData", sep = "")
  load(filename)
  assign(paste("res_qls2_", k, sep = ""), res_qls2)
}


for(k in 1:324){
  print(k)
  filename <- paste("res_qls3_setting_", k, ".RData", sep = "")
  load(filename)
  assign(paste("res_qls3_", k, sep = ""), res_qls3)
}

for(k in 1:324){
  print(k)
  filename <- paste("res_qls3_setting_", k, ".RData", sep = "")
  load(filename)
  assign(paste("res_qls4_", k, sep = ""), res_qls3)
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

mse_qls2_beta <- data.frame(combination = rep("qls2", 324), tail = combs[,1], 
                          mse = numeric(324))

mse_qls2_sigma <- data.frame(combination = rep("qls2",  324), scale = combs[,2], 
                           mse = numeric(324))


mse_qls3_beta <- data.frame(combination = rep("qls3", 324), tail = combs[,1], 
                            mse = numeric(324))

mse_qls3_sigma <- data.frame(combination = rep("qls3",  324), scale = combs[,2], 
                             mse = numeric(324))

mse_qls4_beta <- data.frame(combination = rep("qls4", 324), tail = combs[,1], 
                            mse = numeric(324))

mse_qls4_sigma <- data.frame(combination = rep("qls4",  324), scale = combs[,2], 
                             mse = numeric(324))

for (j in 1:324) {
  mse_qls2_beta[j, 3] <- mean((get(paste("res_qls2_", j, sep = ""))[,4]-combs[j,1])^2)
  mse_qls2_sigma[j, 3] <- mean((get(paste("res_qls2_", j, sep = ""))[,5]-combs[j,2])^2)

  
  mse_qls3_beta[j, 3] <- mean((get(paste("res_qls3_", j, sep = ""))[,4]-combs[j,1])^2)
  mse_qls3_sigma[j, 3] <- mean((get(paste("res_qls3_", j, sep = ""))[,5]-combs[j,2])^2)
  
  mse_qls4_beta[j, 3] <- mean((get(paste("res_qls4_", j, sep = ""))[,4]-combs[j,1])^2)
  mse_qls4_sigma[j, 3] <- mean((get(paste("res_qls4_", j, sep = ""))[,5]-combs[j,2])^2)
}




save(mse_qls2_beta, file = "mse_qls2_beta_324.RData")
save(mse_qls2_sigma, file = "mse_qls2_sigma_324.RData")

save(mse_qls3_beta, file = "mse_qls3_beta_324.RData")
save(mse_qls3_sigma, file = "mse_qls3_sigma_324.RData")

save(mse_qls4_beta, file = "mse_qls4_beta_324.RData")
save(mse_qls4_sigma, file = "mse_qls4_sigma_324.RData")
