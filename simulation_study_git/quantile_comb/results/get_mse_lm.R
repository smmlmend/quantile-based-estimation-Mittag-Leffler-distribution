
# load data ---------------------------------------------------------------


setwd("~/paper2/simulation_study/quantile_comb/results")

load("res_lm_324.RData")

res_lm[which(res_lm$X4 > 1),4] <- 1

# settings ----------------------------------------------------------------


## tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)

combs <- rbind(combs, expand.grid(tail, 25, n))

# determine mse -----------------------------------------------------------


mse_lm_beta <- data.frame(combination = rep("lm", 324),
                          tail = combs[,1], 
                          mse = numeric(324))

mse_lm_sigma <- data.frame(combination = rep("lm", 324),
                          scale = combs[,2], 
                          mse = numeric(324))
for (j in 1:324) {
  mse_lm_beta[j, 3] <- mean((res_lm[(1+(j-1)*1000):(j*1000),4]
                          -combs[j,1])^2)
  mse_lm_sigma[j, 3] <- mean((res_lm[(1+(j-1)*1000):(j*1000),5]
                           -combs[j,2])^2)
}

save(mse_lm_beta, file = "mse_lm_beta_324.RData")
save(mse_lm_sigma, file = "mse_lm_sigma_324.RData")


