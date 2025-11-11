setwd("~/paper2/simulation_study/quantile_comb/results")
load("res_qb5_289_396_2448.RData")

### tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)

combs <- rbind(combs, expand.grid(tail, 25, n),
               expand.grid(c(0.5, 0.55), c(25, scale_ml), n))


mse_beta <- data.frame(tail = combs[,1], 
                       mse = numeric(396))

mse_sigma <- data.frame(scale = combs[,2], 
                        mse = numeric(396))


for (j in 289:396) {
  mse_beta[j, 2] <- mean((res[(1+(j-1)*1000):(j*1000),4]-combs[j,1])^2)
  mse_sigma[j, 2] <- mean((res[(1+(j-1)*1000):(j*1000),5]-combs[j,2])^2)
}
