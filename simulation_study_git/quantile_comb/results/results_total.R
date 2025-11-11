############## Analysis of 5 quantile combinations, MSE 

library(xtable)
library(tidyverse)
library(ggpubr)
library(dplyr)

setwd("~/paper2/simulation_study/quantile_comb/results")


### quantile matching
load("mse_beta_324.RData")

load("mse_sigma_324.RData")

### maximum likelihood
load("mse_ml_beta_324.RData")

load("mse_ml_sigma_324.RData")

### cramer v mises
load("mse_cmd_beta_324.RData")

load("mse_cmd_sigma_324.RData")


### qls2
load("mse_qls2_beta_324.RData")

load("mse_qls2_sigma_324.RData")

### qls3
load("mse_qls3_beta_324.RData")

load("mse_qls3_sigma_324.RData")

### qls4
load("mse_qls4_beta_324.RData")

load("mse_qls4_sigma_324.RData")


### lm
load("mse_lm_beta_324.RData")

load("mse_lm_sigma_324.RData")

###### quantiles

q1 <- seq(0.05, 0.15, 0.025)
q2 <- seq(0.2, 0.3, 0.025)
q3 <- seq(0.45, 0.55, 0.025)
q4 <- seq(0.7, 0.8, 0.025)
q5 <- seq(0.85, 0.95, 0.025)

quantiles <- expand.grid(q1, q2, q3, q4, q5)

## tail parameters
tail <- seq(0.6, 1, 0.05)

### scale parameters
scale_ml <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)

n <- c(200, 500, 1000, 5000)


combs <- expand.grid(tail, scale_ml, n)

combs <- rbind(combs, expand.grid(tail, 25, n))

mse_beta_split <- split(mse_beta, mse_beta$combination)

mse_sigma_split <- split(mse_sigma, mse_sigma$combination)

f <- function(X){
  mse_ml_beta$mse/X[,3]
}

g <- function(X){
  mse_ml_sigma$mse  /X[,3]
}

mean_efficiency_beta <- mse_beta_split %>% 
  lapply(., f) %>% 
  lapply(. ,mean) %>% 
  unlist()

mean_efficiency_sigma <-  mse_sigma_split %>% 
  lapply( .,g) %>% 
  lapply(. ,mean) %>% 
  unlist()


#### product
efficiency_beta <- mse_beta_split %>% 
  lapply(., f)%>% 
  unlist()

efficiency_sigma <-  mse_sigma_split %>% 
  lapply( .,g)%>% 
  unlist()


mean(efficiency_beta * efficiency_sigma)

best <- order(mean_efficiency_sigma + mean_efficiency_beta, 
      decreasing = TRUE)[1:25]

mean_efficiency_beta[best]
mean_efficiency_sigma[best]

df <- quantiles[best,]
df[,6] <- (mean_efficiency_sigma + mean_efficiency_beta)[best]

xtable(df, digits = 3)

mse_qb_beta <- mse_beta_split[[2448]]

mse_qb_sigma <- mse_sigma_split[[2448]]

boxplot(mse_qb_beta[,3], mse_lm_beta[,3], mse_cmd_beta[,3],
        mse_qls2_beta[,3], mse_qls4_beta[,3], mse_ml_beta[,3])

boxplot(mse_qb_sigma[,3], mse_lm_sigma[,3], mse_cmd_sigma[,3],
        mse_qls2_sigma[,3], mse_qls4_sigma[,3], mse_ml_sigma[,3])




# Final Plots for paper ---------------------------------------------------



# Plots in dependence of beta / sigma


### beta

efficiency_beta <- rbind(mse_lm_beta,  mse_cmd_beta,
                         mse_qb_beta, mse_qls2_beta)

efficiency_beta$efficiency <-  mse_ml_beta[,3]/efficiency_beta$mse
efficiency_beta$n <- combs[,3]
efficiency_beta$estimator <- rep(c("LM", "CM", "QB", "QLS"), each = 324)


new_data <- efficiency_beta %>%
  group_by(n, estimator, tail) %>% 
  summarise(median=median(efficiency), 
            .groups = "keep")

range_data <- efficiency_beta %>%
  group_by(n, estimator, tail) %>% 
  summarise(min=min(efficiency), 
            max = max(efficiency),
            .groups = "keep")

range_data$range <- range_data$max - range_data$min
max(range_data[range_data$n == 500, 6])


ggplot(new_data[new_data$n == 1000,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line()+
  labs(
    x = expression(beta),
    y = "Efficiency",
    color = "Estimator", 
    title = expression(paste("Median Efficiency of the Estimators for ", beta ,", n = 1000"))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        subtitle = element_text(size = 14),
        legend.text = element_text(size=12))



efficiency_sigma <- rbind(mse_lm_sigma,  mse_cmd_sigma,
                          mse_qb_sigma, mse_qls2_sigma)

efficiency_sigma$efficiency <-  mse_ml_sigma[,3]/efficiency_sigma$mse
efficiency_sigma$tail <- combs[,1]
efficiency_sigma$n <- combs[,3]
efficiency_sigma$estimator <- rep(c("LM", "CM", "QB", "QLS"), each = 324)

new_data_sigma <- efficiency_sigma %>%
  group_by(n, estimator, tail) %>% 
  summarise( median=median(efficiency), 
            .groups = "keep")

range_data_sigma <- efficiency_sigma %>%
  group_by(n, estimator, tail) %>% 
summarise(min=min(efficiency), 
          max = max(efficiency),
          .groups = "keep")

max(range_data_sigma[which(range_data_sigma$n == 500 & 
                         range_data_sigma$estimator == "QB"), 5]-
  range_data_sigma[which(range_data_sigma$n == 500 & 
                           range_data_sigma$estimator == "QB"), 4])
  

g1 <- ggplot(new_data[new_data$n == 500,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 500",
    subtitle = expression(paste("Estimators for ", beta))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) + ylim(0,1)


g2 <- ggplot(new_data_sigma[new_data_sigma$n == 500,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 500",
    subtitle = expression(paste("Estimators for ", sigma ))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+ylim(0,1)


ggarrange(g1, g2, common.legend = TRUE, legend = "bottom")



### n = 1000 ------------------------------------------------------------


g1_1000 <- ggplot(new_data[new_data$n == 1000,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 1000",
    subtitle = expression(paste("Estimators for ", beta))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+ylim(0,1.01)

g2_1000 <- ggplot(new_data_sigma[new_data_sigma$n == 1000,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 1000",
    subtitle = expression(paste("Estimators for ", sigma ))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+ylim(0,1.01)


ggarrange(g1_1000, g2_1000, common.legend = TRUE, legend = "bottom")



### n = 200 ------------------------------------------------------------


g1_200 <- ggplot(new_data[new_data$n == 200,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 200",
    subtitle = expression(paste("Estimators for ", beta))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+ylim(0,1.0)

g2_200 <- ggplot(new_data_sigma[new_data_sigma$n == 200,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 200",
    subtitle = expression(paste("Estimators for ", sigma ))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+ylim(0,1.0)


ggarrange(g1_200, g2_200, common.legend = TRUE, legend = "bottom")



### n = 5000 ----------------------------------------------------------------


g1_5000 <- ggplot(new_data[new_data$n == 5000,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 5000",
    subtitle = expression(paste("Estimators for ", beta))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+ylim(0,1)

g2_5000 <- ggplot(new_data_sigma[new_data_sigma$n == 5000,], aes(x = tail, y = median, color = estimator)) +
  geom_point( size = 2) + geom_line(linewidth = 0.65)+
  labs(
    x = expression(beta),
    y = "Median relative efficiency",
    color = "Estimator",
    title = "n = 5000",
    subtitle = expression(paste("Estimators for ", sigma ))
  ) +
  theme_minimal()+ 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+ylim(0,1.0)


ggarrange(g1_5000, g2_5000, common.legend = TRUE, legend = "bottom")
