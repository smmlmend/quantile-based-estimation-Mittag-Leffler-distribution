####################### Results comparison #####################


# Packages and working directory ------------------------------------------

setwd("~/paper2/simulation_study/computation_time/results")

library(MittagLeffleR)
library(tidyverse)
library(gridExtra)
library("microbenchmark")
library(scales) 

beta <- seq(0.6, 1, 0.05)
sigma <- c(50, 100, 250, 500, 750, 1000, 1500, 2000)
n <- c(200, 500, 1000, 5000)

settings <- expand.grid(beta, sigma, n)



# Load data ---------------------------------------------------------------


average_time <- matrix(nrow = 288*5, ncol = 3) 
average_time[,2] <- c("LM", "ML", "CM", "QB", "QLS")
average_time[,3] <- rep(settings[,3], each = 5)

for (j in 1:288) {
  filename <- paste("time_set_",j , ".RData", sep = "")
  load(filename)
  average_time[((j-1)*5+1):(j*5),1] <-summary(check_time)[,4]
}



# analysis ----------------------------------------------------------------

average_time <- as.data.frame(average_time)
names(average_time) <- c("time", "Estimator", "n")
average_time$time <- as.numeric(average_time$time)
average_time$time_ms <- as.numeric(average_time$time)*10^(-3)
average_time$time_s <- average_time$time*10^(-6)
average_time$time_log <- log(average_time$time)
average_time$n <- factor(average_time$n, levels = c(200, 500, 1000, 5000))





# ggplot  -----------------------------------------------------------------

### in log scale 

scientific_10 <- function(x) {
  parse(text=gsub("e\\+", "%*%10^", scales::scientific_format(digits = 2)(x)))
}

scientific_10 <- function(x) {
  labels <- scales::scientific_format(digits = 2)(x)
  
  # Custom overrides
  labels[labels == "1.0e+00"] <- "1"
  labels[labels == "5.5e+01"] <- "5.5 %*%10"
  labels[labels == "3.0e+03"] <- "3.0 %*%10^3"
  
  # Convert remaining labels to expression
  parse(text = gsub("e\\+", "%*%10^", labels))
}




ggplot(average_time, aes(x = Estimator, y = time_ms, fill = n)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13), 
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank()
  ) +
  ylab("Average computation time (ms)") +
  scale_y_continuous(trans = "log", 
                     labels = scientific_10)

max(average_time$time_ms[average_time$Estimator == "QLS"] / 
  average_time$time_ms[average_time$Estimator == "QB"])

### original scale
library(ggh4x)

ggplot(average_time, aes(x=Estimator, y=time_s, fill = n )) + 
  geom_boxplot()+
  theme_minimal()+
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 14))+
  ylab("Average computation time (s)")






