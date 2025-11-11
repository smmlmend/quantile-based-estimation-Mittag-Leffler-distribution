library(tidyverse)
library(ggpubr)
library(MittagLeffleR)


setwd("~/paper2/simulation_study/one_wild")

seeds <- c(110825, 
           120825, 
           130825,
           1011, 
           091, 
           2025, 
           (1:10)^2)
k <- 16
filename <- paste("res_one_wild_seed_", seeds[k] ,".RData", sep = "")
load(filename)

# create ggplots ----------------------------------------------------------


# Assuming estimates is a matrix or data.frame

change <- as.data.frame(sweep(estimates, 2, estimates[1, ], "-"))

change_gg <- data.frame(change = change[2:501,1] * 201)

for (i in 2:10) {
  change_gg <- rbind(change_gg , 
                     data.frame(change = change[2:501,i] * 201))
}

change_gg$Estimator <- rep(c("LM", "ML", "QB", "QLS", "CM"), each = 1000)
change_gg$parameter <- c(rep("tail", 500), rep("scale", 500))
change_gg$x <- seq(qml(0.001, 0.9, 1), 
                   qml(0.999, 0.9, 1), 
                   length = 500)

# First plot: columns 1,3,5,7,9
p1 <- ggplot(change_gg[change_gg$parameter == "tail", ],
  aes(x = x, y = change, color = Estimator)
) +
  geom_line(size = 0.8) +
  theme_minimal() +
  ylab(expression(paste("SC(x, ", hat(beta), ")")))+
  theme(
    axis.title = element_text(size = 14),   
    axis.text= element_text(size = 12),   
    legend.title  = element_text(size = 14),
    legend.text   = element_text(size = 14)   
  )


# Second plot: columns 2,4,6,8,10
p2 <- ggplot(change_gg[change_gg$parameter == "scale",],
             aes(x, y = change, color = Estimator)
) +
  geom_line(size = 0.8) +
  theme_minimal() +
  ylab(expression(paste("SC(x, ", hat(sigma), ")")))+
  theme(
    axis.title = element_text(size = 14),   
    axis.text= element_text(size = 12),   
    legend.title  = element_text(size = 14),
    legend.text   = element_text(size = 14)
  )

ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")

