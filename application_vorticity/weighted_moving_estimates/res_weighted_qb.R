####### Analyze weighted qb Lido

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(scales)
library(viridis)
library(ggnewscale)
library(MittagLeffleR)

## load original data
setwd("~/paper2/application_vorticity/")
load("45-60N_40-0W.RData")


## load estimates
setwd("~/paper2/application_vorticity/weighted_moving_estimates")

seq(ymd("2022-01-01"), ymd("2022-12-31"), by = "days")[274]


tail_qb <- matrix(nrow = 656, ncol = 4)
scale_qb <- matrix(nrow = 656, ncol = 4)

tail_ml <- matrix(nrow = 656, ncol = 4)
scale_ml <- matrix(nrow = 656, ncol = 4)

for(k in (1:656)){
  print(k)
  filename <- paste("weighted_qb_", k, ".RData", sep = "")
  load(filename)
  tail_qb[k,] <- daily_res$tail[c(1, 91, 182, 274)]
  scale_qb[k,] <- daily_res$scale[c(1, 91, 182, 274)]
  
  assign(paste("res_", k, sep = ""), daily_res)
  
  filename <- paste("weighted_ml_", k, ".RData", sep = "")
  load(filename)
  daily_res$tail[daily_res$tail > 1] <- 1
  tail_ml[k,] <- daily_res$tai[c(1, 91, 182, 274)]
  scale_ml[k,] <- daily_res$scale[c(1, 91, 182, 274)]
  assign(paste("res_ml_", k, sep = ""), daily_res)
}


load("weighted_qb_exp_12.RData")
load("empirical_median_12.RData")
load("empirical_075quantile_12.RData")
load("empirical_prob48_12_b.RData")
load("empirical_prob72_12.RData")

# Create a combined data frame from all 656 res_k objects


# Assume each res_k$tail and $scale has the same length, e.g. 100 days
n_days <- length(res_1$tail)
days <- seq(ymd("2022-01-01"), ymd("2022-12-31"), by = "days")

# Combine all into one data frame
all_data <- lapply(1:656, function(k) {
  df <- get(paste0("res_", k))
  data.frame(
    day = days,
    tail = df$tail,
    scale = df$scale,
    id = paste0("res_", k)
  )
}) %>% bind_rows()


res_12_df <- data.frame(
  day = days,
  tail = res_12$tail,
  scale = res_12$scale
)

daily_res$day <- days

# Tail plot
p1 <- ggplot() +
  geom_line(data = all_data, aes(x = day, y = tail, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = tail),
            color = "black", size = 0.5) +
  labs(x = "day", y = "tail") +
  theme_minimal()+
  labs(x = element_blank(), y = expression("Estimate " * hat(beta)))+
  ylim(0.5, 1.05)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

# Scale plot
p2 <- ggplot() +
  geom_line(data = all_data, aes(x = day, y = scale, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = scale),
            color = "black", size = 0.5) +
  #geom_line(data = daily_res, aes(x = day, y = scale),
  #          color = "red", size = 0.5) +
  labs(x = "day", y = "scale") +
  theme_minimal()+
  labs(x = element_blank(), y = expression("Estimate " * hat(sigma)))+
  ylim(150, 2250)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

grid.arrange(p1, p2, ncol = 2)




#sum(all_data[month(all_data$day) %in% c( 7, 8),2] >= 0.8)/
#  length(all_data[month(all_data$day) %in% c( 7, 8),2])

#sum(all_data[month(all_data$day) %in% c( 4, 5),2] <= 0.8)/
#  length(all_data[month(all_data$day) %in% c( 4, 5),2])


#summary(all_data[month(all_data$day) %in% c(6, 7, 8),3] )
#quantile(all_data[month(all_data$day) %in% c(6, 7, 8),3] , 0.1)

#summary(all_data[month(all_data$day) %in% c(1, 2, 12),3] )
#quantile(all_data[month(all_data$day) %in% c(1, 2, 12),3] , 0.9)



# P48 ------------------------------------------------------------------


#all_data$P_24 <- numeric(nrow(all_data))
all_data$P_48 <- numeric(nrow(all_data))
all_data$P_72 <- numeric(nrow(all_data))
all_data$median <- numeric(nrow(all_data))
all_data$quartile3 <- numeric(nrow(all_data))

for (i in 1:nrow(all_data)) {
  print(i)
  #all_data$P_48[i] <- pml(48, all_data$tail[i], all_data$scale[i])
  all_data$P_72[i] <- pml(72, all_data$tail[i], all_data$scale[i])
  #all_data$P_24[i] <- pml(24, all_data$tail[i], all_data$scale[i])
  #all_data$median[i] <- qml(0.5, all_data$tail[i], all_data$scale[i])
  #all_data$quartile3[i] <- qml(0.75, all_data$tail[i], all_data$scale[i])
}

#daily_res$P_24 <- numeric(365)
daily_res$P_48 <- numeric(365)
daily_res$P_72 <- numeric(365)
daily_res$median<- numeric(365)
daily_res$quartile3 <- numeric(365)

for (i in 1:365) {
  print(i)
  daily_res$P_48 [i] <- pml(48, daily_res$tail[i], daily_res$scale[i])
  daily_res$P_72 [i] <- pml(72, daily_res$tail[i], daily_res$scale[i])
  daily_res$median[i] <- qml(0.5, daily_res$tail[i], daily_res$scale[i])
  daily_res$quartile3[i] <- qml(0.75, daily_res$tail[i], daily_res$scale[i])
}


res_12_df$P_48 <- all_data$P_48[all_data$id == "res_12"]
res_12_df$P_72 <- all_data$P_72[all_data$id == "res_12"]
res_12_df$median <- all_data$median[all_data$id == "res_12"]
res_12_df$quartile3 <- all_data$quartile3[all_data$id == "res_12"]
#res_12_df$P_24 <- all_data$P_24[all_data$id == "res_12"]

empirical <- data.frame(day = days)
empirical$median <- weighted_median
empirical$quartile3 <- weighted_quartile3
empirical$quantile065 <- weighted_quantile065
empirical$P_48 <- weighted_probability_48
empirical$P_72 <- weighted_probability_72

p4 <- ggplot() +
  geom_line(data = all_data, aes(x = day, y = P_48, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = P_48),
            color = "black", size = 0.5) +
   geom_line(data = daily_res, aes(x = day, y = P_48),
            color = "darkred", size = 0.5) +
  geom_line(data = empirical, aes(x = day, y = P_48),
            color = "blue", size = 0.5) +
  labs(x = "day", y = "scale") +
  theme_minimal()+
  labs(x = element_blank(), y = "P(T(u) < 48 h)")+
  ylim(0, 0.32)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )


p3 <- ggplot() +
  geom_line(data = all_data, aes(x = day, y = median, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = median),
            color = "black", size = 0.5) +
  geom_line(data = daily_res, aes(x = day, y = median),
            color = "darkred", size = 0.5) +
  geom_line(data = empirical, aes(x = day, y = median),
            color = "blue", size = 0.5)+
  labs(x = "day", y = "scale") +
  theme_minimal()+
  labs(x = element_blank(), y = "Median return time")+
  ylim(0, 1400)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )


p5 <- ggplot() +
  geom_line(data = all_data, aes(x = day, y = quartile3, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = quartile3),
            color = "black", size = 0.5) +
  geom_line(data = daily_res, aes(x = day, y = quartile3),
            color = "darkred", size = 0.5) +
  geom_line(data = empirical, aes(x = day, y = quartile3),
            color = "blue", size = 0.5,
            linetype = "dashed")+
  labs(x = "day", y = "scale") +
  theme_minimal()+
  labs(x = element_blank(), y = "0.75 quantile of the return time")+
  ylim(0, 2400)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )


p6 <- ggplot()  +
  geom_line(data = all_data, aes(x = day, y = P_72, group = id),
            color = "grey", alpha = 0.1) +
  geom_line(data = res_12_df, aes(x = day, y = P_72),
            color = "black", size = 0.5) +
  geom_line(data = daily_res, aes(x = day, y = P_72),
            color = "darkred", size = 0.5) +
  geom_line(data = empirical, aes(x = day, y = P_72),
            color = "blue", size = 0.5,
            linetype = "dashed") +
  labs(x = "day", y = "scale") +
  theme_minimal()+
  labs(x = element_blank(), y = "P(W < 72 h)")+
  ylim(0, 0.4)+
  scale_x_date(labels = date_format("%b %d"))+
  theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

#grid.arrange(p3, p4, p5,  ncol = 3)
grid.arrange(p5, p6,  ncol = 2)


res_12_df$day[which.min(res_12_df$quartile3)]
res_12_df$day[which.max(res_12_df$P_72)]

res_12_df$day[which.max(res_12_df$median)]
res_12_df$day[which.min(res_12_df$P_48)]
#### Map Plot

library(sf)
library(tidyverse)
world_coordinates <- map_data("world")


#out_6h <- cbind(out_6h, tail_qb, scale_qb, tail_ml, scale_ml)

#names(out_6h)[8:23] <- c("tail_qb_jan1", "tail_qb_apr1", "tail_qb_jul1", "tail_qb_oct1",
#                         "scale_qb_jan1", "scale_qb_apr1", "scale_qb_jul1", "scale_qb_oct1",
#                         "tail_ml_jan1", "tail_ml_apr1", "tail_ml_jul1", "tail_ml_oct1",
#                         "scale_ml_jan1", "scale_ml_apr1", "scale_ml_jul1", "scale_ml_oct1")


# facet plot --------------------------------------------------------------


out_6h <- cbind(out_6h[rep(1:656, 4), ] , as.vector(tail_qb), as.vector(scale_qb), 
                rep(c("Jan 01", "Apr 01", "Jul 01", "Oct 01"), each = 656))
names(out_6h)[8:10] <- c("tail", "scale", "date")
out_6h$date <- factor(out_6h$date, levels = c("Jan 01", "Apr 01", "Jul 01", "Oct 01"))




# Prepare data with parameter type
beta_df <- out_6h %>%
  mutate(param = "hat(beta)",
         value = tail)

sigma_df <- out_6h %>%
  mutate(param = "hat(sigma)",
         value = scale)

combined_df <- bind_rows(beta_df, sigma_df)


ggplot() +
  # Base map
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white", size = 0.3
  ) +
  scale_x_continuous(
    limits = c(-40.5, 0.5), expand = c(0,0),
    breaks = seq(-40, 0, by = 10),
    labels = function(x) paste0(abs(x), "°", ifelse(x < 0, "W", ""))
  ) +
  scale_y_continuous(
    limits = c(44.5, 60.5), expand = c(0,0),
    breaks = seq(50, 60, by = 10),
    labels = function(y) paste0(abs(y), "°", ifelse(y < 0, "S", "N"))
  ) +
  coord_sf(crs = st_crs(4326)) +
  
  # ---- Beta layer ----
geom_raster(
  data = subset(combined_df, param == "hat(beta)"),
  aes(lon, lat, fill = value), alpha = 0.65
) +
  scale_fill_viridis_c(
    na.value = "white", option = "C", direction = -1,
    limits = c(0.5, 1),
    name = expression(hat(beta)*":"),
    guide = guide_colorbar(order = 1, title.vjust = 1.02)
  ) +
  
  # ---- New fill scale for sigma ----
new_scale_fill() +
  
  geom_raster(
    data = subset(combined_df, param == "hat(sigma)"),
    aes(lon, lat, fill = value), alpha = 0.65
  ) +
  scale_fill_gradientn(
    colors = terrain.colors(10),
    limits = c(174.1, 2502), trans = "log10",
    breaks = c(250, 500, 1000, 2000),
    name = expression(hat(sigma)*":"),
    guide = guide_colorbar(order = 2, title.vjust = 0.9)
  ) +
  
  # ---- Facet so param becomes rows, date becomes columns ----
facet_grid(param ~ date, 
           labeller = labeller(
             param = label_parsed,  # parse just param
             date  = label_value    # keep date as text
           )) +
  labs(x = NULL, y = NULL) +
  theme(panel.spacing = unit(0.5, "cm"), 
        strip.text = element_text(size = 10), 
        legend.position = "bottom",
        legend.text  = element_text(size = 9,),
        legend.key.width = unit(1.5, "cm"),
        legend.justification.bottom = "left")


sum(beta_df$tail[beta_df$date == "Jul 01"] >= 0.999999)/656

sum(beta_df$tail[beta_df$date == "Jul 01"] >= 0.999999) /
  length(beta_df$tail[beta_df$date == "Jul 01"])

sum((beta_df$tail[beta_df$date == "Jul 01"] > 
       beta_df$tail[beta_df$date == "Oct 01"]) & 
      (beta_df$tail[beta_df$date == "Jul 01"] > 
         beta_df$tail[beta_df$date == "Jan 01"]) & 
      (beta_df$tail[beta_df$date == "Oct 01"] > 
         beta_df$tail[beta_df$date == "Apr 01"]) & 
      (beta_df$tail[beta_df$date == "Jan 01"] > 
         beta_df$tail[beta_df$date == "Apr 01"]) & 
      (beta_df$tail[beta_df$date == "Jul 01"] > 
         beta_df$tail[beta_df$date == "Apr 01"]) 
)


sum((sigma_df$scale[sigma_df$date == "Jul 01"] > 
  sigma_df$scale[sigma_df$date == "Oct 01"]) & 
    (sigma_df$scale[sigma_df$date == "Jul 01"] > 
    sigma_df$scale[sigma_df$date == "Apr 01"]) & 
    (sigma_df$scale[sigma_df$date == "Oct 01"] > 
       sigma_df$scale[sigma_df$date == "Jan 01"]) & 
    (sigma_df$scale[sigma_df$date == "Apr 01"] > 
       sigma_df$scale[sigma_df$date == "Jan 01"]) 
  )
