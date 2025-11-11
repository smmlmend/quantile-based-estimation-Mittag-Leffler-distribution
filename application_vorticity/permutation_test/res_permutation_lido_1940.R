### analyze results permutation test

setwd("~/paper2/application_vorticity/results_1940-2023")

beta_perm <- numeric(656)
sigma_perm <- numeric(656)
for(k in (1:656)){
  print(k)
  filename <- paste("perm_test_1940_", k, ".RData", sep = "")
  load(filename)
  assign(paste("res_", k, sep = ""), res)
  beta_perm[k] <- sum(get(paste("res_", k, sep = ""))[[1]])
  sigma_perm[k] <- sum(get(paste("res_", k, sep = ""))[[2]])
}

sum(beta_perm <= 500)/656
which(beta_perm <= 500)
sum(sigma_perm <= 500)/656
which(sigma_perm <= 500)

hist(beta_perm, breaks = 20)
hist(sigma_perm, breaks = 20)

setwd("C:/Users/mendel/Documents/paper2/application_vorticity")
load("45-60N_40-0W.RData")



beta_significant <- numeric(656)
beta_significant[which(beta_perm <= 500)] <- 1

sigma_significant <- rep("H0", 656)
sigma_significant[which(sigma_perm <= 500)] <- "H1"

out_6h$beta_sig <- beta_significant

out_6h$sigma_sig <- sigma_significant


library(sf)
library(tidyverse)
world_coordinates <- map_data("world")

ggplot() + 
  
  # geom_map() function takes world coordinates  
  # as input to plot world map 
  geom_map( 
    data = world_coordinates, map = world_coordinates, 
    aes(long, lat, map_id = region) , color = "black",
    fill = "white", size = 0.3
  ) +
  scale_x_continuous(
    limits = c(-40.5, 0.5), 
    expand = c(0,0),
    name = "longitude",
    breaks = seq(-60, 30, by = 10),  # Define longitude tick positions
    labels = function(x) {
      paste0(abs(x), "°", ifelse(x < 0, "W", "E"))
    }
  ) +
  scale_y_continuous(
    limits = c(44.5, 60.5),  
    expand = c(0,0),
    name = "latitude",
    breaks = seq(30, 60, by = 10),  # Define latitude tick positions
    labels = function(y) {
      paste0(abs(y), "°", ifelse(y < 0, "S", "N"))
    }) +
  coord_sf(crs = st_crs(4326)) +
  geom_raster( data = out_6h, alpha = 0.7,
               aes(lon, lat, fill = sigma_sig)) +
  scale_fill_manual(values = c("H0" = "#BA52F2",  # Soft lavender
                               "H1" = "#F2D252") )+ 
  labs(x = NULL, y = NULL, fill = NULL)


