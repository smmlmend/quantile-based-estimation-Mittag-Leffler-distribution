
setwd("~/paper2/application_vorticity/")
load("45-60N_40-0W.RData")
for (i in 1:656) {
  out_6h$WW[[i]] <- 6*out_6h$WW[[i]]
}

time <- list()
### Umschreiben in Daten
for (j in 1:656) {
  time[[j]] <- lubridate::as_datetime(out_6h$TT[[j]]*3600, 
                                      origin = "1900-01-01")[-1216]
}


weighted.quantile <- function(x, w, p){
  q <- numeric(length(p))
  names(q) <- paste(p)
  w <- w/sum(w)
  w <- w[order(x, decreasing = TRUE)]
  for (j in 1:length(p)) {
    l <- min(which(cumsum(w) >= 1-p[j]))
    if(cumsum(w)[l] == 1-p[j]){
      q[j] <- mean(sort(x, decreasing = TRUE)[c(l + 1,l)])
    }
    else{
      q[j] <- sort(x, decreasing = TRUE)[l]
    }
  }
  return(q)
}


moving_quantile <- function(data, times, k, p ){
  year(times) <- 2002
  days <- as.POSIXct(
    seq(as.Date("2002/1/1"), by = "day", length.out = 365))
  res <- data.frame(matrix(ncol = 3, nrow = 365))
  res[,1] <- days
  weighted_median <- numeric(365)
  for (i in 1:365) {
  
  use <- which(abs(difftime(days[i], times, units = "days")) <= k |
                 abs(difftime(days[i], times, units = "days")) >= (365 - k))
  x <- abs(difftime(days[i], times[use], units = "days"))
  x[x > k] <- 365 - x[x > k]
  w <- 3/(4*k)*(1-(as.numeric(x)/k)^2)
  
  if(length(use) < 1){
    weighted_median[i] <- NA
  }
  
  else{
    weighted_median[i] <- weighted.quantile(data[use], w, p = p)
  }
  }
  return(weighted_median)
}

weighted_median <-moving_quantile(out_6h$WW[[12]], time[[12]], 45, p = 0.5)
weighted_quartile3 <-moving_quantile(out_6h$WW[[12]], time[[12]], 45, p = 0.75)
weighted_quantile065 <-moving_quantile(out_6h$WW[[12]], time[[12]], 45, p = 0.65)

setwd("~/paper2/application_vorticity/weighted_moving_estimates")

save(weighted_median, file = "empirical_median_12.RData")

save(weighted_quartile3, file = "empirical_075quantile_12.RData")


######### probability of another vorticity extreme

moving_probability <- function(data, times, k, h ){
  year(times) <- 2002
  days <- as.POSIXct(
    seq(as.Date("2002/1/1"), by = "day", length.out = 365))
  res <- data.frame(matrix(ncol = 3, nrow = 365))
  res[,1] <- days
  weighted_probability <- numeric(365)
  for (i in 1:365) {
    
    use <- which(abs(difftime(days[i], times, units = "days")) <= k |
                   abs(difftime(days[i], times, units = "days")) >= (365 - k))
    x <- abs(difftime(days[i], times[use], units = "days"))
    x[x > k] <- 365 - x[x > k]
    w <- 3/(4*k)*(1-(as.numeric(x)/k)^2)
    
    if(length(use) < 1){
      weighted_probability[i] <- NA
    }
    
    else{
      w <- w/sum(w)
      weighted_probability[i] <- sum( as.numeric(data[use] < h) * w )
    }
  }
  return(weighted_probability)
}

weighted_probability_48 <- moving_probability(out_6h$WW[[12]], time[[12]], 45, h = 48)

save(weighted_probability_48, file = "empirical_prob48_12_b.RData")

weighted_probability_72 <- moving_probability(out_6h$WW[[12]], time[[12]], 45, h = 72)

save(weighted_probability_72, file = "empirical_prob72_12.RData")
