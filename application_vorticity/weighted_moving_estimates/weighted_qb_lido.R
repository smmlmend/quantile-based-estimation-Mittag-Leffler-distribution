
library(MittagLeffleR)
library(lubridate)
load("45-60N_40-0W.RData")
### Zeit aendern zu der tatsaechlichen (wir betrachten nur alle 6 
### Stunden)
for (i in 1:656) {
  out_6h$WW[[i]] <- 6*out_6h$WW[[i]]
}

time <- list()
### Umschreiben in Daten
for (j in 1:656) {
  time[[j]] <- lubridate::as_datetime(out_6h$TT[[j]]*3600, 
                                      origin = "1900-01-01")[-1216]
}

#### weighted log moments estimator

weighted.LogMomentEstimator <- function(x, w, alpha = 0.05){
  EULER.C = 0.577215664901533
  log.x = log(x)
  w <- w/sum(w)
  m = sum(w*log.x)
  v2 <- sum(w^2)
  s.2 = (1/(1-v2))*sum(w*(log.x-m)^2)
  nu = pi/sqrt(3 * (s.2 + pi^2/6))
  delta = exp(m + EULER.C)
  n = length(x)
  se.nu = sqrt((nu^2) * (32 - 20 * nu^2 - nu^4)/(40 * n))
  zcv = stats::qnorm(1 - alpha/2, 0, 1)
  l.nu = nu - zcv * se.nu
  u.nu = nu + zcv * se.nu
  se.delta = sqrt(((pi^2 * delta^2)/(6 * n)) * ((2/nu^2) - 
                                                  1))
  l.delta = delta - zcv * se.delta
  u.delta = delta + zcv * se.delta
  return(c(tail = nu, scale = delta, tailLo = l.nu, tailHi = u.nu, 
           scaleLo = l.delta, scaleHi = u.delta))
}

## weighted qbe ------------------------------------------------------------

## for a p-quantile

weighted.quantile <- function(x, w, p){
  q <- numeric(length(p))
  names(q) <- paste(p)
  w <- w/sum(w)
  w <- w[order(x, decreasing = TRUE)]
  for (j in 1:length(p)) {
    l <- min(which(cumsum(w) >= 1-p[j]))
    if(cumsum(w)[l] == 1-p[j]){
      q[j] <- mean(sort(x, decreasing = TRUE)[c(l+1,l)])
    }
    else{
      q[j] <- sort(x, decreasing = TRUE)[l]
    }
  }
  return(q)
}

### now just use weighted quantiles in the quantile based estimation

weighted.mlqbe <- function(data, weights, q){
  quants <- weighted.quantile(data, weights, q)
  a <- function(params) {
    sum((q - pml(quants, params[1], params[2]))^2)}
  b <- optim(logMomentEstimator(data)[1:2], a, 
             method = "L-BFGS-B", 
             lower = c(010^-5,10^-5), 
             upper = c(1-10^-8, 10^6))
  return(b)
}



# weighted mittag leffler estimation --------------------------------------


moving_mle <- function(data, times, k, type, q = c(0.1, 0.3, 0.5, 0.8, 0.925) ){
  year(times) <- 2002
  days <- as.POSIXct(
    seq(as.Date("2002/1/1"), by = "day", length.out = 365))
  res <- data.frame(matrix(ncol = 3, nrow = 365))
  res[,1] <- days
  if (type == "logmom") {
    for (i in 1:365) {
      use <- which(abs(difftime(days[i], times, units = "days")) <= k |
                     abs(difftime(days[i], times, units = "days")) >= (365 - k))
      
      x <- abs(difftime(days[i], times[use], units = "days"))
      x[x > k] <- 365 - x[x > k]
      w <- 3/(4*k)*(1-(as.numeric(x)/k)^2)
      
      if(length(use) < 2){
        res[i, 2:3] <- NA
      }
      
      else{
        res[i, 2:3] <- weighted.LogMomentEstimator(data[use], w)[1:2]
      }
    }
  }
  if (type == "ml") {
    for (i in 1:365) {
      print(i)
      
      use <- which(abs(difftime(days[i], times, units = "days")) <= k |
                     abs(difftime(days[i], times, units = "days")) >= (365 - k))
      x <- abs(difftime(days[i], times[use], units = "days"))
      x[x > k] <- 365 - x[x > k]
      w <- 3/(4*k)*(1-(as.numeric(x)/k)^2)
      
      if(length(use) < 2){
        res[i, 2:3] <- NA
      }
      
      else{
        res[i, 2:3] <- weighted.mlmle(data[use], w)$par
      }
    }
  }
  if(type == "qb"){
    for (i in 1:365) {
      
      use <- which(abs(difftime(days[i], times, units = "days")) <= k |
                     abs(difftime(days[i], times, units = "days")) >= (365 - k))
      x <- abs(difftime(days[i], times[use], units = "days"))
      x[x > k] <- 365 - x[x > k]
      w <- 3/(4*k)*(1-(as.numeric(x)/k)^2)
      
      if(length(use) < 2){
        res[i, 2:3] <- NA
      }
      
      else{
        res[i, 2:3] <- weighted.mlqbe(data[use], w, q = q)$par
      }
    }
  }
  return(res)
}

### which place out of the 656 grid points



for (j in 1:656) {
  print(j)

daily_res <- data.frame(matrix(nrow = 365, ncol = 5))
colnames(daily_res) <- c("lon", "lat", "day", "tail", "scale")
daily_res[,1] <- rep(out_6h$lon[j], 365)
daily_res[,2] <- rep(out_6h$lat[j], 365)
daily_res[,3] <- 1:365


daily_res[,4:5] <- moving_mle(out_6h$WW[[j]], 
                              time[[j]], 45, type = "qb")[,-1]


### save dataset:
filename <- paste("weighted_qb_", j, ".RData", sep = "")

save(daily_res, file = filename)
}

print("done")