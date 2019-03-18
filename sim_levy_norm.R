### This file does some simulations to demonstrate how the sample standard deviation bhaves when the underlying data follows a Stable Distribution

### 0.1 Basic setup ###

rm(list = ls())
gc()

pacman::p_load(stabledist,tidyverse,data.table,foreach,doParallel,reshape)

### 1.1 Simulated Data ###

# Here, data of increasing samples is drawn from a Stable process. 
# 1000 draws are made for each sample size, and the standard deviation is computed in each case.
# This section requires parallelisation as it is quite intensive


if(!file.exists('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/testsd.Rda')){
  alpha <- 1.7 # Parameters for distribution
  beta <- 0
  gamma <- 1
  delta <- 0
  n <- 8 # max sample size is 10^n
  runs <- iter(1:1000) # nuber of iterations
  
  cl <- makePSOCKcluster(detectCores()) # begin parallel backend
  registerDoParallel(cl)
  
  start_time <- Sys.time() # time the code
  testsd <- foreach(i = runs, .combine = rbind, .packages = 'stabledist') %dopar% {
    c <- as.list(seq(1,n,1)) # sample sizes considered
    c <- lapply(c, function(x) {
      sd(rstable(10^x, alpha, beta, gamma, delta, pm = 0)) # for each sample size, compute the standard deviation from the data
    })
    unlist(c)
  }
  end_time <- Sys.time()
  stopCluster(cl)
  time <- end_time - start_time # 1h48m
  
  save(testsd, file = 'C:/Users/JulianW/Documents/Work/Productivity Dispersion/testsd.Rda') # save so we don't need to do it again
  rm(alpha, beta, gamma, delta, i, start_time, end_time, time)
} else {
  load('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/testsd.Rda')
}

### 1.2 Generate Summary Statistics ###

sd_sum <- as.data.frame(testsd) 
sd_sum <- melt(sd_sum)
class(sd_sum$variable) <- 'character'
sd_sum <- sd_sum %>% # for each sample size, we want the average standard deviation, as well as important percentiles (25th, 50th, 75th)
  group_by(variable) %>%
  mutate(mean = mean(value),
         median = median(value),
         topquint = quantile(value, 0.95),
         botquint = quantile(value, 0.05))

sd_sum_srt <- sd_sum %>%
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  select(-value)
  
### 1.3 Plot Results ###

pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/sample_std_dev_alpha_17.pdf', height = 6, width = 8)
par(mfcol = c(1,1))

plot(range(sd_sum_srt$variable), 
     range(sd_sum_srt$topquint, sd_sum_srt$botquint), 
     xlab = 'Sample Size, log base 10 scale', 
     ylab = 'Sample Standard Deviation',
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
polygon(c(sd_sum_srt$variable, rev(sd_sum_srt$variable)), 
        c(sd_sum_srt$topquint, rev(sd_sum_srt$botquint)),
        col = "grey90", border = NA)
lines(sd_sum_srt$variable,
      sd_sum_srt$mean,
      lty = 1,
      col = 'red',
      lwd = 2)
lines(sd_sum_srt$variable,
      sd_sum_srt$median,
      lty = 2,
      col = 'blue',
      lwd = 2)
legend('topleft',
       c('Mean', 'Median'),
       bty = 'n',
       cex = 1.3, 
       col = c('red','blue'), 
       lty = c(1, 2), lwd = 2)

dev.off()

### 2.1 Real Data ###

# if productivity follows a stable process, then taking subsamples of the full data set of varying sizes, and measure their dispersion using the standard deviation, we should get the same results as before

load('D:/Firm-Level Data/Amadeus/All_list_Cleaned.Rda') # load full list

prod <- rbindlist(All_list_Cleaned) # turn into 1 dataframe that we can take lists from
# prod_l <- prod$LP_g[!is.na(prod$LP_g) & !is.infinite(prod$LP_g)]
prod_l <- prod$LP[!is.na(prod$LP)] # list productivity outcomes

runs <- iter(1:1000) # how many interations of each sample size
n <- 7 # change depending on number of observations

cl <- makePSOCKcluster(detectCores()) # register parallel backend
registerDoParallel(cl)

start_time <- Sys.time() # time the code
data_sd <- foreach(i = runs, .combine = rbind) %dopar% { # foreach loop the returns one set of simulated samples, of size 1 to 10^n
  c <- as.list(seq(1,n,1)) # vector sample sizes 10^1 to 10^n
  c <- lapply(c, function(x) {
    sd(sample(prod_l, 10^x, replace = FALSE), na.rm = TRUE) # standard deviation of the sample of draws from data
  })
  unlist(c) # return the vector, which foreach combines into a df
}
end_time <- Sys.time()
stopCluster(cl) # close the backend
time <- end_time - start_time # 3m

### 2.2 Summary Statistics ### 
sd_sum_r <- as.data.frame(data_sd) # create DF
sd_sum_r <- melt(sd_sum_r) # reshape into long format
class(sd_sum_r$variable) <- 'character' # turn into character
sd_sum_r <- sd_sum_r %>% # take the relevant statistics
  group_by(variable) %>%
  mutate(mean = mean(value),
         median = median(value),
         topquint = quantile(value, 0.95),
         botquint = quantile(value, 0.05))

sd_sum_r_srt <- sd_sum_r %>% # keep the statistics we are interested in
  group_by(variable) %>%
  slice(1) %>%
  ungroup() %>%
  select(-value)

### 2.3 Results together ###

# Plot the standard deviations
pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/stdev_sim.pdf', height = 6, width = 16)
par(mfcol = c(1,2))

plot(range(sd_sum_srt$variable), 
     range(sd_sum_srt$topquint, sd_sum_srt$botquint), 
     xlab = 'Sample Size, log base 10 scale', 
     ylab = 'Sample Standard Deviation',
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
polygon(c(sd_sum_srt$variable, rev(sd_sum_srt$variable)), 
        c(sd_sum_srt$topquint, rev(sd_sum_srt$botquint)),
        col = "grey90", border = NA)
lines(sd_sum_srt$variable,
      sd_sum_srt$mean,
      lty = 1,
      col = 'red',
      lwd = 2)
lines(sd_sum_srt$variable,
      sd_sum_srt$median,
      lty = 2,
      col = 'blue',
      lwd = 2)
legend('topleft',
       c('Mean', 'Median'),
       bty = 'n',
       cex = 1.3, 
       col = c('red','blue'), 
       lty = c(1, 2), lwd = 2)
title('Simulated Data')

plot(range(sd_sum_r_srt$variable), 
     range(sd_sum_r_srt$topquint, sd_sum_r_srt$botquint), 
     xlab = 'Sample Size, log base 10 scale', 
     ylab = 'Sample Standard Deviation',
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
polygon(c(sd_sum_r_srt$variable, rev(sd_sum_r_srt$variable)), 
        c(sd_sum_r_srt$topquint, rev(sd_sum_r_srt$botquint)),
        col = "grey90", border = NA)
lines(sd_sum_r_srt$variable,
      sd_sum_r_srt$mean,
      lty = 1,
      col = 'red',
      lwd = 2)
lines(sd_sum_r_srt$variable,
      sd_sum_r_srt$median,
      lty = 2,
      col = 'blue',
      lwd = 2)
abline(h = sd(prod_l),
       lty = 3,
       col = 'black',
       lwd = 2)
legend('topleft',
       c('Mean', 'Median', 'Actual'),
       bty = 'n',
       cex = 1.3, 
       col = c('red','blue', 'black'), 
       lty = c(1, 2, 3), lwd = 2)
title('Productivity Data')

dev.off()

