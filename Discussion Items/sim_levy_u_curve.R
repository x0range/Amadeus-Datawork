# This file runs the simulation for relationship between mean and standard deviation

rm(list = ls()) # clear environment
gc() # garbage collection

pacman::p_load(stabledist,tidyverse,foreach,reshape) # load packages

alpha <- 1.7 # Levy parameters
gamma <- 1
delta <- 0
n <- 100000 # sample size
runs <- iter(1:1000) # iterations

cl <- makePSOCKcluster(detectCores()) # register parallel backend
registerDoParallel(cl)

meansd <- foreach(i = runs, .combine = rbind, .packages = 'stabledist') %dopar% { # foreach loop the returns one set of simulated samples, of size 1 to n
  sample <- rstable(n, alpha, 0, gamma, delta, pm = 0) # generate sample
  c <- list()
  c[1] <- mean(sample)# mean of the sample
  c[2] <- sd(sample) # standard deviation of the sample
  sample <- rstable(n, alpha, 1, gamma, delta, pm = 0) # introduce skew
  c[3] <- mean(sample)# mean of the sample
  c[4] <- sd(sample) # standard deviation of the sample
  unlist(c) # return the vector, which foreach combines into a df
}

stopCluster(cl) # close the backend
rm(cl)

meansd_sum <- as.data.frame(meansd) # create DF
colnames(meansd_sum) <- c('Mean', 'StdDev', 'Mean_skew', 'StdDev_skew')

pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/u_curve_levy.pdf', height = 12, width = 8)
par(mfcol = c(2,1))

plot(meansd_sum$Mean, 
     meansd_sum$StdDev, 
     xlab = 'Sample Mean', 
     ylab = 'Sample Standard Deviation',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3)
title(expression(paste(beta, ' = 0', sep = '')))

plot(meansd_sum$Mean_skew, 
     meansd_sum$StdDev_skew, 
     xlab = 'Sample Mean', 
     ylab = 'Sample Standard Deviation',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3)
title(expression(paste(beta, ' = 1', sep = '')))

dev.off()
