### This file simulates the volatility of sample standard deviations when data follows a Stable process


rm(list = ls()) # clear environment
gc() # garbage collection

pacman::p_load(stabledist,tidyverse,foreach,reshape) # load packages

alpha <- 1.7 # Levy parameters
beta <- 0
gamma <- 1
delta <- 0
p <- 10^8
n <- 10^5 # sample size
runs <- 1000 # iterations

# generate 'population' of random variables
pop <- as.list(rstable(p, alpha, beta, gamma, delta, pm = 0))
pop_sd <- sd(unlist(pop))
pop_n <- as.list(rnorm(p, mean = 0, sd = pop_sd))
pop_n_sd <- sd(unlist(pop_n))


# sampling simulation
samsd <- list()
samsd_n <- list()

for(i in 1:runs){
  samsd[i] <- sd(unlist(sample(pop, n))) # this might take some time
  samsd_n[i] <- sd(unlist(sample(pop_n, n)))
}

samsd_hist <- hist(as.numeric(samsd), breaks = 50, plot = FALSE)
samsd_n_hist <- hist(as.numeric(samsd_n), breaks = 50, plot = FALSE)

iterations <- list(1:runs)

# Generate plot
pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/comp_norm_levy_sd.pdf', height = 6, width = 16) # makes a PDF in selected folder
par(mfcol = c(1,2)) # two graphs, one per row

plot(range(samsd_hist$mids), # empty plot with axis titles
     range(samsd_hist$counts/runs), 
     xlab = 'Sample Standard Deviation', 
     ylab = 'Density',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
points(samsd_hist$mids,
       samsd_hist$counts/runs,
      lty = 1, 
      pch = 20,
      col = 'black')
# aty <- axTicks(2) # make custom scale
# labels <- sapply(aty,function(i)
#   as.expression(bquote(10^ .(i)))
# )
# axis(2,
#      at = aty,
#      labels = labels,
#      cex.axis = 1.3)
title('Levy Stable')
abline(v = pop_sd, # this is the actual population SD
       lwd = 2,
       lty = 2,
       col = 'red')

plot(range(samsd_n_hist$mids), # empty plot with axis titles
     range(samsd_n_hist$counts/n), 
     xlab = 'Sample Standard Deviation', 
     ylab = 'Density',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3,
     type = 'n')
points(samsd_n_hist$mids,
       samsd_n_hist$counts/n,
       lty = 1, 
       pch = 20,
       col = 'black')
title('Standard Normal')
abline(v = pop_n_sd,
       lwd = 2,
       col = 'red',
       lty = 2)

dev.off()
