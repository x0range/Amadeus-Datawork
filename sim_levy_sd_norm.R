# This file simulates the volatility of sample standard deviations
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

pop <- as.list(rstable(p, alpha, beta, gamma, delta, pm = 0))
pop_sd <- sd(unlist(pop))
pop_n <- as.list(rnorm(p, mean = 0, sd = pop_sd))
pop_n_sd <- sd(unlist(pop_n))

samsd <- list()
samsd_n <- list()

for(i in 1:runs){
  samsd[i] <- sd(unlist(sample(pop, n))) # this might take some time
  samsd_n[i] <- sd(unlist(sample(pop_n, n)))
}

iterations <- list(1:runs)

pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/comp_norm_levy_sd.pdf', height = 12, width = 8)
par(mfcol = c(2,1))

plot(unlist(iterations), 
     log10(unlist(samsd)), 
     xlab = 'Iteration', 
     ylab = 'Sample Standard Deviation',
     yaxt = 'n',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3)
aty <- axTicks(2)
labels <- sapply(aty,function(i)
  as.expression(bquote(10^ .(i)))
)
axis(2,
     at = aty,
     labels = labels,
     cex.axis = 1.3)
title('Levy Stable')
abline(h = log10(pop_sd),
       lwd = 2,
       col = 'red')

plot(unlist(iterations), 
     (unlist(samsd_n)), 
     xlab = 'Iteration', 
     ylab = 'Sample Standard Deviation',
     cex = 0.6,
     pch = 20,
     cex.axis = 1.3,
     cex.lab = 1.3)
title('Standard Normal')

abline(h = (pop_n_sd),
       lwd = 2,
       col = 'red')

dev.off()
