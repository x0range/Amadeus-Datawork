# This file runs the simulation for Interquartile Ranges

rm(list = ls()) # clear environment
gc() # garbage collection

pacman::p_load(tidyverse,xtable,stabledist,plotly) # load packages

iqr_tab <- as.data.frame(rbind(c(0.95,0.05,1.5,0,1,0), # create the parameters for the relevant Levy distributions
                               c(0.95,0.05,1.3,0,1,0),
                               c(0.9,0.1,1.5,0,1,0),
                               c(0.9,0.1,1.3,0,1,0),
                               c(0.75,0.25,1.5,0,1,0),
                               c(0.75,0.25,1.3,0,1,0),
                               c(0.95,0.05,1.3,0,1,0),
                               c(0.95,0.05,1.3,1,1,0),
                               c(0.9,0.1,1.3,0,1,0),
                               c(0.9,0.1,1.3,1,1,0),
                               c(0.75,0.25,1.3,0,1,0),
                               c(0.75,0.25,1.3,1,1,0)))
colnames(iqr_tab) <- c('Top', 'Bottom', 'a', 'b', 'g', 'd')

iqr_tab <- iqr_tab %>%
  rowwise() %>%
  mutate(IQR = qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)) %>% # find the relevant IQRs for the given parameters
  ungroup() %>%
  mutate(pc_IQR = ifelse((row_number() %% 2) == 0, (IQR - lag(IQR,1))/lag(IQR,1), NA), # we want to compute the pairwise differences (row 2 vs 1, 4 vs 3 - not 3 vs 2!) - the ifelse checks whether the row is even before computing a number
         pc_IQR = round(100*pc_IQR, digit = 2)) # express as %

print(xtable(iqr_tab, caption = 'Interquartile Range as a Measure of Dispersion'), include.rownames = FALSE) # paste table into latex
# Scale of distribution is affected by gamma - direct impact on IQR, but alpha and beta can also change it

# Plot IQR as a function of beta
# First set parameter values

a <- 1.5
g <- 1
d <- 0
Top <- 0.75
Bottom <- 0.25

IQR <- list()

for(i in 0:200){
  b <- (i - 100)/100 # these are the possible beta values
  IQR[i+1] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
}

# Create plot

pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/beta_IQR.pdf', height = 6, width = 8)

plot(range(beta),
     range(IQR),
     type = 'n',
     cex = 0.9,
     xlab = 'Beta', 
     ylab = 'IQR',
     cex.axis = 1.3,
     cex.lab = 1.3)
lines(beta,
      IQR,
      lty = 1,
      col = 'steelblue',
      lwd = 2)

dev.off()

# Surface plots - how does the IQR change for choices of beta, alpha, and the relevant precentiles
# Consider 3 alpha values, 1.7, 1.5, 1.2 -1 (Cauchy) will cause problems
# Consider IQRs from 30% to 1%

a <- 1.7
IQR_1 <- matrix(NA, nrow = 201, ncol = 30) # initiate empty matrix - the matrix has beta values as rows, and percentiles as columns

# assign row and col names, which are reffered to when plotting
rownames(IQR_1) <- (seq(1:201) - 101)/100
colnames(IQR_1) <- paste((seq(1:30)), '%', sep = '')

for(j in 1:30){ # run over all IQRs
  print(j)
  Top <- (100 - j) / 100 # top percentile
  Bottom <- j / 100 # bottom percentile
  
  for(i in 1:201){ # compute over all betas
    b <- (i - 101)/100
    IQR_1[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

# repeat for alphas 1.5 and 1.2
a <- 1.5
IQR_2 <- matrix(NA, nrow = 201, ncol = 30)

rownames(IQR_2) <- (seq(1:201) - 101)/100
colnames(IQR_2) <- paste((seq(1:30)), '%', sep = '')

for(j in 1:30){
  print(j)
  Top <- (100 - j) / 100
  Bottom <- j / 100
  
  for(i in 1:201){
    
    b <- (i - 101)/100
    IQR_2[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

a <- 1.2
IQR_3 <- matrix(NA, nrow = 201, ncol = 30)

rownames(IQR_3) <- (seq(1:201) - 101)/100
colnames(IQR_3) <- paste((seq(1:30)), '%', sep = '')

for(j in 1:30){
  print(j)
  Top <- (100 - j) / 100
  Bottom <- j / 100
  
  for(i in 1:201){
    
    b <- (i - 101)/100
    IQR_3[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

# plot_ly is used for the surface plot - it is elegant and easy to use

# x and y are lists of axes options

x <- list(
  title = 'Percentile', # title of axis
  tickmode = 'array', # how to mark the ticks
  tickvals = (c(1, 5, 10, 15, 25) -1), # location for tick marks - subrtact 1 because plot starts at 0
  ticktext = colnames(IQR_1)[c(1, 5, 10, 15, 25)] # values for the tick marks
)

# repeat for y 
y <- list(
  title = 'beta',
  tickvals = (c(1, 50, 100, 150, 201) -1),
  ticktext = rownames(IQR_1)[c(1, 51, 101, 151, 201)]
)

plot_ly(showscale = FALSE) %>% # data will be added seperately since three plots should be combined
  add_surface(z = ~IQR_1) %>% # first plot - alpha = 1.7
  add_surface(z = ~IQR_2, opacity = 0.7) %>% # first plot - alpha = 1.7 - add opacity to make is easier to see
  add_surface(z = ~IQR_3, opacity = 0.7) %>% # first plot - alpha = 1.7
  layout(
    title = "Skew and IQRs",
    scene = list( # this is the option that allows manipulation of axis titles, tick marks, camera angle, etc
      xaxis = x,
      yaxis = y,
      zaxis = list(title = 'IQR')
    ))
