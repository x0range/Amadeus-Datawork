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
  mutate(pc_IQR = ifelse((row_number() %% 2) == 0, (IQR - lag(IQR,1))/lag(IQR,1), NA),
         pc_IQR = round(100*pc_IQR, digit = 2)) # express as %

print(xtable(iqr_tab, caption = 'Interquartile Range as a Measure of Dispersion'), include.rownames = FALSE) # paste table into latex
# Scale of distribution is affected by gamma - direct impact on IQR, but alpha and beta can also change it

x <- seq(-10, 10, 0.1)
xx_1 <- dstable(x, alpha = 1.3, beta = 0, gamma = 2, delta = 0, pm = 0)
xx_2 <- dstable(x, alpha = 1.3, beta = 1, gamma = 2, delta = 0, pm = 0)

plot(range(x),
     range(xx_1, xx_2),
     type = 'n',
     cex = 0.9,
     xlab = 'Value', 
     ylab = 'Density',
     cex.axis = 1.3,
     cex.lab = 1.3)
lines(x,
      xx_1,
      lty = 1,
      col = 'steelblue',
      lwd = 2)
lines(x,
      xx_2,
      lty = 1,
      col = 'coral',
      lwd = 2)

# Plot IQR as a function of beta

a <- 1.5
g <- 1
d <- 0
Top <- 0.99
Bottom <- 0.1

IQR_1 <- list()

for(i in 0:200){
  print(i)
  b <- (i - 100)/100
  IQR_1[i+1] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
}

pdf('C:/Users/JulianW/Documents/Work/Productivity Dispersion/sim_graphs/beta_IQR.pdf', height = 6, width = 8)

plot(range(beta),
     range(IQR_1),
     type = 'n',
     cex = 0.9,
     xlab = 'Beta', 
     ylab = 'IQR',
     cex.axis = 1.3,
     cex.lab = 1.3)
lines(beta,
      IQR_1,
      lty = 1,
      col = 'steelblue',
      lwd = 2)

dev.off()

a <- 1.7
IQR_1 <- matrix(, nrow = 201, ncol = 49)

rownames(IQR_1) <- (seq(1:201) - 101)/100
colnames(IQR_1) <- paste((seq(1:49)), '%', sep = '')

for(j in 1:30){
  print(j)
  Top <- (100 - j) / 100
  Bottom <- j / 100
  
  for(i in 1:201){

    b <- (i - 101)/100
    IQR_1[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

a <- 1.5
IQR_2 <- matrix(, nrow = 201, ncol = 49)

rownames(IQR_2) <- (seq(1:201) - 101)/100
colnames(IQR_2) <- paste((seq(1:49)), '%', sep = '')

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
IQR_3 <- matrix(, nrow = 201, ncol = 49)

rownames(IQR_3) <- (seq(1:201) - 101)/100
colnames(IQR_3) <- paste((seq(1:49)), '%', sep = '')

for(j in 1:30){
  print(j)
  Top <- (100 - j) / 100
  Bottom <- j / 100
  
  for(i in 1:201){
    
    b <- (i - 101)/100
    IQR_3[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

a <- 1
IQR_4 <- matrix(, nrow = 201, ncol = 49)

rownames(IQR_4) <- (seq(1:201) - 101)/100
colnames(IQR_4) <- paste((seq(1:49)), '%', sep = '')

for(j in 1:30){
  print(j)
  Top <- (100 - j) / 100
  Bottom <- j / 100
  
  for(i in 1:201){
    
    b <- (i - 101)/100
    IQR_4[i, j] <- qstable(Top, a, b, g, d) - qstable(Bottom, a, b, g, d)
  }
}

f <- list(
  family = 'sans',
  size = 18,
  color = 'black'
)

x <- list(
  title = 'Percentile',
  titlefont = f,
  tickmode = 'array',
  tickvals = (c(1, 5, 10, 15, 25) -1), # subrtact 1 because plot starts at 0
  ticktext = colnames(IQR_1)[c(1, 5, 10, 15, 25)]
)

y <- list(
  title = 'beta',
  titlefont = f,
  tickvals = (c(1, 50, 100, 150, 201) -1), # subrtact 1 because plot starts at 0
  ticktext = rownames(IQR_1)[c(1, 51, 101, 151, 201)]
)

plot_ly(showscale = FALSE) %>%
  add_surface(z = ~IQR_1) %>%
  add_surface(z = ~IQR_2, opacity = 0.7) %>%
  add_surface(z = ~IQR_3, opacity = 0.7) %>%
  # add_surface(z = ~IQR_4, opacity = 0.7) %>% # The Cauchy does not work well - there were warnings from the calculations, but this is not entirely surprising
  layout(
    title = "Skew and IQRs",
    scene = list(
      xaxis = x,
      yaxis = y,
      zaxis = list(title = 'IQR')
    ))
