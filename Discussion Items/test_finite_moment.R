rm(list = ls())
gc()
pacman::p_load(tidyverse,xtable,foreach)


# Sampled Absolute FLOM function:
# 1. dat: data vector for random variable
# 2. p: chosen moment
sample_Aflom <- function(dat,p){
  datp <- abs(dat)^p
  return(
    (1/length(datp)) * sum(datp)
  )
}

finite_moment_test <- function(data, moment, synthetic_sample_size = length(data)^(3/4), scaling_moment = 1, uniform_density_range = c(-1, 1)){
  
  if (moment <= scaling_moment) stop("'scaling_moment' must be less than 'moment'")
  
  # Step 1
  # normalise the variable first - equation (17) in Trapani (2016)
  norm_moment_1 <- sample_Aflom(dat = data, p = moment) / ((2^(moment)/sqrt(pi)) * gamma((moment+1)/2) * (1/sqrt(2))^(moment/2)) # the numerator is the AFLOM for the moment that is tested, the denominator is what the AFLOM would be if the data was standard normal (gamma = 1/sqrt(2))
  
  norm_moment_2 <- norm_moment_1 * (
    (2^(scaling_moment)/sqrt(pi)) * (gamma((scaling_moment+1)/2)) * (1/sqrt(2))^(scaling_moment/2) /  # the numerator here is the AFLOM for the scaling moment if the data was N(0,1)
      (sample_Aflom(dat = data, p = scaling_moment)) # denominator is the actual AFLOM from the data for the rescaling moment
  ) ^ (moment/scaling_moment) # exponent
  
  # Step 2
  # generate synthetic sample r of standard normal variables
  # find values evenly spread along the probability line
  # derive the values from CDF for these values
  r <- qnorm(c(
    seq(0.5, 0.99999999999, 1/synthetic_sample_size), 
    1 - seq(0.5, 0.99999999999, 1/synthetic_sample_size)
    ))
  
  phi_r <- exp(norm_moment_2/2) * r # square root is just 0.5 * power
  
  # Step 3-4
  # generate u uniform variable between -1 and 1
  u <- seq(uniform_density_range[1], uniform_density_range[2], (uniform_density_range[2] - uniform_density_range[1])/10000) # specify that we compute the phi for 100000 steps
  
  # equation (7) in Trapani (2016)
  phi_u <- lapply(u, function(x){
    (2/sqrt(length(phi_r))) * # first term of (7)
    (
      sum(phi_r <= x) - # Summation of indicator function is just checking how many phi_r satisfy inequality
      (length(phi_r)/2) # second term of summation function is just r/2
    )
    }
    ) 

  # Evaluate integral for test statistic
  phi_u_squared <- unlist(lapply(phi_u, function(x){x^2}))
  # sum the trapezoids, divide by the width of the uniform
  test_statistic <- 1/(uniform_density_range[2]-uniform_density_range[1]) * sum((phi_u_squared + lag(phi_u_squared))/2 * (abs(u - lag(u))), na.rm = T) 
  
  
  return(test_statistic)
}


# Load data and variables
load("Q:/Firm-Level Data/Amadeus/All_list_Cleaned_cut.Rda")
load("Q:/Firm-Level Data/Amadeus/Labels.Rda")


finite_second_moment_LP <- data.frame(matrix(vector(), 2, length(country_names_five) + 1, dimnames = list(c('Theta', 'P-Value'), c('Year', country_names_five))), stringsAsFactors = F)
finite_second_moment_LP_diff <- data.frame(matrix(vector(), 2, length(country_names_five) + 1, dimnames = list(c('Theta', 'P-Value'), c('Year', country_names_five))), stringsAsFactors = F)


# fill data frame by looping over samples

for(i in 1:length(country_names_five)){ # for each key country
  
  print(country_names_five[i])
  
  # LP
  
  # Find the test statistic
  finite_second_moment_LP[1, i] <- finite_moment_test(data = (All_list_Cleaned_cut[[country_ind]]$LP), moment = 2) 
  
  # given that the test statistic follow a Chi-squared with one degree of freedom under the null, we can compute the p-value
  finite_second_moment_LP[2, i] <- pchisq(finite_second_moment_LP[1, i], df = 1, lower.tail = FALSE) 
  
  # LP_diff
  
  # Find the test statistic
  finite_second_moment_LP_diff[1, i] <- finite_moment_test(data = (All_list_Cleaned_cut[[country_ind]]$LP_diff), moment = 2) 
  
  # given that the test statistic follow a Chi-squared with one degree of freedom under the null, we can compute the p-value
  finite_second_moment_LP_diff[2, i] <- pchisq(finite_second_moment_LP_diff[1, i], df = 1, lower.tail = FALSE) 
}


finite_second_moment_LP_years <- foreach(j = year_names, .combine = rbind) %do% {
  for(i in 1:length(country_names_five)){ # for each key country
    
    message(paste(country_names_five[i], j, sep = ' '))
    
    # the country's table number
    country_ind <- which(country_names %in% country_names_five[i])
    
    
    # find the observations for that year
    year_index <- which(All_list_Cleaned_cut[[country_ind]]$Year == j)
    finite_second_moment_LP$Year <- rep(j, times = 2)
    
    # LP
    
    # Find the test statistic
    finite_second_moment_LP[1, i+1] <- finite_moment_test(data = (All_list_Cleaned_cut[[country_ind]]$LP[year_index]), moment = 2) 
    
    # given that the test statistic follow a Chi-squared with one degree of freedom under the null, we can compute the p-value
    finite_second_moment_LP[2, i+1] <- pchisq(finite_second_moment_LP[1, i+1], df = 1, lower.tail = FALSE) 
  }
  
  finite_second_moment_LP
}

finite_second_moment_LP_diff_years <- foreach(j = year_names[-1], .combine = rbind) %do% {
  for(i in 1:length(country_names_five)){ # for each key country
    
    message(paste(country_names_five[i], j, sep = ' '))
    
    # the country's table number
    country_ind <- which(country_names %in% country_names_five[i])
    
    
    # find the observations for that year
    year_index <- which(All_list_Cleaned_cut[[country_ind]]$Year == j)
    finite_second_moment_LP_diff$Year <- rep(j, times = 2)
    
    # LP_diff
    
    # Find the test statistic
    finite_second_moment_LP_diff[1, i+1] <- finite_moment_test(data = (All_list_Cleaned_cut[[country_ind]]$LP_diff[year_index]), moment = 2) 
    
    # given that the test statistic follow a Chi-squared with one degree of freedom under the null, we can compute the p-value
    finite_second_moment_LP_diff[2, i+1] <- pchisq(finite_second_moment_LP_diff[1, i+1], df = 1, lower.tail = FALSE) 
  }
  
  finite_second_moment_LP_diff
}

print(xtable(finite_second_moment_LP_years, caption = 'Testing for Infinite Second Moment'), include.rownames = F)
