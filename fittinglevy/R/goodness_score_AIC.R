# Function for providing Akaike information criterion as goodness of fit measure

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

levy_AIC <- function(para_levy, observations) {
  # Arguments:
  #     para_levy: numeric array of size 4, four estimated parameters of the Levy distribution
  #     observations: numeric, sample values
  # Returns:
  #     AIC of the Levy fit
    
  levy_aic <- 2*4 - 2 * sum(dstable(observations, para_levy[1], para_levy[2], para_levy[3], para_levy[4], log = T))

  return(levy_aic)
}
