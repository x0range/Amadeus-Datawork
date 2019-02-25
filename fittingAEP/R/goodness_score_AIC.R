# Function for providing Akaike information criterion as goodness of fit measure

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(lmomco)

AEP_AIC <- function(fit_AEP, obs_mid) {
  # Arguments:
  #     fit_AEP: list containing fit parameters as returned by lmomco's paraep4(); also this package's Sub_fit_LM: AEP fit
  #     obs_mid: numeric array: mid value of bins to be evaluated
  # Returns:
  #     AIC of the AEP fit

  aep_aic <- 2*4 - 2 * sum(log(pdfaep4(obs_mid, fit_AEP)))

  return(aep_aic)
}
