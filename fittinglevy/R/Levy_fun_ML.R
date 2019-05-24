# Wrapper function for Levy-alpha stable estimation using Maximum Likelihood with MLParametersEstim from StableEstim package. This will take a long time.

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

Levy_fun_ML <- function(x, computeSE=F, theta0=NULL) {
  # Wrapper function
  #     Note that the function uses generic parameters for the fitting procedure. It can be extended to allow
  #     different parametrizations for the MLParametersEstim. 
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf
  # Arguments:
  #     x: numeric, dataset to be fitted
  #     computeSE: boolean, should the standard errors be computed (from vcov matrix)
  #     theta0: StableEstim::Estim or NULL, starting values for parameter fit (initial guess)
  # Returns:
  #     list with two slots:
  #         "par": vector of the four parameters.
  #         "se": vector of the standard errors of the four parameters
  #     Note that the wrapper function discards the other return values of MLParametersEstim.
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf

  # set generic parameters
  pm <- 0
  
  # start fit
  ML_1 <- Estim(data = x, EstimMethod="ML", pm = pm, theta0=theta0, PrintTime = TRUE, ComputeCov = computeSE)
  
  parameters <- slot(ML_1,"par")
  standard.errors <- c(slot(ML_1,"vcov")[1,1]**.5, slot(ML_1,"vcov")[2,2]**.5, slot(ML_1,"vcov")[3,3]**.5, slot(ML_1,"vcov")[4,4]**.5)

  # return parameters only
  return(list(par=parameters,se=standard.errors))
}
