# Wrapper function for Levy-alpha stable estimation using GMM with GMMParametersEstim from StableEstim package

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

Levy_fun_GMM <- function(x, computeSE=F, theta0=NULL) {
  # Wrapper function
  #     Note that the function uses generic parameters for the fitting procedure. It can be extended to allow
  #     different parametrizations for the GMMParametersEstim. 
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf
  # Arguments:
  #     x: numeric, dataset to be fitted
  #     computeSE: boolean, should the standard errors be computed (from vcov matrix)
  #     theta0: StableEstim::Estim or NULL, starting values for parameter fit (initial guess)
  # Returns:
  #     list with two slots:
  #         "par": vector of the four parameters.
  #         "se": vector of the standard errors of the four parameters
  #     Note that the wrapper function discards the other return values of StableEstim:Estim.
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf

  # set generic parameters
  pm <- 0
  regularization = "cut-off"
  WeightingMatrix = "OptAsym"
  alphaReg = 0.005
  algo = "2SGMM"
  
  t_scheme = "free"
  t_seq = seq(0.1, 2, length.out = 12)
  
  # NA outputs in case the estimation fails
  parameters <- c(NA, NA, NA, NA)
  standard.errors <- c(NA, NA, NA, NA)
  # start fit
  try({
    GMM_1 <- Estim(EstimMethod = "GMM", data = x, ComputeCov = computeSE,
                               algo = algo, alphaReg = alphaReg,
                              regularization = regularization,
                              WeightingMatrix = WeightingMatrix,
                              t_scheme = t_scheme, theta0=theta0,
                              pm = pm, PrintTime = TRUE, t_free = t_seq)
    parameters <- slot(GMM_1,"par")
    standard.errors <- c(slot(GMM_1,"vcov")[1,1]**.5, slot(GMM_1,"vcov")[2,2]**.5, slot(GMM_1,"vcov")[3,3]**.5, slot(GMM_1,"vcov")[4,4]**.5)
  })

  # return parameters only
  return(list(par=parameters,se=standard.errors))
}
