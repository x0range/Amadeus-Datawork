## This script is to compare the performance of ML and QT for levy estimation (See Appendix)

if (!"pacman" %in% installed.packages()[, "Package"]) install.packages("pacman", repos = "http://cran.r-project.org")
pacman::p_load(dplyr, StableEstim, lmomco, devtools, ggplot2, cowplot)

devtools::load_all("fittinglevy")

## Fake data

theta <- c(1.2,0.55, 1, 0) # true parameter
pm <- 0
num_sam <- seq(100, 5000, by = 500) # sampole size
num_sam_ext <- seq(7500, 20000, by = 2500) # sampole size extended

QT_list <- list(); ML_list <- list()

for(i in 1:length(num_sam)){
  set.seed(100)
  print(i)
  x <- rstable(num_sam[i], theta[1], theta[2], theta[3], theta[4], pm)
  QT_list[[i]] <- McCullochParametersEstim(x)
  ML_list[[i]] <- Estim(EstimMethod = "ML", data = x, ComputeCov = TRUE, theta0 = QT_list[[i]])

}

QT_list_ext <- list(); ML_list_ext <- list()

for(i in 1:length(num_sam_ext)){
  set.seed(100)
  print(i)
  x <- rstable(num_sam_ext[i], theta[1], theta[2], theta[3], theta[4], pm)
  QT_list_ext[[i]] <- McCullochParametersEstim(x)
  ML_list_ext[[i]] <- Estim(EstimMethod = "ML", data = x, ComputeCov = TRUE, theta0 = QT_list_ext[[i]])

}

save(theta, pm, num_sam, QT_list, ML_list, file = "QT_MLE.Rda")
save(num_sam_ext, QT_list_ext, ML_list_ext, file = "QT_MLE_ext.Rda")


###
load("QT_MLE.Rda")
load("QT_MLE_ext.Rda")

##
ML_list_all <- c(ML_list, ML_list_ext) # ML results
QT_list_all <- c(QT_list, QT_list_ext) # QT results
num_sam_all <- c(num_sam, num_sam_ext) # sample size


## Boostrap SD for QT
QT_SD_list_all <- list() 
for(i in 1:length(num_sam_all)){
    set.seed(100)
    print(i)
    x <- rstable(num_sam_all[i], theta[1], theta[2], theta[3], theta[4], pm)
    QT_SD_list_all[[i]] <-  levy_fitting(dat_t = x, bin_num = 100, include_bootstrap=TRUE)
}

qt_sd_fun <- function(x){
  apply(x$est_levy_std_error$t, 2, sd)
}

QT_SD_list_all <- lapply(QT_SD_list_all,  qt_sd_fun) 

## summarize
par_table <- list()

for(par_ind in 1:4){
  par_min <- unlist(lapply(ML_list_all, function(x) x@confint[par_ind ,1]))
  par_max <- unlist(lapply(ML_list_all, function(x) x@confint[par_ind ,2]))
  par_mean <- unlist(lapply(ML_list_all, function(x) x@par[par_ind ]))
  par_actual <- theta[par_ind ]
  par_QT <- unlist(lapply(QT_list_all, function(x) x[par_ind ]))
  par_QT_SD <- unlist(lapply(QT_SD_list_all, function(x) x[par_ind ]))
  
  par_table[[par_ind]] <- data.frame(par_min = par_min,
                            par_max = par_max,
                            par_mean = par_mean,
                            par_QT = par_QT,
                            par_QT_SD_max = par_QT + par_QT_SD * 1.96,
                            par_QT_SD_min = par_QT - par_QT_SD * 1.96,
                            par_actual = par_actual,
                            sam_size = num_sam_all)
}


# plot fuctions

plot_fun <- function(par_table_ind, y_ind, leg_ind){
  
  y_ind_all <- c(expression(alpha), expression(beta), expression(gamma), expression(delta))
  
  y_ind_pick <- y_ind_all[y_ind]
  
  gg_par <- ggplot(par_table_ind, aes(x = num_sam_all, y = par_actual)) +
    geom_ribbon(
      aes(ymin = par_min,
          ymax = par_max),
      alpha = 0.4,
      fill = 'green'
    ) +
    geom_ribbon(
      aes(ymin = par_QT_SD_min,
          ymax = par_QT_SD_max),
      alpha = 0.4,
      fill = 'steelblue'
    ) +
    geom_line(aes(y = par_actual, colour = "Actual"), size = .9) +
    geom_line(aes(y = par_mean, colour = "MLE"), size = .9) +
    geom_line(aes(y = par_QT, colour = "QT"), size = .9) +
    theme_bw() + 
    labs(
      x = 'Sample Size',
      y = y_ind_pick,
      colour = ''
    ) +
    theme(
      axis.text = element_text(size = rel(1.1)),
      axis.title = element_text(size = rel(1.1)),
      legend.background=element_blank(),
      legend.position =  c(0.87,0.87),
      legend.text = element_text(size = rel(1.1))
    )
  
  if(leg_ind == "F"){
    gg_par <- gg_par + theme( legend.position =  "none")
  }
  
  return(gg_par)
}

a <- plot_fun(par_table_ind = par_table[[1]], y_ind = 1, leg_ind = "T")
b <- plot_fun(par_table_ind = par_table[[2]], y_ind = 2, leg_ind = "F")
c <- plot_fun(par_table_ind = par_table[[3]], y_ind = 3, leg_ind = "F")
d <- plot_fun(par_table_ind = par_table[[4]], y_ind = 4, leg_ind = "F")
##
p_all <- plot_grid(a, b, c, d, align = "h")

ggsave(filename="MLE_QT.pdf", plot = p_all , width = 9, height = 6)

