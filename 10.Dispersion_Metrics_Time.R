## This script is to plot the time series of dispersiion metrics in Section 4 

## loading of required data and cleaning
load("All_list_Cleaned_cut.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("Labels.Rda")


load("Year_Levy_list_boot.Rda")
load("Year_non_neg_Levy_list_boot.Rda")
load("Year_Levy_list_boot_g.Rda")


load("Year_list_compare_AIC_SOOFI.Rda")
load("Year_list_compare_AIC_SOOFI_g.Rda")

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(colorspace,RColorBrewer, msir, scales, grDevices, dplyr)

## Functions
fun_agg_lp <- function(dat, num_cut, neg_cut, pov_cut, bottom_cut, top_cut, var_ind) {
  result_list <- list()
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  
  for (k in 1:length(dat)) {
    print(k)

    ok <- dat[[k]]
    
    # ok_all <- rbind(dat[[5]][,c("IDNR", "Year", "EMPL", "LP")], dat[[6]][,c("IDNR", "Year", "EMPL", "LP")], dat[[8]][,c("IDNR", "Year", "EMPL", "LP")], dat[[13]][,c("IDNR", "Year", "EMPL", "LP")])
    # zz <- ok_all %>%
    #   select(IDNR, Year, EMPL, LP) %>% 
    #   filter(EMPL > 1) %>% # remove self-employed firms
    #   #filter(LP >= 0)  %>% # Take only positive LP
    #   #mutate(VA = LP * EMPL) %>% # get value added
    #   filter(LP > quantile(LP, neg_cut) & LP < quantile(LP, pov_cut)) %>%
    #   mutate(Target_LP = LP)
    
    ## SIZE
    ### LP aggregate

    if(var_ind == "log"){
      zz <- ok %>%
        select(IDNR, Year, EMPL, LP) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
        filter(EMPL > 1) %>% # remove self-employed persons
        #mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
        filter(LP > 0) %>% # keep positive value added
        mutate(Target_LP = log(LP)) %>%
        #filter(Target_LP > quantile(Target_LP, neg_cut) & Target_LP < quantile(Target_LP, pov_cut)) %>% 
        #mutate(lag_neg = sign(lag(LP,1))) %>%
        #filter(lag_neg == 1) %>%
        arrange(IDNR)
    }else if(var_ind =="non_neg"){
      zz <- ok %>%
        select(IDNR, Year, EMPL, LP) %>% 
        filter(EMPL > 1) %>% # remove self-employed firms
        filter(LP > 0)  %>% # Take only positive LP
        #mutate(VA = LP * EMPL) %>% # get value added
        #filter(LP > quantile(LP, neg_cut) & LP < quantile(LP, pov_cut)) %>% 
        mutate(Target_LP = LP)
    } else{
      zz <- ok %>%
        select(IDNR, Year, EMPL, LP) %>% 
        filter(EMPL > 1) %>% # remove self-employed firms
        #filter(LP >= 0)  %>% # Take only positive LP
        #mutate(VA = LP * EMPL) %>% # get value added
        #filter(LP > quantile(LP, neg_cut) & LP < quantile(LP, pov_cut)) %>%
        mutate(Target_LP = LP)
    }
    
      LP_sd_n <- zz %>% #SD
        #select(IDNR, Year,Target_LP) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(
          sd = sd(Target_LP),
          n = n()
        )
      
      LP_perc <- zz %>% ## percentile ratio
        #select(IDNR, Year, Target_LP) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(iqr = quantile(Target_LP, top_cut)/quantile(Target_LP, bottom_cut)) # the sum of aggregate LP for bottom_cut% firms
      
      LP_iqr <- zz %>% # interquantile range
        #select(IDNR, Year,  Target_LP) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(iqr = quantile(Target_LP, top_cut) - quantile(Target_LP, bottom_cut)) # the sum of aggregate LP for bottom_cut% firms
      
      # 
      LP_l <- zz %>% #lower quantile 
        #select(IDNR, Year,  Target_LP) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(iqr = quantile(Target_LP, bottom_cut)) # the sum of aggregate LP for bottom_cut% firms


      LP_r <- zz %>% #upper quantile 
        #select(IDNR, Year,  Target_LP) %>%
        na.omit() %>%
        group_by(Year) %>%
        summarise(iqr = quantile(Target_LP, top_cut) ) # the sum of aggregate LP for bottom_cut% firms
     # as.data.frame(LP_l)
     # as.data.frame(LP_r)

      #save(LP_iqr, LP_l, LP_r, file = "Four_iqr.Rda")
      
       result_list[[k]] <- list(LP_sd_n = LP_sd_n, LP_perc = LP_perc, LP_iqr = LP_iqr, LP_l = LP_l, LP_r = LP_r)

   
    # LP_agg <- zz %>%
    #   select(IDNR, Year, NACE_CAT, VA, EMPL, LP) %>%
    #   na.omit() %>%
    #   group_by(Year) %>%
    #   summarise(
    #     LP_front = sum(VA[LP > quantile(LP, top_cut)]) / sum(EMPL[LP > quantile(LP, top_cut)]), # the sum of aggregate LP for top_cut% firms
    #     LP_laggard = sum(VA[LP < quantile(LP, bottom_cut)]) / sum(EMPL[LP < quantile(LP, bottom_cut)]),
    #     LP_diff_agg = LP_front - LP_laggard
    #   ) # the sum of aggregate LP for bottom_cut% firms

    ## I won't use the following lines of code for the final version of the plot. I keep this in case we might need them later.
  
}
  return(result_list)
}

## IQR for four countries


# ## the results with different cut-off points
lp_01 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.01, top_cut = 0.99, var_ind = 0)

 lp_05 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.05, top_cut = 0.95, var_ind = 0)
#
 lp_10 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.1, top_cut = 0.9, var_ind = 0)
#
 lp_10_uncut <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.00, pov_cut = 1, bottom_cut = 0.1, top_cut = 0.9, var_ind = 0)
 
lp_20 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.2, top_cut = 0.8, var_ind = 0)


# ###
# 
save(lp_01, lp_05, lp_10,  lp_10_uncut , lp_20, file = "lp_dispersion.Rda")

# ## the results with different cut-off points (non neg)
lp_non_neg_01 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.01, top_cut = 0.99, var_ind = "non_neg")

lp_non_neg_05 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.05, top_cut = 0.95, var_ind = "non_neg")
#
lp_non_neg_10 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.1, top_cut = 0.9, var_ind = "non_neg")
#
lp_non_neg_20 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.2, top_cut = 0.8, var_ind = "non_neg")

# ###
# ## the results with different cut-off points (log)
save(lp_non_neg_01, lp_non_neg_05, lp_non_neg_10, lp_non_neg_20, file = "lp_non_neg_dispersion.Rda")

## 
lp_log_01 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.01, top_cut = 0.99, var_ind = "log" )
#
 lp_log_05 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.05, top_cut = 0.95, var_ind = "log" )
# #
lp_log_10 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.1, top_cut = 0.9, var_ind = "log" )
# #
 lp_log_20 <- fun_agg_lp(dat = All_list_Cleaned_cut, neg_cut = 0.0025, pov_cut = 0.9975, bottom_cut = 0.2, top_cut = 0.8, var_ind = "log" )
# 
save(lp_log_01, lp_log_05, lp_log_10, lp_log_20, file = "lp_log_dispersion.Rda")
#load("lp_log_dispersion.Rda") 


## plot function 
dispersion_plot <- function(dat_list, iqr_data_1, iqr_data_2, iqr_data_3, leg_pos, title_n, aep_ind) { # this function has 5 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Prod_Fitting_Levy.R" script, 4) variable number: 1:Size, 2: Industry, 5) variable index: either "size" or "ind"
  

  colores_this <- brewer.pal(n = 7, name = "Set1")[-c(6)]
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries

  
  
  c_uni_list <- list()
  levy_par <- list()
  aep_par <- list()
  for (k in 1:length(dat_list[[3]])) {
    c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
    aep_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$sub_para)  # it returns location, scale, shape 1 (skew), and shape 2 (tail) parameters
  }
  
  par_a <- list()
  par_b <- list()
  par_g <- list()
  par_d <- list()

  par_aep_a <- list() # tail: 4th
  par_aep_b <- list() # skew: 3rd
  par_aep_g <- list() # scale: 2nd
  par_aep_d <- list() # location : 1st
  
  for (k in 1:length(dat_list[[3]])) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    
    par_aep_a[[k]] <- unlist(lapply(aep_par[[k]], function(x) x[4])) # parameter \alpha
    par_aep_b[[k]] <- unlist(lapply(aep_par[[k]], function(x) x[3])) # parameter \beta
    par_aep_g[[k]] <- unlist(lapply(aep_par[[k]], function(x) x[2])) # parameter \gamma
    par_aep_d[[k]] <- unlist(lapply(aep_par[[k]], function(x) x[1])) # parameter \delta
    
    
      }
  
  # very well matched
  # plot(unlist(par_a), unlist(par_aep_a))
  # plot(unlist(par_b), unlist(par_aep_b))
  # plot(unlist(par_g), unlist(par_aep_g))
  # plot(unlist(par_d), unlist(par_aep_d))
  
  iqr_1 <- lapply(iqr_data_1, function(x) x$LP_iqr[[2]]) # IQR 5% VS 95%
  iqr_2 <- lapply(iqr_data_2, function(x) x$LP_iqr[[2]])  # IQR 10% VS 90%
  iqr_3 <- lapply(iqr_data_3, function(x) x$LP_iqr[[2]])  # IQR 20% VS 80%
  
  year_list <- lapply(iqr_data_1, function(x) x$LP_iqr[[1]]) 
  n_length <- lapply(c_uni_list, function(x) length(x))

  for(k in 1:length( iqr_1)){
    take_this <- which(year_list[[k]]%in%c(2006:2015)[c_uni_list[[k]]])
    
    iqr_1[[k]] <- iqr_1[[k]][take_this]
    iqr_2[[k]] <- iqr_2[[k]][take_this]
    iqr_3[[k]] <- iqr_3[[k]][take_this]
    year_list[[k]] <- year_list[[k]][take_this]
    
    # par_a[[k]] <- par_a[[k]]/par_a[[k]][1]
    # par_g[[k]] <- par_g[[k]]/par_g[[k]][1]
    # iqr_1[[k]] <- iqr_1[[k]]/iqr_1[[k]][1]
    # iqr_2[[k]] <- iqr_2[[k]]/iqr_2[[k]][1]
    # iqr_3[[k]] <- iqr_3[[k]]/iqr_3[[k]][1]
    
    
  }
  
  
  all_frame <- data.frame(country = rep(country_names, unlist(n_length)), country_ind = rep(1:15, unlist(n_length)), year = unlist(year_list), alpha = unlist(par_a), beta = unlist(par_b), gamma = unlist(par_g), iqr_1 = unlist(iqr_1), iqr_2 = unlist(iqr_2), iqr_3 = unlist(iqr_3), alpha_aep = unlist(par_aep_a), beta_aep = unlist(par_aep_b), gamma_aep = unlist(par_aep_g)) 
  
  all_frame_five <-  all_frame[all_frame$country_ind%in%five_ind,]
  
  all_frame_five <- subset(  all_frame_five, year >= 2007)
  year_ind <- 2007:2015
  
  all_frame_five <-   all_frame_five %>%
    group_by(country) %>%
    mutate(alpha_norm  = alpha/alpha[1],
           beta_norm  = beta/beta[1],
           gamma_norm  = gamma/gamma[1],
           alpha_aep_norm  = alpha_aep/alpha_aep[1],
           beta_aep_norm  = beta_aep/beta_aep[1],
           gamma_aep_norm  = gamma_aep/gamma_aep[1],
           iqr_1_norm  = iqr_1/iqr_1[1],
           iqr_2_norm  = iqr_2/iqr_2[1],
           iqr_3_norm  = iqr_3/iqr_3[1])
  
  all_frame_five_sum <-   all_frame_five %>%
    group_by(year) %>%
    summarise(alpha_norm_mean = mean(alpha_norm),
              beta_norm_mean = mean(beta_norm),
              gamma_norm_mean = mean(gamma_norm),
              alpha_aep_norm_mean = mean(alpha_aep_norm),
              beta_aep_norm_mean = mean(beta_aep_norm),
              gamma_aep_norm_mean = mean(gamma_aep_norm),
              iqr_1_norm_mean = mean(iqr_1_norm),
              iqr_2_norm_mean = mean(iqr_2_norm),
              iqr_3_norm_mean = mean(iqr_3_norm),
              alpha_aep_norm_median = median(alpha_aep_norm),
              beta_aep_norm_median = median(beta_aep_norm),
              gamma_aep_norm_median = median(gamma_aep_norm),
              alpha_norm_median = median(alpha_aep_norm),
              beta_norm_median = median(beta_aep_norm),
              gamma_norm_median = median(gamma_aep_norm),
              iqr_1_norm_median = median(iqr_1_norm),
              iqr_2_norm_median = median(iqr_2_norm),
              iqr_3_norm_median = median(iqr_3_norm))
              
  # sd_v <- iqr_data_1[[five_ind[k]]][[1]]$sd_v # Standard deviation
  
  #iqr_year <- iqr_data_1[[five_ind[k]]][[1]]$Year # Year index
  #iqr_n <- iqr_data_1[[five_ind[k]]][[1]]$n # number of observations
  #sd_sam <- iqr_data_1[[five_ind[k]]][[1]]$sd
  
  #all_frame <- data.frame(alpha_v =  alpha_v, gamma_v = gamma_v, iqr_1 = iqr_1$iqr, iqr_2 = iqr_2$iqr, iqr_3 = iqr_3$iqr, sd_sam = sd_sam) # note that the alpha parameter is reparamatized to make a low alpha less disperse (in line with other dispersion metrics) * this is just for the visualization purpose.
  
  #all_frame <- data.frame(alpha_v = 1 / alpha_v, gamma_v = gamma_v, iqr_1 = iqr_1$LP_diff_agg, iqr_2 = iqr_2$LP_diff_agg, iqr_3 = iqr_3$LP_diff_agg, sd_sam = sd_sam) 
  
  #all_frame_norm <- apply(all_frame, 2, function(x) x / x[1]) # relative change with respects to the initial value
  
  ###
  
  pdf(paste("Dispersion_", title_n, ".pdf", sep = ""), height = 5, width = 6)
  par(mfrow = c(1, 1), mar = c(3, 2.5, 1., 1), mgp = c(1.6, .3, 0), tck = -.01, oma = c(0, 0, 1.2, 0))
  
  ##
  #y_min <- min(all_frame_five[,c("alpha", "gamma", "iqr_1", "iqr_2")] )
  #y_max <- max(all_frame_five[,c("alpha", "gamma", "iqr_1", "iqr_2")] )
  
  if(aep_ind == 0){
    y_min <-   min(all_frame_five_sum[, c("alpha_norm_mean", "gamma_norm_mean",  "iqr_2_norm_mean")])
    y_max <-   max(all_frame_five_sum[, c("alpha_norm_mean", "gamma_norm_mean",  "iqr_2_norm_mean")])
    
  }else{
    y_min <-   min(all_frame_five_sum[, c("alpha_aep_norm_mean", "gamma_aep_norm_mean",  "iqr_2_norm_mean")])
    y_max <-   max(all_frame_five_sum[, c("alpha_aep_norm_mean", "gamma_aep_norm_mean",  "iqr_2_norm_mean")])
    
  }
  
  plot(year_ind,year_ind, yaxt = "n", xaxt = "n", main = "", cex.main = 1.2, xlab = "Year", ylab = "Change relative to first year", cex.lab = 1., cex = 0, ylim = c(y_min, y_max)) # Empty plot for the first parameter: x_value is the numeric value of all classes
  
  abline(h = 1, lty = 2, lwd = 1)
  axis(side = 1, at = year_ind, label = year_ind, lwd = 0.3, cex.axis = 0.85) # year index for the x-axis
  axis(side = 2, lwd = 0.3, cex.axis = 0.85,  las = 1)
  
    #ok <- subset(all_frame_five, country_ind == c)
  if(aep_ind == 0){
    points(all_frame_five_sum$year, all_frame_five_sum$gamma_norm_mean, col = colores_this[1], cex = 0, type = "b", lty = 1, lwd = 2, pch = 16)
    text(all_frame_five_sum$year, all_frame_five_sum$gamma_norm_mean, expression(gamma),  col = colores_this[1], cex = 1.5)
    
    points(all_frame_five_sum$year, all_frame_five_sum$alpha_norm_mean, col = colores_this[2], cex = 0, type = "b", lty = 1, lwd = 1.3, pch = 16)
    text(all_frame_five_sum$year, all_frame_five_sum$alpha_norm_mean, expression(alpha),  col = colores_this[2], cex = 1.5)
  } else{
    points(all_frame_five_sum$year, all_frame_five_sum$gamma_aep_norm_mean, col = colores_this[1], cex = 0, type = "b", lty = 1, lwd = 2, pch = 16)
    text(all_frame_five_sum$year, all_frame_five_sum$gamma_aep_norm_mean, expression(sigma),  col = colores_this[1], cex = 1.5) # sigma instead of gamma
    
    points(all_frame_five_sum$year, all_frame_five_sum$alpha_aep_norm_mean, col = colores_this[2], cex = 0, type = "b", lty = 1, lwd = 1.3, pch = 16)
    text(all_frame_five_sum$year, all_frame_five_sum$alpha_aep_norm_mean, expression("h"),  col = colores_this[2], cex = 1.5) # h instead of alpha
    
  }
    points(all_frame_five_sum$year, all_frame_five_sum$iqr_2_norm_mean, col = colores_this[3], cex = 0, type = "b", lty = 1, lwd = 1.3, pch = 16)
    text(all_frame_five_sum$year,  all_frame_five_sum$iqr_2_norm_mean,  bquote(paste('I'['10'])), col = colores_this[3], cex = 1.5)
    
    #points(all_frame_five_sum$year, all_frame_five_sum$iqr_3_norm_mean, col = colores_this[3], cex = 0, type = "b", lty = 1, lwd = 1.3, pch = 16)
    #text(all_frame_five_sum$year,  all_frame_five_sum$iqr_3_norm_mean,  bquote(paste('I'['10'])), col = colores_this[3], cex = 1.5)
    
    if(aep_ind == 0){
  legend(leg_pos, legend = c("L\uE9vy scale", "L\uE9vy tail","iqr 90-10"), col = colores_this[c(1,2,3)], lty = rep(1, 3), bty = "n", cex = .9, ncol = 1)
    }else{
      legend(leg_pos, legend = c("AEP scale", "AEP tail","iqr 90-10"), col = colores_this[c(1,2,3)], lty = rep(1, 3), bty = "n", cex = .9, ncol = 1)
      
    }
    
  #mtext(paste(title_n), side = 3, line = 0, outer = TRUE, cex = 1.3)
  dev.off()
}

# 
# ## plot
# 
# ## Estimated Parameter for Year class
# # setwd("~/Desktop/Cleaned Rda/Productivity/Figures")
# 

# LP Levy para
dispersion_plot(dat_list =  LP_year_list_compare_AIC_SOOFI, iqr_data_1 = lp_05, iqr_data_2 = lp_10, iqr_data_3 = lp_20, leg_pos = "topleft", title_n = "LP_dispersion", aep_ind = 0)

# LP AEP para

dispersion_plot(dat_list =  LP_year_list_compare_AIC_SOOFI, iqr_data_1 = lp_05, iqr_data_2 = lp_10, iqr_data_3 = lp_20, leg_pos = "topright", title_n = "LP_dispersion_AEP", aep_ind = 1)

# uncutLP Levy para
dispersion_plot(dat_list = LP_year_list_compare_AIC_SOOFI_uncut, iqr_data_1 = lp_05, iqr_data_2 = lp_10_uncut, iqr_data_3 = lp_20, leg_pos = "topleft", title_n = "LP_dispersion_uncut", aep_ind = 0)

#dispersion_plot(dat_list = LP_year_non_neg_Levy_list_boot, iqr_data_1 = lp_non_neg_05, iqr_data_2 = lp_non_neg_10, iqr_data_3 = lp_non_neg_20,  leg_pos = "topleft", title_n = "LP_non_neg_dispersion", aep_ind = 0)

# Log LP Levy para
dispersion_plot(dat_list = LP_log_year_list_compare_AIC_SOOFI, iqr_data_1 = lp_log_05, iqr_data_2 = lp_log_10, iqr_data_3 = lp_log_20,  leg_pos = "topleft", title_n = "LP_log_dispersion", aep_ind = 0)

# Log LP AEP para
dispersion_plot(dat_list = LP_log_year_list_compare_AIC_SOOFI, iqr_data_1 = lp_log_05, iqr_data_2 = lp_log_10, iqr_data_3 = lp_log_20,  leg_pos = "topleft", title_n = "LP_log_dispersion_AEP", aep_ind = 1)




#### This is to show the impact of location change on the dispersion (See Appendix)
a <- rgamma(200000, shape = 3, rate = 1) + 1
b <- a - 1

quantile(a, 0.9) - quantile(a, 0.1) 

quantile(log(a), 0.9) - quantile(log(a), 0.1) 
quantile(log(b), 0.9) - quantile(log(b), 0.1) 


a_hist <- hist(a, breaks = 50)
b_hist <- hist(b, breaks = 100)

pdf(paste("Dispersion_example.pdf", sep = ""), height = 4, width = 9)
par(mfrow = c(1, 2), mar = c(3, 2.5, 1., 1), mgp = c(1.6, .3, 0), tck = -.01, oma = c(0, 0, 1.2, 0))


plot(density(b), type = "l", xlab = "x", ylab = "density",  bty = "n", yaxt = "n", xaxt = "n", main= "", cex.main=1,  cex.axis=0.8, lty = 2, lwd = 2)
axis(side = 1, lwd = 0.3, cex.axis=0.8)
axis(side = 2, lwd = 0.3, cex.axis=0.8)

lines(density(a), lty = 1, lwd = 2)

plot(density(log(a)), type = "l", xlab = "log(x)", ylab = "density",  bty = "n", yaxt = "n", xaxt = "n", main= "", cex.main=1,  cex.axis=0.8, lty = 1, lwd = 2, xlim = c(-1,3))
axis(side = 1, lwd = 0.3, cex.axis=0.8)
axis(side = 2, lwd = 0.3, cex.axis=0.8)

lines(density(log(b)), lty = 2, lwd = 2)

dev.off()


