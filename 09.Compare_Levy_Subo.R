# Script to perform cross-validation with Levy-alpha stable fits and AEP/Subbotin fits in comparison


############ 0. Basic Set up ############
## 0.1. loading of required libraries
if (!"pacman" %in% installed.packages()[, "Package"]) install.packages("pacman", repos = "http://cran.r-project.org")
pacman::p_load(dplyr, StableEstim, lmomco, devtools)

## 0.2 loading local packages
# functions from cross.validation package
devtools::load_all("crossvalidation")

load("All_list_Cleaned_cut.Rda")
load("Labels.Rda")
## 0.3. Setting of basic functions

# the fuction for the fitting result
fun_CV <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()

  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)

    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) %>% # percentage unit for the growth variables
      group_by(IDNR) %>% 
      mutate(Year_diff = Year - lag(Year,1)) %>%
      mutate(LP_diff = ifelse(Year_diff > 1, NA, LP_diff))
    
    zz <- as.data.frame(zz)

    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable


    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class
    zz_n <- zz %>%
      group_by(Cond) %>%
      summarise(n = n())

    if (nrow(zz_n) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class

      c_uni_name <- c()
      c_uni_num <- c()

      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }

      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]

      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(k, "-", country_names[[k]], ":", c, "out of", length(c_uni)))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        cv_levy <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "Levy") # Cross validation function
        cv_AEP <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "AEP") # Cross validation function
        c_list[[c]] <- list(cv_levy, cv_AEP) # Cross validation function
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}

fun_CV_g <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
    
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP,LP_diff, CP, EMPL, WS) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      group_by(IDNR) %>% 
      filter(LP > 0) %>% # keep positive value added
      filter(CP > 0) %>% # keep positive value added
      mutate(LP_g = (LP - lag(LP,1))/lag(LP,1),
             CP_g = (CP - lag(CP,1))/lag(CP,1),
             TFP_g = LP_g*lag(WS,1) + CP_g*(1-lag(WS,1)),
             log_LP = log(LP),
             log_CP = log(CP),
             log_TFP = WS*log_LP + (1-WS)*log_CP,
             LP_lg = log(LP/lag(LP,1)),
             CP_lg = log(CP/lag(CP,1)),
             TFP_lg = LP_lg*lag(WS,1) + CP_lg*(1-lag(WS,1))) %>%
      mutate(Year_diff = Year - lag(Year,1)) %>%
      mutate(LP_g = ifelse(Year_diff > 1, NA, LP_g),
             CP_g = ifelse(Year_diff > 1, NA, CP_g),
             TFP_g = ifelse(Year_diff > 1, NA, TFP_g),
             LP_lg = ifelse(Year_diff > 1, NA, LP_lg),
             CP_lg = ifelse(Year_diff > 1, NA, CP_lg),
             TFP_lg = ifelse(Year_diff > 1, NA, TFP_lg)
      ) 
    
    zz <- as.data.frame(zz)
    
    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable
    
    
    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class
    zz_n <- zz %>%
      group_by(Cond) %>%
      summarise(n = n())
    
    if (nrow(zz_n) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class
      
      c_uni_name <- c()
      c_uni_num <- c()
      
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(k, "-", country_names[[k]], ":", c, "out of", length(c_uni)))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        cv_levy <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "Levy") # Cross validation function
        cv_AEP <- CV_fun(n_fold = 10, n_rep = 10, uni_data = c_lp, distribution = "AEP") # Cross validation function
        c_list[[c]] <- list(cv_levy, cv_AEP) # Cross validation function
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}

# main entry point


##  0.4. loading of required data and cleaning          # TODO: Since this is duplicated in many scripts, this should sit in an external package
# 0.4.0 Load the data file created from "Productivity_Analysis_Data.Rmd"



############ 1. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


## Year class
# LP conditional on year (year class)
load("Year_list_compare.Rda")

LP_year_list_compare <- fun_CV(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_Change_year_list_compare <- fun_CV(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)


# setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_list_compare ,  LP_Change_year_list_compare,  file = "Year_list_compare.Rda")


# LP conditional on year (year class)
load("Year_list_compare_g.Rda")

LP_log_year_list_compare <- fun_CV_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "log_LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
# # LP_change conditional on year
LP_lg_year_list_compare <- fun_CV_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_lg", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
#  setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_log_year_list_compare ,  LP_lg_year_list_compare,  file = "Year_list_compare_g.Rda")

 
############ 2. Soofi and AIC ############

# Fittig function for the levy
fun_info_gen <- function(dat_t, bin_num) { # two arguments: 1) data and 2) the bin number
  p_data <- dat_t
  est_levy_qt <- Levy_fun_QT(p_data)
  est_sub_lm <- Sub_fun_LM(p_data)

  p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data), max(p_data), l = bin_num))

  obs_mid <- p_data_h$mids
  obs_p <- p_data_h$counts / sum(p_data_h$counts)

  pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4])

  pred_p_sub_b <- pdfaep4(obs_mid, est_sub_lm)

  
  pred_p_levy_b_all <- dstable(p_data, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4])
  
  pred_p_sub_b_all <- pdfaep4(p_data, est_sub_lm)
  

  # pred_p_sub_b <- dSEP1(obs_mid, mu =  est_sub_ml[1], sigma =  est_sub_ml[2], nu =  est_sub_ml[3], tau = est_sub_ml[4])


  # hist(rlmomco(100000, vec2par( est_sub_lm$para, type="aep4")))

  pred_p_levy <- pred_p_levy_b / sum(pred_p_levy_b)
  pred_p_sub <- pred_p_sub_b / sum(pred_p_sub_b)

  levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy)
  sub_soofi <- 1 - soofi_gen(obs_p, pred_p_sub)

  levy_aic <- 2 * 4 - 2 * sum(log(pred_p_levy_b_all))
  sub_aic <- 2 * 4 - 2 * sum(log(pred_p_sub_b_all))


  ok_list <- list(data_p = obs_p, levy_q = pred_p_levy, sub_q = pred_p_sub, levy_para = est_levy_qt, sub_info = est_sub_lm, sub_para = est_sub_lm[[2]], levy_soofi = round(levy_soofi, 4) * 100, sub_soofi = round(sub_soofi, 4) * 100, levy_aid = levy_aic, sub_aic = sub_aic)

  return(ok_list)
}


# the fuction for the fitting result
fun_AIC_SOOFI <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()

  c_uni_list <- list()
  c_uni_num_list <- list()
  num_obs <- list()
  for (k in 1:length(dat)) {
    print(k)

    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) %>%# percentage unit for the growth variables
      group_by(IDNR) %>% 
      mutate(Year_diff = Year - lag(Year,1)) %>%
      mutate(LP_diff = ifelse(Year_diff > 1, NA, LP_diff))
    
    zz <- as.data.frame(zz)

    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable


    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class


    if (nrow(zz) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class

      c_uni_name <- c()
      c_uni_num <- c()

      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }

      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]

      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        c_list[[c]] <- fun_info_gen(dat_t = c_lp, bin_num = bin_num) # Levy estimation
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
      num_obs[[k]] <- nrow(zz)
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list, num_obs)
  return(all_list)
}


fun_AIC_SOOFI_g <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  num_obs <- list()
  for (k in 1:length(dat)) {
    print(k)
    
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, CP, EMPL, WS) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      group_by(IDNR) %>% 
      filter(LP > 0) %>% # keep positive value added
      filter(CP > 0) %>% # keep positive value added
      mutate(LP_g = (LP - lag(LP,1))/lag(LP,1),
             CP_g = (CP - lag(CP,1))/lag(CP,1),
             TFP_g = LP_g*lag(WS,1) + CP_g*(1-lag(WS,1)),
             log_LP = log(LP),
             log_CP = log(CP),
             log_TFP = WS*log_LP + (1-WS)*log_CP,
             LP_lg = log(LP/lag(LP,1)),
             CP_lg = log(CP/lag(CP,1)),
             TFP_lg = LP_lg*lag(WS,1) + CP_lg*(1-lag(WS,1))) %>%
      mutate(Year_diff = Year - lag(Year,1)) %>%
      mutate(LP_g = ifelse(Year_diff > 1, NA, LP_g),
             CP_g = ifelse(Year_diff > 1, NA, CP_g),
             TFP_g = ifelse(Year_diff > 1, NA, TFP_g),
             LP_lg = ifelse(Year_diff > 1, NA, LP_lg),
             CP_lg = ifelse(Year_diff > 1, NA, CP_lg),
             TFP_lg = ifelse(Year_diff > 1, NA, TFP_lg)
      ) 
      
    zz <- as.data.frame(zz)
    
    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable
    
    
    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class
    
    
    if (nrow(zz) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class
      
      c_uni_name <- c()
      c_uni_num <- c()
      
      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }
      
      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        c_list[[c]] <- fun_info_gen(dat_t = c_lp, bin_num = bin_num) # Levy estimation
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
      num_obs[[k]] <- nrow(zz)
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list, num_obs)
  return(all_list)
}


fun_num_obs <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used as the conditional class, 4) the index for target variable, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()

  c_uni_list <- list()
  c_uni_num_list <- list()
  num_obs <- list()
  for (k in 1:length(dat)) {
    print(k)

    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Growth, TFP Growth, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) %>%
      group_by(IDNR) %>% 
      mutate(Year_diff = Year - lag(Year,1)) %>%
      mutate(LP_diff = ifelse(Year_diff > 1, NA, LP_diff))
    
    zz <- as.data.frame(zz)

    zz$Cond <- zz[, cond_ind] # create a new column of the class variable
    zz$Var <- zz[, var_ind] # create a new column of the value variable


    zz <- zz %>%
      select(IDNR, Year, Var, Cond) %>%
      na.omit() %>%
      filter(Var > quantile(Var, neg_cut) & Var < quantile(Var, pov_cut)) %>% # cut the tail
      group_by(Cond) %>%
      filter(length(IDNR) > cut_num) # set the minimum number of obs for each class


    if (nrow(zz) == 0) {
      result_list[[k]] <- NA
    } else {
      c_uni <- unique(zz$Cond) # unique class

      c_uni_name <- c()
      c_uni_num <- c()

      for (i in 1:length(c_uni)) {
        c_uni_num[i] <- which(c_names %in% c_uni[i])
      }

      c_uni_num <- sort(c_uni_num)
      c_uni_name <- c_names[c_uni_num]

      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
      
        c_list[[c]] <- length(c_lp) # length of variable 
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
      num_obs[[k]] <- nrow(zz)
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list, num_obs)
  return(all_list)
}

LP_num_obs <- fun_num_obs(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_Change_num_obs <- fun_num_obs(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_num_obs, LP_Change_num_obs, file = "num_obs.Rda")

load("num_obs.Rda")

# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## Year class
# LP conditional on year (year class)


load("Year_list_compare_AIC_SOOFI.Rda")

LP_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_year_list_compare_AIC_SOOFI_uncut <- fun_AIC_SOOFI(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = 0, pov_cut = 1)
# LP_change conditional on year

LP_Change_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

#setwd("~/Desktop/Cleaned Rda/Productivity")
save(LP_year_list_compare_AIC_SOOFI ,LP_year_list_compare_AIC_SOOFI_uncut , LP_Change_year_list_compare_AIC_SOOFI , file = "Year_list_compare_AIC_SOOFI.Rda")



load("Year_list_compare_AIC_SOOFI_g.Rda")

LP_log_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "log_LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
LP_lg_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_lg", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
save(LP_log_year_list_compare_AIC_SOOFI  , LP_lg_year_list_compare_AIC_SOOFI , file = "Year_list_compare_AIC_SOOFI_g.Rda")


load("Year_list_compare_AIC_SOOFI_g_TFP.Rda")
##
TFP_log_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "log_TFP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
TFP_lg_year_list_compare_AIC_SOOFI <- fun_AIC_SOOFI_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "TFP_lg", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)
# 
save(TFP_log_year_list_compare_AIC_SOOFI  , TFP_lg_year_list_compare_AIC_SOOFI , file = "Year_list_compare_AIC_SOOFI_g_TFP.Rda")


## to get the average sample size 
# total obs per country
lp_length <- unlist(LP_year_list_compare_AIC_SOOFI[[4]])
lp_change_length <- unlist(LP_Change_year_list_compare_AIC_SOOFI[[4]])

# the third country in the lp_change sample needs to be NA
lp_change_length[4:15] <- lp_change_length[3:14]
lp_change_length[3] <- NA

# number of years for each country
lp_year <- unlist(lapply(LP_year_list_compare_AIC_SOOFI[[1]], function(x) length(x)))
lp_change_year <- unlist(lapply(LP_Change_year_list_compare_AIC_SOOFI[[1]], function(x) length(x)))

#the average obs per country-year
lp_length  <- lp_length/lp_year
lp_change_length <- lp_change_length/lp_change_year

############ 2. Summary table ############
#### averaged over country

# for tex table
fun_m_sd <- function(x,y){
  string <- round(x,0)
  zz <- paste0(round(x,0), " (",round(y,2), ")")
  return(zz)
}


arr_fun <- function(dat_1, dat_2, lp_ind) { # dat_1 is the CV result and dat_2 is the Soofi & AIC result

  f_frame <- list()
  for (k in 1:15) {
    levy_cv <- mean(unlist(lapply(dat_1[[1]][[k]], function(x) mean(unlist(x[[1]])))))

    if (is.na(levy_cv)) { # this means that there is no element in the list
      f_frame[[k]] <- NA
    } else {
      sub_cv <- mean(unlist(lapply(dat_1[[1]][[k]], function(x) mean(unlist(x[[2]])))))

      levy_soofi <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$levy_soofi)))
      sub_soofi <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$sub_soofi)))

      levy_aic <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$levy_aid)))
      sub_aic <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$sub_aic)))

      # diff
      soofi_diff <- levy_soofi - sub_soofi
      aic_diff <- levy_aic - sub_aic
      cv_diff <- levy_cv - sub_cv
      
      if(lp_ind == "LP"){
        f_frame[[k]] <- data.frame(
          Empty = "", 
          Country = country_names[[k]], 
          levy_cv = levy_cv,
          levy_cv_norm = levy_cv/lp_length[k], 
          sub_cv = sub_cv,
          sub_cv_norm = sub_cv/lp_length[k], 
          rel_lik = exp(-cv_diff/c(lp_length[k])), 
          levy_soofi = levy_soofi, 
          sub_soofi = sub_soofi, 
          soofi_diff = soofi_diff, 
          levy_aic = levy_aic, 
          levy_aic_norm = levy_aic/lp_length[k], 
          sub_aic = sub_aic, 
          sub_aic_norm = sub_aic/lp_length[k], 
          rel_lik = exp(aic_diff/c(lp_length[k]*2)))
      }else{
        f_frame[[k]] <- data.frame(
          Empty = "", 
          Country = country_names[[k]], 
          levy_cv = levy_cv,
          levy_cv_norm = levy_cv/lp_change_length[k], 
          sub_cv = sub_cv,
          sub_cv_norm = sub_cv/lp_change_length[k], 
          rel_lik = exp(-cv_diff/c(lp_change_length[k])), 
          levy_soofi = levy_soofi, 
          sub_soofi = sub_soofi, 
          soofi_diff = soofi_diff, 
          levy_aic = levy_aic, 
          levy_aic_norm = levy_aic/lp_change_length[k], 
          sub_aic = sub_aic, 
          sub_aic_norm = sub_aic/lp_change_length[k], 
          rel_lik = exp(aic_diff/c(lp_change_length[k]*2)))
      }
     
    }
  }
  
  return(f_frame)
}

### make a dataframe for a latex table


LP_year_comp <- arr_fun(
  dat_1 = LP_year_list_compare,
  dat_2 = LP_year_list_compare_AIC_SOOFI,
  lp_ind = "LP"
)
LP_year_comp <- do.call("rbind", LP_year_comp)

LP_year_comp[16,] <- c(NA, NA, apply(LP_year_comp[,-c(1,2)], 2, mean))

LP_year_comp$levy_cv <- fun_m_sd(LP_year_comp$levy_cv, LP_year_comp$levy_cv_norm)
LP_year_comp$sub_cv <- fun_m_sd(LP_year_comp$sub_cv, LP_year_comp$sub_cv_norm)

LP_year_comp$levy_aic <- fun_m_sd(LP_year_comp$levy_aic, LP_year_comp$levy_aic_norm)
LP_year_comp$sub_aic <- fun_m_sd(LP_year_comp$sub_aic, LP_year_comp$sub_aic_norm)


LP_Change_year_comp <- arr_fun(
  dat_1 = LP_Change_year_list_compare,
  dat_2 = LP_Change_year_list_compare_AIC_SOOFI,
  lp_ind = "LP_Change"
)

LP_Change_year_comp[[3]] <- data.frame(Empty = "", Country = country_names[[3]], levy_cv = NA, levy_cv_norm = NA, sub_cv = NA, sub_cv_norm = NA, rel_lik = NA, levy_soofi = NA, sub_soofi = NA, soofi_diff = NA, levy_aic = NA, levy_aic_norm = NA, sub_aic = NA, sub_aic_norm = NA, rel_lik = NA) # NA for the empty row

LP_Change_year_comp <- do.call("rbind", LP_Change_year_comp)

LP_Change_year_comp[16,] <-  c(NA, NA, apply(LP_Change_year_comp[-c(3),-c(1,2)], 2, mean))

LP_Change_year_comp$levy_cv <- fun_m_sd(LP_Change_year_comp$levy_cv, LP_Change_year_comp$levy_cv_norm)
LP_Change_year_comp$sub_cv <- fun_m_sd(LP_Change_year_comp$sub_cv, LP_Change_year_comp$sub_cv_norm)

LP_Change_year_comp$levy_aic <- fun_m_sd(LP_Change_year_comp$levy_aic, LP_Change_year_comp$levy_aic_norm)
LP_Change_year_comp$sub_aic <- fun_m_sd(LP_Change_year_comp$sub_aic, LP_Change_year_comp$sub_aic_norm)

##
library(xtable)
print(xtable(LP_year_comp[,-c(4,6,12,14)], digits = c(0, 0, 0, 0, 0, 2, 1, 1, 1, 0, 0, 2)), include.rownames = FALSE)
print(xtable(LP_Change_year_comp[,-c(4,6,12,14)], digits = c(0, 0, 0, 0, 0, 2, 1, 1, 1, 0, 0, 2)), include.rownames = FALSE)


### for log variables 
## to get the average sample size 
# total obs per country
lp_length <- unlist(LP_log_year_list_compare_AIC_SOOFI[[4]])
lp_change_length <- unlist(LP_lg_year_list_compare_AIC_SOOFI[[4]])

# the third country in the lp_change sample needs to be NA
lp_change_length[4:15] <- lp_change_length[3:14]
lp_change_length[3] <- NA

# number of years for each country
lp_year <- unlist(lapply(LP_log_year_list_compare_AIC_SOOFI[[1]], function(x) length(x)))
lp_change_year <- unlist(lapply(LP_lg_year_list_compare_AIC_SOOFI[[1]], function(x) length(x)))

#the average obs per country-year
lp_length  <- lp_length/lp_year
lp_change_length <- lp_change_length/lp_change_year

############ 2. Summary table ############
#### averaged over country

# for tex table
fun_m_sd <- function(x,y){
  string <- round(x,0)
  zz <- paste0(round(x,0), " (",round(y,2), ")")
  return(zz)
}


arr_fun <- function(dat_1, dat_2, lp_ind) { # dat_1 is the CV result and dat_2 is the Soofi & AIC result
  
  f_frame <- list()
  for (k in 1:15) {
    levy_cv <- mean(unlist(lapply(dat_1[[1]][[k]], function(x) mean(unlist(x[[1]])))))
    
    if (is.na(levy_cv)) { # this means that there is no element in the list
      f_frame[[k]] <- NA
    } else {
      sub_cv <- mean(unlist(lapply(dat_1[[1]][[k]], function(x) mean(unlist(x[[2]])))))
      
      levy_soofi <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$levy_soofi)))
      sub_soofi <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$sub_soofi)))
      
      levy_aic <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$levy_aid)))
      sub_aic <- mean(unlist(lapply(dat_2[[1]][[k]], function(x) x$sub_aic)))
      
      # diff
      soofi_diff <- levy_soofi - sub_soofi
      aic_diff <- levy_aic - sub_aic
      cv_diff <- levy_cv - sub_cv
      
      if(lp_ind == "LP"){
        f_frame[[k]] <- data.frame(
          Empty = "", 
          Country = country_names[[k]], 
          levy_cv = levy_cv,
          levy_cv_norm = levy_cv/lp_length[k], 
          sub_cv = sub_cv,
          sub_cv_norm = sub_cv/lp_length[k], 
          rel_lik = exp(-cv_diff/c(lp_length[k])), 
          levy_soofi = levy_soofi, 
          sub_soofi = sub_soofi, 
          soofi_diff = soofi_diff, 
          levy_aic = levy_aic, 
          levy_aic_norm = levy_aic/lp_length[k], 
          sub_aic = sub_aic, 
          sub_aic_norm = sub_aic/lp_length[k], 
          rel_lik = exp(aic_diff/c(lp_length[k]*2)))
      }else{
        f_frame[[k]] <- data.frame(
          Empty = "", 
          Country = country_names[[k]], 
          levy_cv = levy_cv,
          levy_cv_norm = levy_cv/lp_change_length[k], 
          sub_cv = sub_cv,
          sub_cv_norm = sub_cv/lp_change_length[k], 
          rel_lik = exp(-cv_diff/c(lp_change_length[k])), 
          levy_soofi = levy_soofi, 
          sub_soofi = sub_soofi, 
          soofi_diff = soofi_diff, 
          levy_aic = levy_aic, 
          levy_aic_norm = levy_aic/lp_change_length[k], 
          sub_aic = sub_aic, 
          sub_aic_norm = sub_aic/lp_change_length[k], 
          rel_lik = exp(aic_diff/c(lp_change_length[k]*2)))
      }
      
    }
  }
  
  return(f_frame)
}

### make a dataframe for a latex table


LP_year_comp <- arr_fun(
  dat_1 = LP_log_year_list_compare,
  dat_2 = LP_log_year_list_compare_AIC_SOOFI,
  lp_ind = "LP"
)
LP_year_comp <- do.call("rbind", LP_year_comp)

LP_year_comp[16,] <- c(NA, NA, apply(LP_year_comp[,-c(1,2)], 2, mean))

LP_year_comp$levy_cv <- fun_m_sd(LP_year_comp$levy_cv, LP_year_comp$levy_cv_norm)
LP_year_comp$sub_cv <- fun_m_sd(LP_year_comp$sub_cv, LP_year_comp$sub_cv_norm)

LP_year_comp$levy_aic <- fun_m_sd(LP_year_comp$levy_aic, LP_year_comp$levy_aic_norm)
LP_year_comp$sub_aic <- fun_m_sd(LP_year_comp$sub_aic, LP_year_comp$sub_aic_norm)


LP_Change_year_comp <- arr_fun(
  dat_1 = LP_lg_year_list_compare,
  dat_2 = LP_lg_year_list_compare_AIC_SOOFI,
  lp_ind = "LP_Change"
)

LP_Change_year_comp[[3]] <- data.frame(Empty = "", Country = country_names[[3]], levy_cv = NA, levy_cv_norm = NA, sub_cv = NA, sub_cv_norm = NA, rel_lik = NA, levy_soofi = NA, sub_soofi = NA, soofi_diff = NA, levy_aic = NA, levy_aic_norm = NA, sub_aic = NA, sub_aic_norm = NA, rel_lik = NA) # NA for the empty row

LP_Change_year_comp <- do.call("rbind", LP_Change_year_comp)

LP_Change_year_comp[16,] <-  c(NA, NA, apply(LP_Change_year_comp[-c(3),-c(1,2)], 2, mean))

LP_Change_year_comp$levy_cv <- fun_m_sd(LP_Change_year_comp$levy_cv, LP_Change_year_comp$levy_cv_norm)
LP_Change_year_comp$sub_cv <- fun_m_sd(LP_Change_year_comp$sub_cv, LP_Change_year_comp$sub_cv_norm)

LP_Change_year_comp$levy_aic <- fun_m_sd(LP_Change_year_comp$levy_aic, LP_Change_year_comp$levy_aic_norm)
LP_Change_year_comp$sub_aic <- fun_m_sd(LP_Change_year_comp$sub_aic, LP_Change_year_comp$sub_aic_norm)

##
library(xtable)
print(xtable(LP_year_comp[,-c(4,6,12,14)], digits = c(0, 0, 0, 0, 0, 2, 1, 1, 1, 0, 0, 2)), include.rownames = FALSE)
print(xtable(LP_Change_year_comp[,-c(4,6,12,14)], digits = c(0, 0, 0, 0, 0, 2, 1, 1, 1, 0, 0, 2)), include.rownames = FALSE)



#### for uncut (only look at the AIC) *Soofi doesn't make much sense

aic_levy <- c()
aic_aep <- c()
aic_levy_uncut <- c()
aic_aep_uncut <- c()
for(k in 1:15){
  aic_levy_uncut[k] <- mean(unlist(lapply(LP_year_list_compare_AIC_SOOFI_cut[[1]][[k]], function(x) x$levy_aid)))
  aic_aep_uncut[k] <- mean(unlist(lapply(LP_year_list_compare_AIC_SOOFI_cut[[1]][[k]], function(x) x$sub_aic)))
  
  aic_levy[k] <- mean(unlist(lapply(LP_year_list_compare_AIC_SOOFI[[1]][[k]], function(x) x$levy_aid)))
  aic_aep[k] <- mean(unlist(lapply(LP_year_list_compare_AIC_SOOFI[[1]][[k]], function(x) x$sub_aic)))
}

aic_levy_uncut
aic_aep_uncut

aic_levy
aic_aep
