# Function to do Levy alpha stable parameter error estimates using bootstraps.

############ 0. Basic Set up ############
## 0.1. loading of required libraries

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim,devtools)

## 0.2 loading fitting functions from fitting.levy package
devtools::load_all("fittinglevy")

load("All_list_Cleaned_cut.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("Labels.Rda")

ind_name_table <- ind_name_table %>% 
  mutate(ind_agg = ifelse(ind_names_short == "Agr" | ind_names_short == "Mine", "Arg",
                          ifelse(ind_names_short == "Elec" | ind_names_short == "Water", "Energy", 
                                 ifelse(ind_names_short == "Manu", "Manu",
                                        ifelse(ind_names_short == "Cons", "Cons", 
                                               ifelse(ind_names_short == "Info", "Info", 
                                                      ifelse(ind_names_short == "Real" | ind_names_short == "F&I"| ind_names_short == "Prof-S",  "FIRE", 
                                                             ifelse(ind_names_short == "Edu"| ind_names_short == "Heath"| ind_names_short == "Public",  "NM_Ser", 
                                                                    ifelse(ind_names_short == "Whole" | ind_names_short == "Trans"| ind_names_short == "Accom"| ind_names_short == "Art"| ind_names_short == "Admin" | ind_names_short == "O-Serv", "NF_Ser", 
                                                                           NA)))))))))



for(k in 1:15){
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned_cut[[k]] %>%
    mutate(ind_agg = ifelse(NACE_CAT == "AGRICULTURE, FORESTRY AND FISHING" | NACE_CAT == "MINING AND QUARRYING", "Arg",
                            ifelse(NACE_CAT == "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY" | NACE_CAT == "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES", "Energy", 
                                   ifelse(NACE_CAT == "MANUFACTURING", "Manu",
                                          ifelse(NACE_CAT == "CONSTRUCTION", "Cons", 
                                                 ifelse(NACE_CAT == "INFORMATION AND COMMUNICATION", "Info", 
                                                        ifelse(NACE_CAT == "REAL ESTATE ACTIVITIES" | NACE_CAT == "FINANCIAL AND INSURANCE ACTIVITIES" | NACE_CAT == "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",  "FIRE", 
                                                               ifelse(NACE_CAT == "EDUCATION"| NACE_CAT == "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES"| NACE_CAT == "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY",  "NM_Ser", 
                                                                      ifelse(NACE_CAT == "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES" | NACE_CAT == "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES" | NACE_CAT == "TRANSPORTATION AND STORAGE"| NACE_CAT == "ACCOMMODATION AND FOOD SERVICE ACTIVITIES"| NACE_CAT == "ARTS, ENTERTAINMENT AND RECREATION"| NACE_CAT == "OTHER SERVICE ACTIVITIES", "NF_Ser", 
                                                                             NA)))))))))
  
}

## 0.3 remaining function definitions: 
# the fuction for the fitting result
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used for the conditional class, 4) the target variable name, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
  
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, ind_agg, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      mutate(LP_diff = LP_diff / 1000) # percentage unit for the growth variables
    
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
      c_uni_name <- c_names[c_uni_num] # record the class names
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class

        levy_result <- levy_fitting(dat_t = c_lp, bin_num = bin_num, include_bootstrap=TRUE) # Levy estimation
        levy_result$levy_para
            
        c_list[[c]] <- list(levy_para = levy_result$levy_para, levy_soofi = levy_result$levy_soofi, est_levy_std_error = levy_result$est_levy_std_error, data_mid = levy_result$data_mid , data_p = levy_result$data_p, levy_q = levy_result$levy_q)
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      #c_uni_list_2[[k]] <- c_uni_name_2 #
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}


# main entry point

##  1.1. loading of required data and cleaning


############ 2. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point


## Year class
# LP conditional on year (year class)

#load("Year_Levy_list_boot.Rda")

# diff_est <- list()
# for(k in 1:15){
#   no_cut <- do.call("rbind", lapply(LP_year_Levy_list_boot_no_cut[[1]][[k]], function(x) x$levy_para))
#   cut <- do.call("rbind", lapply(LP_year_Levy_list_boot[[1]][[k]], function(x) x$levy_para))
# 
#   diff_est[[k]] <- no_cut - cut
# }

LP_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_year_Levy_list_boot_uncut <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = 0, pov_cut = 1)


# LP_change conditional on year
LP_Change_year_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_year_Levy_list_boot,LP_year_Levy_list_boot_uncut, LP_Change_year_Levy_list_boot, file = "Year_Levy_list_boot.Rda")

## Size class
# LP conditional on size
LP_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on size
LP_Change_size_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_diff", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_size_Levy_list_boot, LP_Change_size_Levy_list_boot, file = "Size_Levy_list_boot.Rda")

## Industry class



# LP conditional on sector
LP_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
LP_Change_ind_Levy_list_boot <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_diff", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_Levy_list_boot, LP_Change_ind_Levy_list_boot, file = "Industry_Levy_list_boot.Rda")

# LP conditional on sector
LP_ind_Levy_list_boot_sim <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP", c_names = unique(ind_name_table$ind_agg),  cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
LP_Change_ind_Levy_list_boot_sim <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP_diff", c_names = unique(ind_name_table$ind_agg),  cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_Levy_list_boot_sim, LP_Change_ind_Levy_list_boot_sim, file = "Industry_Levy_list_boot_sim.Rda")


#### LP_g and TFP_g (removing negative value added)
fun_fit_levy_g<- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used for the conditional class, 4) the target variable name, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
    
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, ind_agg, LP,LP_diff, CP, EMPL, WS) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
      filter(EMPL > 1) %>% # remove self-employed persons
      mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
      group_by(IDNR) %>% 
      filter(LP > 0) %>% # keep positive value added
      mutate(LP_g = (LP - lag(LP,1))/lag(LP,1),
             log_LP = log(LP),
             LP_lg = log(LP/lag(LP,1)),
             CP_g = (CP - lag(CP,1))/lag(CP,1),
             TFP_g = LP_g*WS + CP_g*(1-WS),
             LP_diff = LP_diff / 1000) %>%
      #mutate(lag_neg = sign(lag(LP,1))) %>%
      #filter(lag_neg == 1) %>%
      arrange(IDNR)
      
    # Spain_lp_g <-  All_list_Cleaned_cut[[k]]%>%
    #   select(IDNR, Year, EMPL, LP) %>%
    #   na.omit() %>%
    #   filter(EMPL > 1) %>% # remove self-employed persons
    #   group_by(IDNR) %>%
    #   mutate(LP_g = (LP - lag(LP,1))/lag(LP,1)) %>%
    #   mutate(lag_neg = sign(lag(LP,1))) %>%
    #   mutate(y_lag = (Year - lag(Year,1))) %>%
    #   filter(y_lag < 2) %>%
    #   filter(lag_neg == 1) %>%
    #   arrange(IDNR)
    # 
    
    
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
      c_uni_name <- c_names[c_uni_num] # record the class names
      
      c_list <- list()
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        
        levy_result <- levy_fitting(dat_t = c_lp, bin_num = bin_num, include_bootstrap=TRUE) # Levy estimation
        
        
        c_list[[c]] <- list(levy_para = levy_result$levy_para, levy_soofi = levy_result$levy_soofi, est_levy_std_error = levy_result$est_levy_std_error, data_mid = levy_result$data_mid , data_p = levy_result$data_p, levy_q = levy_result$levy_q)
      }
      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      #c_uni_list_2[[k]] <- c_uni_name_2 #
      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(all_list)
}



## Year class
# LP conditional on year *non-neg
LP_year_non_neg_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_year_log_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "log_LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year *non-neg
LP_Change_non_neg_year_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_year_non_neg_Levy_list_boot, LP_year_log_Levy_list_boot , LP_Change_non_neg_year_Levy_list_boot, file = "Year_non_neg_Levy_list_boot.Rda")


# LP_g, LP_lg, TFP_g  conditional on year 
LP_g_year_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_g", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_lg_year_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_lg", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

TFP_g_year_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "TFP_g", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_g_year_Levy_list_boot, LP_lg_year_Levy_list_boot, TFP_g_year_Levy_list_boot, file = "Year_Levy_list_boot_g.Rda")

## Size class
# LP conditional on size *non-neg
LP_size_non_neg_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP", c_names = size_names_long, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_size_log_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "log_LP", c_names = size_names_long, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)


# LP_change conditional on size *non-neg
LP_Change_non_neg_size_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_diff", c_names = size_names_long, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_size_non_neg_Levy_list_boot, LP_size_log_Levy_list_boot, LP_Change_non_neg_size_Levy_list_boot, file = "Size_non_neg_Levy_list_boot.Rda")


# LP_g, LP_lg, TFP_g conditional on size
LP_g_size_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_g", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_lg_size_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_lg", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

TFP_g_size_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "TFP_g", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_g_size_Levy_list_boot, LP_lg_size_Levy_list_boot , TFP_g_size_Levy_list_boot, file = "Size_Levy_list_boot_g.Rda")


## Industry class

#  *non-neg
LP_ind_non_neg_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP", c_names = ind_name_table$ind_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_ind_log_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "log_LP", c_names = ind_name_table$ind_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on ind *non-neg
LP_Change_non_neg_ind_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_diff", c_names = ind_name_table$ind_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_non_neg_Levy_list_boot, LP_ind_log_Levy_list_boot, LP_Change_non_neg_ind_Levy_list_boot, file = "ind_non_neg_Levy_list_boot.Rda")


# LP_g, LP_lg, TFP_g conditional on sector
LP_g_ind_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_g", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_lg_ind_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_lg", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

TFP_g_ind_Levy_list_boot <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "TFP_g", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_g_ind_Levy_list_boot, LP_lg_ind_Levy_list_boot, TFP_g_ind_Levy_list_boot, file = "Industry_Levy_list_boot_g.Rda")


#### _sim
#*non-neg
LP_ind_non_neg_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP", c_names = unique(ind_name_table$ind_agg), cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_ind_log_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "log_LP", c_names = unique(ind_name_table$ind_agg),  cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on ind *non-neg
LP_Change_non_neg_ind_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP_diff", c_names = unique(ind_name_table$ind_agg),  cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_non_neg_Levy_list_boot_sim, LP_ind_log_Levy_list_boot_sim, LP_Change_non_neg_ind_Levy_list_boot_sim, file = "ind_non_neg_Levy_list_boot_sim.Rda")


# LP_g, LP_lg, TFP_g conditional on sector
LP_g_ind_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP_g", c_names = unique(ind_name_table$ind_agg),  cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

LP_lg_ind_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "LP_lg", c_names = unique(ind_name_table$ind_agg),  cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

TFP_g_ind_Levy_list_boot_sim <- fun_fit_levy_g(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "ind_agg", var_ind = "TFP_g", c_names = unique(ind_name_table$ind_agg),  cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_g_ind_Levy_list_boot_sim, LP_lg_ind_Levy_list_boot_sim, TFP_g_ind_Levy_list_boot_sim, file = "Industry_Levy_list_boot_g_sim.Rda")
