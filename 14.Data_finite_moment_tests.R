# Load packages
library(pacman)
pacman::p_load(tidyverse,xtable,foreach,dplyr,Rcpp,stabledist)

# Load Trapani test package from Rcpp
#sourceCpp("finite_moment_test.cpp")
library(finity)

load("Labels.Rda", verbose=T)
load("All_list_Cleaned.Rda", verbose=T)

# Cleaning
All_list_Cleaned_cut <- list() 
for (k in 1:length(All_list_Cleaned)) {
  print(k)
  All_list_Cleaned_cut[[k]] <- All_list_Cleaned[[k]] %>%
    select(IDNR, Year, COMPCAT, NACE_CAT, NACE_PRIM_CODE, LSTATUS, EMPL, Firm_Age, LP, CP, TFP, LP_lr, TFP_lr, LP_diff, TFP_diff, ZOMBIE, WS, SALE) %>% # Firm ID, Year, Firm Size, Industry ID, Legal Status, Employment, Firm age, Labor Produtivity, TFP, LP log return, TFP log return, LP difference, TFP difference,  Zombie firm index
    mutate(COMPCAT_one = substr(COMPCAT, 1, 1)) %>% # the first letter of the size variable
    group_by(Year) %>%
    filter(length(IDNR) > 10000) %>% # Note that 23 countries out of 44 do not have VA info. Out of 21 countries, 15 countries have enough info for anlaysis (more than 5 years with 10,000 firms )
    group_by()
}

# Countries with more than 5 year obs with 10,000 firms
take_this <- which(unlist(lapply(All_list_Cleaned_cut, function(x) length(unique(x$Year)) > 5))) 

All_list_Cleaned_cut <- All_list_Cleaned_cut[take_this]
country_names <- country_names[take_this]

prod_data <- list()

for(k in 1:15){
  print(k)
  prod_data[[k]] <- All_list_Cleaned_cut[[k]] %>%
    select(IDNR, Year, COMPCAT, NACE_CAT, LP, CP, TFP, LP_diff, EMPL,WS) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
    filter(EMPL > 1) %>% # remove self-employed persons
    mutate(LP = LP / 1000) %>% # change the unit scale of the labor productivity by dividing it by 1000
    mutate(LP_diff = LP_diff / 1000) %>% 
    filter(LP > 0) %>% # keep positive value added
    filter(CP > 0) %>% # keep positive value added
    group_by(IDNR) %>% 
    arrange(IDNR, Year) %>%
    mutate(LP_g = (LP - lag(LP,1))/lag(LP,1),
       CP_g = (CP - lag(CP,1))/lag(CP,1),
       TFP_g = LP_g*lag(WS,1) + CP_g*(1-lag(WS,1)),
       log_LP = log(LP),
       log_CP = log(CP),
       log_TFP = WS*log_LP + (1-WS)*log_CP,
       LP_lg = log(LP/lag(LP,1)),
       CP_lg = log(CP/lag(CP,1)),
       TFP_lg = LP_lg*lag(WS,1) + CP_lg*(1-lag(WS,1)),
       LP_diff = LP_diff / 1000) %>%
    mutate(Year_diff = Year - lag(Year,1)) %>%
    mutate(LP_diff = ifelse(Year_diff > 1, NA, LP_diff)) %>%
    mutate(LP_g = ifelse(Year_diff > 1, NA, LP_g),
         CP_g = ifelse(Year_diff > 1, NA, CP_g),
         TFP_g = ifelse(Year_diff > 1, NA, TFP_g),
         LP_lg = ifelse(Year_diff > 1, NA, LP_lg),
         CP_lg = ifelse(Year_diff > 1, NA, CP_lg),
         TFP_lg = ifelse(Year_diff > 1, NA, TFP_lg),
         LP_diff = ifelse(Year_diff > 1, NA, LP_diff)
    ) 
}


finite_moment <- data.frame(matrix(vector(), 2, length(country_names_five) + 1, dimnames = list(c('Theta', 'P-Value'), c('Year', country_names_five))), stringsAsFactors = F)

for (variable in c("LP", "LP_diff", "log_LP", "LP_lg", "TFP", "TFP_g", "log_TFP", "TFP_lg")) {
  for (moment_order in c(1,2)) {
    message(paste("\n\n\n\nCommencing moment:", as.character(moment_order), "\n\n"))
    # fill data frame by looping over samples

    finite_moment_by_years <- foreach(j = year_names, .combine = rbind) %do% {
      for(i in 1:length(country_names_five)){ # for each key country
        
        #message(paste(country_names_five[i], j, sep = ' '))
        
        # the country's table number
        country_ind <- which(country_names %in% country_names_five[i])
        
        
        # find the observations for that year
        year_index <- which(prod_data[[country_ind]]$Year == j)
        #finite_moment$Year <- rep(j, times = 2)
        finite_moment$Year <- c(paste("\\multirow{2}{*}{", j, "}", sep=""), "")
        
        # select data by variable
        if (variable == "LP") {#lp
          data = (prod_data[[country_ind]]$LP[year_index])
        } else if (variable == "LP_diff") {#lp diff
          data = (prod_data[[country_ind]]$LP_diff[year_index])
        } else if (variable == "log_LP") {#log LP
          data = (prod_data[[country_ind]]$log_LP[year_index])
        } else if (variable == "LP_lg") {#LP_lg
          data = (prod_data[[country_ind]]$LP_lg[year_index])
        } else if (variable == "TFP") {#TFP
          data = (prod_data[[country_ind]]$TFP[year_index])
        } else if (variable == "TFP_g") {#TFP_g
          data = (prod_data[[country_ind]]$TFP_g[year_index])
        } else if (variable == "log_TFP") {#log_TFP
          data = (prod_data[[country_ind]]$log_TFP[year_index])
        } else if (variable == "TFP_lg") {#TFP_lg
          data = (prod_data[[country_ind]]$TFP_lg[year_index])
        }
        
         
        result <- finite_moment_test(obs = data, k = moment_order, ignore_errors=T, psi=moment_order/2.)
        finite_moment[1, i+1] <- result[[1]]
        finite_moment[2, i+1] <- result[[2]]
      }
      finite_moment
    }
    
    # save data
    filename = paste("Data_finite_moments_test_results_", variable, "_moment_order_", as.character(moment_order), ".Rda", sep="")
    save(finite_moment_by_years, file=filename)

    # Prapare table for printing as xtable
    # convert to matrix
    finite_moment_by_years_numeric <- data.matrix(finite_moment_by_years, rownames.force = NA)
    finite_moment_by_years <- as.matrix(finite_moment_by_years, rownames.force = NA)
    # apply brackets, digits
    ind = which((row(finite_moment_by_years) %% 2 == 0) & (col(finite_moment_by_years) > 1), arr.ind=T)
    finite_moment_by_years[ind[,1], ind[,2]] <- sprintf("(%.3f)", finite_moment_by_years_numeric[ind[,1], ind[,2]])
    ind = which((row(finite_moment_by_years) %% 2 == 1) & (col(finite_moment_by_years) > 1), arr.ind=T)
    finite_moment_by_years[ind[,1], ind[,2]] <- sprintf("%.3f", finite_moment_by_years_numeric[ind[,1], ind[,2]])
    
    for (row_idx in 1:nrow(finite_moment_by_years_numeric)) {
      if (row_idx %% 2 == 0) {
        for (col_idx in 2:ncol(finite_moment_by_years_numeric)) {
          if (!is.na(finite_moment_by_years_numeric[row_idx, col_idx]) && finite_moment_by_years_numeric[row_idx, col_idx] > 0.1) {
            finite_moment_by_years[row_idx, col_idx] = paste(finite_moment_by_years[row_idx, col_idx], "**", sep="")
          } else if (!is.na(finite_moment_by_years_numeric[row_idx, col_idx]) && finite_moment_by_years_numeric[row_idx, col_idx] > 0.05) {
            finite_moment_by_years[row_idx, col_idx] = paste(finite_moment_by_years[row_idx, col_idx], "*", sep="")
          }
        }
      }
    }

    # caption
    caption_var_name <- variable
    if (variable=="LP_diff") {
      caption_var_name <- "LP Change"
    } else if (variable=="log_LP") {
      caption_var_name <- "log LP"
    } else if (variable=="LP_lg") {
      caption_var_name <- "Change in log LP"
    } else if (variable=="TFP_g") {
      caption_var_name <- "TFP growth"
    } else if (variable=="log_TFP") {
      caption_var_name <- "log TFP"
    } else if (variable=="TFP_lg") {
      caption_var_name <- "Change in log TFP"
    }
    
    captiontext = paste(caption_var_name, ": Testing for infinite moment of order ", as.character(moment_order), ".", sep="")
    
    # print xtable
    print(xtable(finite_moment_by_years, caption = captiontext), sanitize.text.function = force, include.rownames = F)
    
  }  
}
