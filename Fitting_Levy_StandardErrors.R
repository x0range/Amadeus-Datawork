# Function to do Levy alpha stable parameter error estimates using bootstraps.
# TODO: File Fitting_Levy_Boot.R should be merged into this one.
#           - They duplicate much code.
#           - This one is nicer and returns a data frame, not a list like Fitting_Levy_Boot.R 

############ 0. Basic Set up ############
## 0.1. loading of required libraries

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim,devtools)

## 0.2 loading fitting functions from fitting.levy package
devtools::load_all("fittinglevy")

## 0.3 remaining function definitions: 
# the fuction for the fitting result
fun_fit_levy <- function(dat, bin_num, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used for the conditional class, 4) the target variable name, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  results_df <- data.frame(Fit_Variable=character(), Separator_Variable=character(), Class=character(), class_idx=integer(), Country=character(), country_idx=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_alpha_Standard_Error=double(), Levy_beta_Standard_Error=double(), Levy_gamma_Standard_Error=double(), Levy_delta_Standard_Error=double(), Levy_Soofi_ID=double(), stringsAsFactors=FALSE)
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  for (k in 1:length(dat)) {
    print(k)
  
    zz <- dat[[k]] %>%
      select(IDNR, Year, COMPCAT, NACE_CAT, LP, LP_diff, EMPL) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
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
    
    
    if (nrow(zz) != 0) {
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

        levy_result <- levy_fitting(dat_t = c_lp, bin_num = bin_num, include_standarderror=TRUE, include_Soofi=TRUE, fitting_method="GMM") # Levy estimation
        
        results_df[nrow(results_df)+1,] = list(var_ind, cond_ind, c_uni_name[[c]], c, country_names[[k]], k, length(c_lp), levy_result$levy_para[[1]], levy_result$levy_para[[2]], levy_result$levy_para[[3]], levy_result$levy_para[[4]], levy_result$standard_errors[[1]], levy_result$standard_errors[[2]], levy_result$standard_errors[[3]], levy_result$standard_errors[[4]], levy_result$levy_soofi)    
        print(results_df)
        #results_df <- data.frame(Fit_Variable=character(), Separator_Variable=character(), Class=character(), class_idx=integer(), Country=character(), country_idx=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_alpha_Standard_Error=double(), Levy_beta_Standard_Error=double(), Levy_gamma_Standard_Error=double(), Levy_delta_Standard_Error=double(), stringsAsFactors=FALSE)
        #c_list[[c]] <- list(levy_para = levy_result$levy_para, levy_soofi = levy_result$levy_soofi, est_levy_std_error = levy_result$est_levy_std_error, data_mid = levy_result$data_mid , data_p = levy_result$data_p, levy_q = levy_result$levy_q)
      }
#      c_uni_list[[k]] <- c_uni_name # record the ordered name of unique class
      #c_uni_list_2[[k]] <- c_uni_name_2 #
#      c_uni_num_list[[k]] <- c_uni_num # record the ordered numeric name of unique class
#      result_list[[k]] <- c_list # record the result from "fun_info_gen"
    }
  }
#  all_list <- list(result_list, c_uni_list, c_uni_num_list)
  return(results_df)
}


# main entry point

##  1.1. loading of required data and cleaning
#load("All_list_Cleaned_cut.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("All_list_Cleaned_cut.Rda", verbose=T) ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("Labels.Rda")



############ 2. Fit the Levy distribution  ############
## note the following index
# con_ind: 2: year, 3: size, 4: industry
# var_ind: 5: LP, 6: LP_change, 7: TFP growth

## set up the cut-off point
neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point
year_names <- 2006:2015

## Year class
# LP conditional on year (year class)
LP_year_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on year
LP_Change_year_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_year_Levy_GMM_df_SE, LP_Change_year_Levy_GMM_df_SE, file = "Year_Levy_GMM_df_SE_2.Rda")

## Size class
# LP conditional on size
LP_size_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on size
LP_Change_size_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "COMPCAT", var_ind = "LP_diff", c_names = size_names_long, cut_num = 5000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_size_Levy_GMM_df_SE, LP_Change_size_Levy_GMM_df_SE, file = "Size_Levy_GMM_df_SE_2.Rda")

## Industry class
# LP conditional on sector
LP_ind_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

# LP_change conditional on sector
LP_Change_ind_Levy_GMM_df_SE <- fun_fit_levy(dat = All_list_Cleaned_cut, bin_num = 100, cond_ind = "NACE_CAT", var_ind = "LP_diff", c_names = ind_name_table$ind_names, cut_num = 1000, neg_cut = neg_cut, pov_cut = pov_cut)

save(LP_ind_Levy_GMM_df_SE, LP_Change_ind_Levy_GMM_df_SE, file = "Industry_Levy_GMM_df_SE_2.Rda")

####
load("Year_Levy_GMM_df_SE.Rda")
load("Size_Levy_GMM_df_SE.Rda")
load("Industry_Levy_GMM_df_SE.Rda")
load("Labels.Rda")

dim(LP_year_Levy_GMM_df_SE)

library(RColorBrewer)
fun_levy_par_gmm <- function(pdf_name, title, dat_list, x_value, x_value_name, x_lab, leg_pos, leg_ind, year_ind) { # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  c_uni_list <- list(); 
  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()
  for (k in 1:5) {
    
    ok <- subset(dat_list, Country == country_names_five[[k]])
    ok <- do.call(data.frame,lapply(ok, function(x) replace(x, is.infinite(x),NA)))
    
    ok <- na.omit(ok)
    ok <- subset(ok, Levy_Soofi_ID > 90)
    
     #ok <- subset(ok, Levy_gamma_Standard_Error < Levy_gamma)
    # 
    # ok <- subset(ok, Levy_alpha_Standard_Error < Levy_alpha)
    # 
    
    if(year_ind == 1){
      c_uni_list[[k]] <- as.numeric(as.character(ok$Class)) # the numeric value of the unique class
      
    }else{
      c_uni_list[[k]] <- ok$class_idx # the numeric value of the unique class
      
    }
    
    par_a[[k]] <- ok$Levy_alpha # parameter \alpha
    par_a_sd[[k]] <- ok$Levy_alpha_Standard_Error# sd of parameter \alpha
    
    par_b[[k]] <- ok$Levy_beta # parameter \beta
    par_b_sd[[k]] <- ok$Levy_beta_Standard_Error
    par_g[[k]] <- ok$Levy_gamma
    # parameter \gamma
    par_g_sd[[k]] <- ok$Levy_gamma_Standard_Error
    par_d[[k]] <- ok$Levy_delta # parameter \delta
    par_d_sd[[k]] <-ok$Levy_delta_Standard_Error
    
  }
  

  
  par_list <- list(par_a, par_b, par_g, par_d)
  par_sd_list <- list(par_a_sd, par_b_sd, par_g_sd, par_d_sd)
  # parameter names
  par_name_list <- list(
    expression(paste(alpha, ": tail")),
    expression(paste(beta, ": skewness")),
    expression(paste(gamma, ": scale")),
    expression(paste(delta, ": location"))
  )
  ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta))) ## for the y_lab
  
  # lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)
  
  
  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 5., width = 8)
  par(mfrow = c(2, 2), mar = c(3, 2.2, 1, 1), mgp = c(1.2, .3, 0), tck = -.01, oma = c(0, 0, 2, 0), las = 1)
  

  l <- 1 # first parameter
  plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = "Year", ylab = ylab_list[[l]], cex.lab = 1., cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
  # mtext(side = 1, text=paste0(x_lab), line = 1, cex = 0.7)
  
  axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85) # x-axis
  axis(side = 2, lwd = 0.3, cex.axis = 0.85, las = 2) # y-axis
  
  
  for (k in 1:5) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.7, pch = 20, type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample
    
    polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]])) # error bar
    polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))
    
    polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[k], alpha.f = 0.4), border = NA)
  }
  
  for (l in 2:4) { # 2nd to 4th parameters
    plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1., cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])))) # Empty plot for the first parameter: x_value is the numeric value of all classes
    
    axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85)
    axis(side = 2, lwd = 0.3, cex.axis = 0.85)
    
    for (k in 1:5) {
      points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[k], cex = 0.7, pch = 20, type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample
      
      # error bars
      polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]]))
      polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))
      
      polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[k], alpha.f = 0.4), border = NA)
    }
    
    
  }
  if(leg_ind == 1){
    legend("topright", inset=c(1.3,-1.65), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[1:5], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.1, ncol = 1, horiz = T)
    
  }
  # legend outside the main plots
  
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.2)
  dev.off()
}

fun_levy_par_gmm(pdf_name = "Figure_Levy_Para_LP_Year", title = "Labor Productivity", dat_list = LP_year_Levy_GMM_df_SE, x_value = year_names, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft", leg_ind = 1, year_ind = 1)

fun_levy_par_gmm(pdf_name = "Figure_Levy_Para_LP_Change_Year", title = "Labor Productivity Change", dat_list = LP_Change_year_Levy_GMM_df_SE, x_value = year_names[-c(1)], x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "topleft", leg_ind = 1, year_ind = 1)

fun_levy_par_gmm(pdf_name = "Figure_Levy_Para_LP_Size", title = "Labor Productivity", dat_list = LP_size_Levy_GMM_df_SE, x_value = 1:4, x_value_name = size_names, x_lab = "Size", leg_pos = "topleft", leg_ind = 1, year_ind = 0)

fun_levy_par_gmm(pdf_name = "Figure_Levy_Para_LP_Change_Size", title = "Labor Productivity Change", dat_list = LP_Change_size_Levy_GMM_df_SE,x_value = 1:4, x_value_name = size_names, x_lab = "Size", leg_pos = "topleft", leg_ind = 1, year_ind = 0)

