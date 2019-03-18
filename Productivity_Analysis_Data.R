# Script to join by-country-panels into one Rda file with the relevant variables for further analysis

# Load libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,plyr,zoo)

# Define "not in" operator
'%nin%' <- Negate('%in%')

# Define country names and years
year_names <- c(2006:2015)
country_names  <- c('Albania', 'Austria', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kosovo', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macedonia, FYR', 'Malta', 'Monaco', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Moldova', 'Romania', 'Russian Federation', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'Turkey', 'Ukraine', 'United Kingdom')

# Prepare lists to collect country data
non_na_ind <- list()
Firm_Size_list <- list()
Index_list <- list()
Productivity_list <- list() 
Profitability_list <- list()
Cost_Structure_list <- list()

# Loop over countries, read files and collect desired data into lists
for(i in c(1:length(country_names))){
  print(i)
  file_name <- paste("panels_J!&", paste(country_names[i], collapse=""), ".Rda", sep="")
  load(file_name)
  
  # get indices
  idx_required_present_Firm_size_variables <- which(colnames(Cleaned_dat_Firm_Size) %in% c("EMPL","TOAS", "VA"))
  idx_retain_INDEX_variables <- which(colnames(Cleaned_dat_INDEX) %in% c("NUTS_1", "NUTS_2", "NUTS_3", "NACE_PRIM_CODE", "COMPCAT", "Firm_Age"))
  idx_retain_Productivity_variables <- which(colnames(Cleaned_dat_Productivity) %nin% c("IDNR", "Year"))
  idx_retain_Profitability_variables <- which(colnames(Cleaned_dat_Profitability) %nin% c("IDNR", "Year"))
  idx_retain_Cost_Structure_variables <- which(colnames(Cleaned_dat_Cost_Structure) %in% c("WS", "WS_AD"))
  
  
  non_na_ind[[i]] <- which(apply(is.na(Cleaned_dat_Firm_Size[, idx_required_present_Firm_size_variables]), 1, sum) == 0) # non-missing for emp, toas, and va 
  
  Firm_Size_list[[i]] <- Cleaned_dat_Firm_Size[non_na_ind[[i]],] 
  
  Index_list[[i]] <- Cleaned_dat_INDEX[non_na_ind[[i]], idx_retain_INDEX_variables]# nut_3, nace code, firm_size, firm_age, and name
  
  Productivity_list[[i]] <- Cleaned_dat_Productivity[non_na_ind[[i]], idx_retain_Productivity_variables] # all 
  
  Profitability_list[[i]] <- Cleaned_dat_Profitability[non_na_ind[[i]], idx_retain_Profitability_variables] # all but year and ID
  
  Cost_Structure_list[[i]] <- Cleaned_dat_Cost_Structure[non_na_ind[[i]], idx_retain_Cost_Structure_variables] # ws and ws_ad
  #
  #non_na_ind[[i]] <- which(apply(is.na(Cleaned_dat_Firm_Size[,c(4,5,7)]), 1, sum) == 0) # non-missing for emp, toas, and va 
  #
  #Firm_Size_list[[i]] <- Cleaned_dat_Firm_Size[non_na_ind[[i]],] 
  #
  #Index_list[[i]] <- Cleaned_dat_INDEX[non_na_ind[[i]], c(5,6,8,10,13)]# nut_3, nace code, firm_size, firm_age, and name
  #
  #Productivity_list[[i]] <- Cleaned_dat_Productivity[non_na_ind[[i]],-c(1,2)] # all 
  #
  #Profitability_list[[i]] <- Cleaned_dat_Profitability[non_na_ind[[i]],-c(1,2)] # all but year and ID
  #
  #Cost_Structure_list[[i]] <- Cleaned_dat_Cost_Structure[non_na_ind[[i]],c(3,4)] # ws and ws_ad
  #
  
}

# Prepare joint list
All_list_Cleaned <- list()

# Join variables for all countries
for(k in 1:length(Productivity_list)){
  print(k)
  All_list_Cleaned[[k]] <- cbind(Index_list[[k]], Firm_Size_list[[k]], Productivity_list[[k]], Profitability_list[[k]], Cost_Structure_list[[k]])
  

  Index_list[[k]] <- 1 
  Firm_Size_list[[k]] <- 1
  Productivity_list[[k]] <- 1  
  Profitability_list[[k]] <- 1
  Cost_Structure_list[[k]] <- 1
}

#Assign NACE code

load("NACE2_CLASS_DES.Rda")

nace2$code <- as.numeric(nace2$code)
nace2 <- nace2[!is.na(nace2$code),]

All_list_Cleaned <- lapply(All_list_Cleaned, function(x){
  x <- x %>% left_join(nace2 %>% select(NACE_PRIM_CODE = code, NACE_DES = description, NACE_CAT = category), by = "NACE_PRIM_CODE")
})


# Save list
save(All_list_Cleaned, country_names, file = "All_list_Cleaned.Rda")


### ------------------------------------------------------------------------
#library(knitr)
#purl("Productivity_Analysis_Data.Rmd")  

