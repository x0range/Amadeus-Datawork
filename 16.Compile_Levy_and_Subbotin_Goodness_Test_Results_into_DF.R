library(feather)

parse_one_record = function(variable, country, years, years2, years3, dat, dat2, dat3) {
    # Function to parse one estimate from nested lists. 
    # Accepts arguments:
    #       variable: (character) Estimation variable name
    #       country: (character) country name
    #       years: (nested list) year in AIC/Soofi record
    #       dat: (nested list) AIC/Soofi record list
    #       years2: (nested list) year in k-fold cv record
    #       dat2: (nested list) k-fold cv record list
    #       years3: (nested list) year in observation number record
    #       dat3: (nested list) k-fold  observation number record list
    
    # assert data is sane
    if (years!=years2|years!=years3) {
        print(paste("Inconsistent record discovered. Years:", years, years2, years3))
        if (variable!="LP_Change"|years!=years2|years3-1!=years) {                      # num_obs.Rda is somehow weird
            print(paste(variable, years, years2, years3-1))
        }
        quit(status=-4) 
    }
    
    # prepare data frame
    df3 = data.frame(Variable=character(), Country=character(), Year=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_AIC=double(), Levy_Soofi_ID_S=double(), Levy_CV_Likelihood=double(), AEP_xi=double(), AEP_alpha=double(),AEP_h=double(), AEP_kappa=double(), AEP_AIC=double(), AEP_Soofi_ID_S=double(), AEP_CV_Likelihood=double(), stringsAsFactors=FALSE)
    
    # get records
    levy_alpha = dat$levy_para[[1]]
    levy_beta = dat$levy_para[[2]]
    levy_gamma = dat$levy_para[[3]]
    levy_delta = dat$levy_para[[4]]
    levy_AIC = dat$levy_aid
    levy_Soofi_ID_score = dat$levy_soofi
    levy_cv_L = sum(unlist(dat2[[1]]))/10.
    AEP_xi = dat$sub_para[[1]]              # right order?
    AEP_sigma = dat$sub_para[[2]]           # right order?
    AEP_h = dat$sub_para[[3]]               # right order?
    AEP_kappa = dat$sub_para[[4]]           # right order?
    AEP_AIC = dat$sub_aic
    AEP_Soofi_ID_score = dat$sub_soofi
    AEP_cv_L = sum(unlist(dat2[[2]]))/10.
    num_obs = dat3
    
    # record parameters
    df3[nrow(df3)+1,] = list(variable, country, years, num_obs, levy_alpha, levy_beta, levy_gamma, levy_delta, levy_AIC, levy_Soofi_ID_score, levy_cv_L, AEP_xi, AEP_sigma, AEP_h, AEP_kappa, AEP_AIC, AEP_Soofi_ID_score, AEP_cv_L)
    
    # return
    return(df3)
}



# main entry point

# load data
load("Year_list_compare.Rda", verbose=T) # provides LP_year_list_compare, LP_Change_year_list_compare
load("Year_list_compare_AIC_SOOFI.Rda", verbose=T) # provides LP_year_list_compare_AIC_SOOFI, LP_Change_year_list_compare_AIC_SOOFI
load("num_obs.Rda", verbose=T) # provides LP_num_obs, LP_Change_num_obs
load("Labels.Rda", verbose=T) # provides  year_names, country_names, country_names_five, ind_name_table, size_names, size_names_long

print("")
print("LP")

# Computations for LP: prepare df
df = data.frame(Variable=character(), Country=character(), Year=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_AIC=double(), Levy_Soofi_ID_S=double(), Levy_CV_Likelihood=double(), AEP_xi=double(), AEP_alpha=double(),AEP_h=double(), AEP_kappa=double(), AEP_AIC=double(), AEP_Soofi_ID_S=double(), AEP_CV_Likelihood=double(), stringsAsFactors=FALSE)

# Computations for LP: populate df
for (i in 1:length(LP_year_list_compare_AIC_SOOFI[[1]])) {
    years = LP_year_list_compare_AIC_SOOFI[[2]][[i]]
    years2 = LP_year_list_compare[[2]][[i]]
    years3 = LP_num_obs[[2]][[i]]
    dat = LP_year_list_compare_AIC_SOOFI[[1]][[i]]
    dat2 = LP_year_list_compare[[1]][[i]]
    dat3 = LP_num_obs[[1]][[i]]
    if (length(years) > 0) {
        for (j in 1:length(years)) {
            df_next = parse_one_record("LP", country_names[[i]], years[[j]], years2[[j]], years3[[j]], dat[[j]], dat2[[j]], dat3[[j]])
            df <- rbind(df, df_next)
        }
    }
}

# Computations for LP: save df
save(df, file="Orbis_LP_goodness_test_results.Rda")
write_feather(df, "Orbis_LP_goodness_test_results.feather")

print("")
print("LP_Change")


# Computations for LP Change: prepare df
df = data.frame(Variable=character(), Country=character(), Year=integer(), Observations=integer(), Levy_alpha=double(), Levy_beta=double(), Levy_gamma=double(), Levy_delta=double(), Levy_AIC=double(), Levy_Soofi_ID_S=double(), Levy_CV_Likelihood=double(), AEP_xi=double(), AEP_alpha=double(),AEP_h=double(), AEP_kappa=double(), AEP_AIC=double(), AEP_Soofi_ID_S=double(), AEP_CV_Likelihood=double(), stringsAsFactors=FALSE)
    
# Computations for LP Change: populate df
for (i in 1:length(LP_year_list_compare_AIC_SOOFI[[1]])) {
    years = LP_Change_year_list_compare_AIC_SOOFI[[2]][[i]]
    years2 = LP_Change_year_list_compare[[2]][[i]]
    years3 = LP_Change_num_obs[[2]][[i]]
    dat = LP_Change_year_list_compare_AIC_SOOFI[[1]][[i]]
    dat2 = LP_Change_year_list_compare[[1]][[i]]
    dat3 = LP_Change_num_obs[[1]][[i]]
    if (length(years) > 0) {
        for (j in 1:length(years)) {
            if (length(years3) < length(years)) {
                print(paste("Inconsistent records for country", country_names[[i]]))
                print(years)
                print(years2)
                print(years3)
                browser()
            }
            df_next = parse_one_record("LP_Change", country_names[[i]], years[[j]], years2[[j]], years3[[j]], dat[[j]], dat2[[j]], dat3[[j]])
            df <- rbind(df, df_next)
        }
    }
}

print(df)
# Computations for LP Change: save df
save(df, file="Orbis_LP_Change_goodness_test_results.Rda")
write_feather(df, "Orbis_LP_Change_goodness_test_results.feather")

