# Script to produce 2d density plots

# load packages
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(dplyr,ggplot2,ggExtra)

# function definitions

# TODO: this function should be in a separate file
load_data <- function(filename="All_list_Cleaned.Rda", return_variables) {
  # Function to load and return specified data from a specified file. The function loads the specified file and asserts 
  #     that the requested objects are present.
  # Arguments:
  #     filename: string                    - name of data file
  #     return_variables: list of string    - names of requested return variables
  # Returns:
  #     list of objects. The objects are the loaded data structures, their names are preserved and match the requested names 
  loaded <- load(filename)
  for (var in return_variables) {
    if (!(hasName(mget(loaded), var))) {
      print(paste("Requested object", var, "not present in loaded file", filename))
      print("Giving up.")
      quit(status=1)
    }
  }
  return(mget(loaded))
}

plot_2ddensity <- function(data_frames, country_names, var_name1="LP", var_name2="LP_g", cutoff_quantile=0.005) {
  # Function to plot 2d densities by country. Will take list of data frames and list of conutry names to produce a series of 2d 
  #    density plots in pdf format.
  # Arguments:
  #     data_frames: list of data.frame     - List of input data frames
  #     country_names: list of string       - List of country names
  #     var_name1: string                   - Name of first variable
  #     var_name2: string                   - Name of second variable
  #     cutoff_quantile: float              - Quantile to be removed from both tails (each)
  # Returns: None.
  
  neg_cut <- cutoff_quantile
  pov_cut <- 1 - neg_cut 
  
  # Loop over countries
  for (i in 1:length(data_frames)) {
    
    # prepare data frame
    df = data.frame(data_frames[[i]])
    df$Var1 <- df[ , var_name1]
    df$Var2 <- df[ , var_name2]
    df <- df %>%
      select(IDNR, Year, Var1, Var2, COMPCAT, NACE_CAT) %>%
      na.omit() %>%                                                               # omit NA
      filter(Var1 > quantile(Var1, neg_cut) & Var1 < quantile(Var1, pov_cut)) %>% # cut the tails for Var1
      filter(Var2 > quantile(Var2, neg_cut) & Var2 < quantile(Var2, pov_cut))     # cut the tails for Var2
    
    # plot and save if the data frame is not empty
    if (nrow(df)>0) {
      gp <- ggplot(df, aes(x=Var1, y=Var2) ) + geom_hex(bins=100) + theme_bw() + ggtitle(country_names[[i]]) + xlab(var_name1) + ylab(var_name2) + scale_fill_continuous(name="count", trans="log")
      filename = paste("Density_2D_", country_names[[i]], ".pdf", sep="")
      ggsave(filename, ggMarginal(gp, type = "histogram", size = ), width = 5, height = 5)
    }
  }
}


# main entry point
data <- load_data(filename="All_list_Cleaned.Rda", return_variables=c("All_list_Cleaned", "country_names"))
plot_2ddensity(data$All_list_Cleaned, data$country_names)
