############ 0. Basic Set up ############
## loading of required libraries
# loading of libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(colorspace,RColorBrewer, msir, plotly, dplyr)

# loading of data
load("Year_Levy_list_boot.Rda")

load("Labels.Rda")
load("All_list_Cleaned_cut.Rda")

penn_data <- read.csv("pwt90.csv", stringsAsFactors = F, header = T)

penn_15 <- subset(penn_data[penn_data$country%in%country_names,], 
                  year >= 2006 & year <= 2015)

# rgdpna: Real GDP at constant 2011 national prices (in mil. 2011US$)
# emp: Number of persons engaged (in millions)

# rtfpna: TFP at constant national prices (2011=1)

penn_15_cut <- penn_15 %>%
  select(country, year, rgdpna, emp, rtfpna) %>%
  group_by(country) %>%
  mutate(LP = rgdpna/emp) %>%
  mutate(LP_change = (LP - lag(LP,1))/lag(LP,1),
         GPD_change = (rgdpna - lag(rgdpna,1))/lag(rgdpna,1),
         TFP_change = (rtfpna - lag(rtfpna,1))/lag(rtfpna,1))



####

  colores_this <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  
dat_list <- LP_Change_year_Levy_list_boot
  all_but_3_ind <- c(1:15)[-c(3)] # index for the top five countries
  
  
  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list();
  penn_lp_g_list <- list(); penn_gdp_g_list <- list(); penn_tfp_g_list <- list(); penn_year_ind <- list()
  
  for (k in all_but_3_ind) {
    
    country_ind <- country_names[k]
    pick_con <- na.omit(penn_15_cut[penn_15_cut$country%in%country_ind,])
    
    penn_lp_g_list[[k]] <- pick_con$LP_change
    penn_gdp_g_list[[k]] <- pick_con$GPD_change
    penn_tfp_g_list[[k]] <- pick_con$TFP_change
    penn_year_ind[[k]] <- pick_con$year
    
    c_uni_list[[k]] <- c(2006:2015)[dat_list[[3]][[k]]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(dat_list[[1]][[k]], function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }
  

  par_d <- list()
  for (k in all_but_3_ind) {
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    #par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
  }
 
  ###
   p_list <- list()
  for (k in all_but_3_ind) {


  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "LP_Change (Penn)",
    zeroline = FALSE
  )
  
  p <- plot_ly() %>%
    add_lines(x = c_uni_list[[k]], y = par_d[[k]], name = "Orbis", line = list(shape = "spline", width = 4))%>%
    add_lines(x = penn_year_ind[[k]], y = penn_lp_g_list[[k]], name = "PWT", yaxis = "y2", line = list(shape = "spline", width = 4)) %>%
    layout(
      title = country_names[k], yaxis2 = ay,
      xaxis = list(title="Year"),
      yaxis = list(title = "Location of LP_Change (Orbis)",
                   zeroline = FALSE)
    )
  
  
  p_list[[k]] <- p
  
  }
   
   p_list[[14]]
   
   if (!require("processx")) install.packages("processx")
   Sys.setenv("plotly_username" = "yangjh2612")
   Sys.setenv("plotly_api_key" = "gASmTGISfKZf9e2LCJAI")
   

   for(k in all_but_3_ind[11:14]){
     plotly_IMAGE(p_list[[k]], format = "jpeg", out_file = paste(country_names[k],".png", sep = ""))  
   }
  
   