############ 0. Basic Set up ############
## loading of required libraries
# loading of libraries
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(colorspace,RColorBrewer, msir, scales, grDevices,dplyr)

# loading of data
load("Year_Levy_list_boot.Rda")
load("Size_Levy_list_boot.Rda")
load("Industry_Levy_list_boot.Rda")
load("Industry_Levy_list_boot_sim.Rda")

load("Year_Levy_list_boot_g.Rda")
load("Size_Levy_list_boot_g.Rda")
load("Industry_Levy_list_boot_g.Rda")
load("Industry_Levy_list_boot_g_sim.Rda")

load("Year_non_neg_Levy_list_boot.Rda")
load("Size_non_neg_Levy_list_boot.Rda")
load("ind_non_neg_Levy_list_boot.Rda")
load("ind_non_neg_Levy_list_boot_sim.Rda")

load("Labels.Rda")
load("All_list_Cleaned_cut.Rda")


############ 1. Plot the distribution with the Levy fitted lines ############
## Basically the same plot function but augumented with the fitter line from the Levy distribution. For detailed explanations, see the function detail of "Descriptive_Analysis.R" script.

## 1.1. function

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")


fun_plot_levy_fit <- function(pdf_name, title, dat_list, c_names, x_lab,  con_ind) {


    cairo_pdf(paste(pdf_name, ".pdf", sep = ""),  height = 7.5, width = 7.5)
    par(mfrow = c(3, 3), mar = c(3, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0), las = 1)
    
    k <- which(country_names%in%con_ind)
    
    
    con_temp <- dat_list[[1]][[k]]

    c_uni <- dat_list[[2]][[k]]
    
    if(length(c_uni) == 10){
      c_uni <- c_uni[-c(1)]
      con_temp <- dat_list[[1]][[k]][-c(1)]
      c_uni <- dat_list[[2]][[k]][-c(1)]
    }
    

    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0)

    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni <- c_uni[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    
    for (y in 1:length(con_temp)) {
      c_ind <- which(c_names %in% c_uni[y])
      
      
      plot(con_temp[[y]]$data_mid, log10(con_temp[[y]]$data_p), cex = 1,  yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Density", main =   c_uni[y] ,  pch = 21, bg = "lightgray", col = "black", frame = FALSE)
      
      
          #mtext(side = 1, text=paste0(x_lab), line = 1, cex = 0.7)
      #mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)

      if( x_lab == "LP (log)"){
        axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
        
        mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  0.7)
        
        
      }else if(x_lab == "LP_Growth"){
        axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
        
        mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  0.7)
        
        
      }else{
        axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),-2),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),-2), lwd = 0.3, cex.axis = .9)
        
        mtext(side = 1, text= paste(x_lab," (", euro(1000) ,"/Employee)", sep = ""), line = 1.2, cex =  0.7)
        
        
      }
          
      #axis(side = 2, lwd = 0.3, cex.axis = .9)
      #axis_int <- pretty(axTicks(2), n = 4)
      axis_int <- seq(min(log10(con_temp[[y]]$data_p[con_temp[[y]]$data_p >0])), max(log10(con_temp[[y]]$data_p)), length.out = 4)
      labels <- sapply(round(axis_int,0),function(i) as.expression(bquote(10^ .(i))))
      axis(2, at = axis_int, labels = labels, lwd = 0.3, cex.axis = .9)
      
      

      lines(con_temp[[y]]$data_mid, log10(con_temp[[y]]$levy_q),  lwd = 1.1, lty = 1) # Levy fit

    }

    mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
    dev.off()
  }


##
fun_plot_levy_fit_three <- function(pdf_name, title, dat_list, c_names, x_lab,  con_ind) {


  cairo_pdf(paste(pdf_name, "_three.pdf", sep = ""),  height = 2.5, width = 7.5)
  par(mfrow = c(1, 3), mar = c(3, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0), las = 1)

  k <- which(country_names%in%con_ind)


  con_temp <- dat_list[[1]][[k]]

  c_uni <- dat_list[[2]][[k]]

  if(length(c_uni) == 10){
    c_uni <- c_uni[-c(1)]
    con_temp <- dat_list[[1]][[k]][-c(1)]
    c_uni <- dat_list[[2]][[k]][-c(1)]
  }


  levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0)

  if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
    ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
    con_temp <- con_temp[-ok_ind]
    c_uni <- c_uni[-ok_ind]
    levy_soofi <- levy_soofi[-ok_ind]
  }



  for (y in c(2, 5, 8)) {
    c_ind <- which(c_names %in% c_uni[y])


    if(x_lab == "LP (log)"| x_lab =="LP ( > 0)"| x_lab =="LP"){
      
      plot(con_temp[[y]]$data_mid, log10(con_temp[[y]]$data_p), cex = 1,  yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Density", main =   paste(c_uni[y]) ,  pch = 21, bg = "lightgray", col = "black", frame = FALSE)
      
    }else{
      
      plot(con_temp[[y]]$data_mid, log10(con_temp[[y]]$data_p), cex = 1,  yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Density", main =   "",  pch = 21, bg = "lightgray", col = "black", frame = FALSE)
      
    }


        #mtext(side = 1, text=paste0(x_lab), line = 1, cex = 0.7)
    #mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)

    if( x_lab == "LP (log)"){
      axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
      
      mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  0.7)
      
      
    }else if(x_lab == "LP_Growth"){
      axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
      
      mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  0.7)
      
    }else{
      axis(1, at = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),-2),  labels = round(seq(min(con_temp[[y]]$data_mid), max(con_temp[[y]]$data_mid), length.out = 6),-2), lwd = 0.3, cex.axis = .9)
      
      mtext(side = 1, text= paste(x_lab," (", euro(1000) ,"/Employee)", sep = ""), line = 1.2, cex =  0.7)
      
      
    }
    
    #axis(side = 2, lwd = 0.3, cex.axis = .9)
    #axis_int <- pretty(axTicks(2), n = 4)
    axis_int <- seq(min(log10(con_temp[[y]]$data_p[con_temp[[y]]$data_p >0])), max(log10(con_temp[[y]]$data_p)), length.out = 4)
    labels <- sapply(round(axis_int,0),function(i) as.expression(bquote(10^ .(i))))
    axis(2, at = axis_int, labels = labels, lwd = 0.3, cex.axis = .9)



    lines(con_temp[[y]]$data_mid, log10(con_temp[[y]]$levy_q),  lwd = 1.1, lty = 1) # Levy fit

  }

  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


### LP 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log")

fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit_France", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit_Germany", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit_Italy", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names, x_lab = "LP", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit_Spain", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_Year_Levy_Fit_UK", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log (Non-Neg)")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_France", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Germany", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Italy", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names, x_lab = "LP ( > 0)", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Spain", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_UK", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Log")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_log_Levy_Fit_France", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_log_Levy_Fit_Germany", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_log_Levy_Fit_Italy", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names, x_lab = "LP (log)", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_log_Levy_Fit_Spain", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_year_log_Levy_Fit_UK", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)",con_ind = "United Kingdom")

### LP_Change and LP Growth (log and non-log)
# Change 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change")
fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit_France", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Germany", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Italy", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Spain", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_Year_Levy_Fit_UK", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Change Non-Neg
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change (Non-Neg)")
fun_plot_levy_fit(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_France", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Germany", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Italy", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Spain", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_UK", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Growth
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Non-Log")

fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit_France", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Growth", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit_Germany", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit_Italy", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit_Spain", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_g_Year_Levy_Fit_UK", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")
 
# Growth (Log)
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Log")

fun_plot_levy_fit(pdf_name = "Figure_LP_lg_Year_Levy_Fit_France", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "France")

fun_plot_levy_fit(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Germany", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Italy", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Spain", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit(pdf_name = "Figure_LP_lg_Year_Levy_Fit_UK", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")
# 
# #########@# fun_plot_levy_fit_three
# ### LP 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Year_Levy_Fit_France", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Year_Levy_Fit_Germany", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Year_Levy_Fit_Italy", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names, x_lab = "LP", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Year_Levy_Fit_Spain", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Year_Levy_Fit_UK", title = "", dat_list = LP_year_Levy_list_boot, c_names = year_names,  x_lab = "LP",con_ind = "United Kingdom")
#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log (Non-Neg)")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_France", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Germany", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Italy", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names, x_lab = "LP ( > 0)", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_Spain", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_non_neg_Levy_Fit_UK", title = "", dat_list = LP_year_non_neg_Levy_list_boot, c_names = year_names,  x_lab = "LP ( > 0)",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Log")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_log_Levy_Fit_France", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_log_Levy_Fit_Germany", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_log_Levy_Fit_Italy", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names, x_lab = "LP (log)", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_log_Levy_Fit_Spain", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_year_log_Levy_Fit_UK", title = "", dat_list = LP_year_log_Levy_list_boot, c_names = year_names,  x_lab = "LP (log)",con_ind = "United Kingdom")

### LP_Change and LP Growth (log and non-log)
# Change
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change")
fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_Year_Levy_Fit_France", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Germany", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Italy", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_Year_Levy_Fit_Spain", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_Year_Levy_Fit_UK", title = "", dat_list = LP_Change_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Change Non-Neg
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change (Non-Neg)")
fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_France", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Germany", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Italy", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_Spain", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_Change_non_neg_Year_Levy_Fit_UK", title = "", dat_list = LP_Change_non_neg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Growth
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Non-Log")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_g_Year_Levy_Fit_France", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP Growth", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_g_Year_Levy_Fit_Germany", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_g_Year_Levy_Fit_Italy", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_g_Year_Levy_Fit_Spain", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_g_Year_Levy_Fit_UK", title = "", dat_list = LP_g_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")

# Growth (Log)
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Log")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_lg_Year_Levy_Fit_France", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "France")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Germany", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Italy", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_lg_Year_Levy_Fit_Spain", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_three(pdf_name = "Figure_LP_lg_Year_Levy_Fit_UK", title = "", dat_list = LP_lg_year_Levy_list_boot, c_names = year_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")



#####

fun_plot_levy_fit_size <- function(pdf_name, title, dat_list, c_names, x_lab,  con_ind) {
  color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)] # color index 
  
  
  cairo_pdf(paste(pdf_name, ".pdf", sep = ""),  height = 6, width = 6)
  par(mfrow = c(1, 1), mar = c(3, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0), las = 1)
  
  k <- which(country_names%in%con_ind)
  
  
  con_temp <- dat_list[[1]][[k]]
  
  c_uni <- dat_list[[2]][[k]]
  
  levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0)
  
  if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
    ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
    con_temp <- con_temp[-ok_ind]
    c_uni <- c_uni[-ok_ind]
    levy_soofi <- levy_soofi[-ok_ind]
  }
  
  
  
   
  # 
  y_max <- max(unlist(lapply(con_temp, function(x) max(x$data_p, x$levy_q))))
  y_min <- min(unlist(lapply(con_temp, function(x) min(x$data_p[x$data_p > min(x$data_p)], x$levy_q[x$levy_q > min(x$levy_q)]))))
  # 
  
  plot(con_temp[[1]]$data_mid, log10(con_temp[[1]]$data_p), cex = 0,  yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Density", main = "", ylim = c(log10(y_min), log10(y_max)))
  
  
  #mtext(side = 1, text=paste0(x_lab), line = 1, cex = 1)
  #mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)
  
  if( x_lab == "LP (log)"){
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  1)
    
  }else if(x_lab == "LP_Growth"){
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  1)
  }else{
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),-2),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),-2), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab," (", euro(1000) ,"/Employee)", sep = ""), line = 1.2, cex =  1)
    
  }  #axis(side = 2, lwd = 0.3, cex.axis = .9)
  
  axis_int <- seq(min(log10(con_temp[[1]]$data_p[con_temp[[1]]$data_p >0])), max(log10(con_temp[[1]]$data_p)), length.out = 4)
  labels <- sapply(round(axis_int,0),function(i) as.expression(bquote(10^ .(i))))
  axis(2, at = axis_int, labels = labels, lwd = 0.3, cex.axis = .9)
  
  
  c_ind_all <- c()
  
  for (y in 1:length(con_temp)) {
    c_ind <- which(c_names %in% c_uni[y])
    points(con_temp[[y]]$data_mid, log10(con_temp[[y]]$data_p), pch = 14+y, cex = 1, col = color_ind[c_ind])
    
    lines(con_temp[[y]]$data_mid, log10(con_temp[[y]]$levy_q), col = color_ind[c_ind], lwd = 1.5, lty = 1) # Levy fit
    
    c_ind_all[y] <- c_ind
  }
  
  
  legend("topright",  legend = size_names_long, pch = 15:18, col = color_ind[c_ind_all], bty = "n", xpd = NA, cex = 1, ncol = 1)

  
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


### LP 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Non-Log")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Size_Levy_Fit_France", title = "", dat_list = LP_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Size_Levy_Fit_Germany", title = "", dat_list = LP_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Size_Levy_Fit_Italy", title = "", dat_list = LP_size_Levy_list_boot, c_names = size_names_long, x_lab = "LP", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Size_Levy_Fit_Spain", title = "", dat_list = LP_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Size_Levy_Fit_UK", title = "", dat_list = LP_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Non-Log (Non-Neg)")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_non_neg_Levy_Fit_France", title = "", dat_list = LP_size_non_neg_Levy_list_boot, c_names = size_names_long,  x_lab = "LP ( > 0)", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_non_neg_Levy_Fit_Germany", title = "", dat_list = LP_size_non_neg_Levy_list_boot, c_names = size_names_long,  x_lab = "LP ( > 0)", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_non_neg_Levy_Fit_Italy", title = "", dat_list = LP_size_non_neg_Levy_list_boot, c_names = size_names_long, x_lab = "LP ( > 0)", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_non_neg_Levy_Fit_Spain", title = "", dat_list = LP_size_non_neg_Levy_list_boot, c_names = size_names_long,  x_lab = "LP ( > 0)", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_non_neg_Levy_Fit_UK", title = "", dat_list = LP_size_non_neg_Levy_list_boot, c_names = size_names_long,  x_lab = "LP ( > 0)",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Log")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_log_Levy_Fit_France", title = "", dat_list = LP_size_log_Levy_list_boot, c_names = size_names_long,  x_lab = "LP (log)", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_log_Levy_Fit_Germany", title = "", dat_list = LP_size_log_Levy_list_boot, c_names = size_names_long,  x_lab = "LP (log)", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_log_Levy_Fit_Italy", title = "", dat_list = LP_size_log_Levy_list_boot, c_names = size_names_long, x_lab = "LP (log)", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_log_Levy_Fit_Spain", title = "", dat_list = LP_size_log_Levy_list_boot, c_names = size_names_long,  x_lab = "LP (log)", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_size_log_Levy_Fit_UK", title = "", dat_list = LP_size_log_Levy_list_boot, c_names = size_names_long,  x_lab = "LP (log)",con_ind = "United Kingdom")

### LP_Change and LP Growth (log and non-log)
# Change 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Change/LP Change")
fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_Size_Levy_Fit_France", title = "", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_Size_Levy_Fit_Germany", title = "", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_Size_Levy_Fit_Italy", title = "", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_Size_Levy_Fit_Spain", title = "", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_Size_Levy_Fit_UK", title = "", dat_list = LP_Change_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Change Non-Neg
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Change/LP Change (Non-Neg)")
fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_non_neg_Size_Levy_Fit_France", title = "", dat_list = LP_Change_non_neg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_non_neg_Size_Levy_Fit_Germany", title = "", dat_list = LP_Change_non_neg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_non_neg_Size_Levy_Fit_Italy", title = "", dat_list = LP_Change_non_neg_size_Levy_list_boot, c_names = size_names_long, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_non_neg_Size_Levy_Fit_Spain", title = "", dat_list = LP_Change_non_neg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_Change_non_neg_Size_Levy_Fit_UK", title = "", dat_list = LP_Change_non_neg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Growth
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Growth/Non-Log")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_g_Size_Levy_Fit_France", title = "", dat_list = LP_g_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP Growth", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_g_Size_Levy_Fit_Germany", title = "", dat_list = LP_g_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_g_Size_Levy_Fit_Italy", title = "", dat_list = LP_g_size_Levy_list_boot, c_names = size_names_long, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_g_Size_Levy_Fit_Spain", title = "", dat_list = LP_g_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_g_Size_Levy_Fit_UK", title = "", dat_list = LP_g_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth",con_ind = "United Kingdom")

# Growth (Log)
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Growth/Log")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_lg_Size_Levy_Fit_France", title = "", dat_list = LP_lg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth", con_ind = "France")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_lg_Size_Levy_Fit_Germany", title = "", dat_list = LP_lg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_lg_Size_Levy_Fit_Italy", title = "", dat_list = LP_lg_size_Levy_list_boot, c_names = size_names_long, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_lg_Size_Levy_Fit_Spain", title = "", dat_list = LP_lg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_size(pdf_name = "Figure_LP_lg_Size_Levy_Fit_UK", title = "", dat_list = LP_lg_size_Levy_list_boot, c_names = size_names_long,  x_lab = "LP_Growth",con_ind = "United Kingdom")


## Industry class
# LP conditional on sector

fun_plot_levy_fit_ind <- function(pdf_name, title, dat_list, c_names, x_lab,  con_ind) {
  color_ind <- c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 8, name = "Set3"), "grey", "blue", "green", "red")[1:length(c_names)] # color index 
  
  
  cairo_pdf(paste(pdf_name, ".pdf", sep = ""),  height = 6, width = 6)
  par(mfrow = c(1, 1), mar = c(3, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0), las = 1)
  
  k <- which(country_names%in%con_ind)

  con_temp <- dat_list[[1]][[k]]
  
  c_uni <- dat_list[[2]][[k]]
  
  get_this <- which(c_uni%in%c("AGRICULTURE, FORESTRY AND FISHING", "MANUFACTURING", "INFORMATION AND COMMUNICATION", "FINANCIAL AND INSURANCE ACTIVITIES"))
  
  
  con_temp <- con_temp[get_this]
  c_uni <- c_uni[get_this]
  
  levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0)
  
  if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
    ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
    con_temp <- con_temp[-ok_ind]
    c_uni <- c_uni[-ok_ind]
    levy_soofi <- levy_soofi[-ok_ind]
  }
  
  
  # 
  y_max <- max(unlist(lapply(con_temp, function(x) max(x$data_p, x$levy_q))))
  y_min <- min(unlist(lapply(con_temp, function(x) min(x$data_p[x$data_p > min(x$data_p)], x$levy_q[x$levy_q > min(x$levy_q)]))))
  # 
  
  plot(con_temp[[1]]$data_mid, log10(con_temp[[1]]$data_p), cex = 0,  yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Density", main = "", ylim = c(log10(y_min), log10(y_max)))
  
  
 # mtext(side = 1, text=paste0(x_lab), line = 1, cex = 1)
  #mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)
  
  if( x_lab == "LP (log)"){
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  1)
    
    
  }else if(x_lab == "LP_Growth"){
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),0), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab, sep = ""), line = 1.2, cex =  1)
    
  }else{
    axis(1, at = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),-2),  labels = round(seq(min(con_temp[[1]]$data_mid), max(con_temp[[1]]$data_mid), length.out = 6),-2), lwd = 0.3, cex.axis = .9)
    mtext(side = 1, text= paste(x_lab," (", euro(1000) ,"/Employee)", sep = ""), line = 1.2, cex =  1)
    
    
  }  #axis(side = 2, lwd = 0.3, cex.axis = .9)
  #axis(side = 2, lwd = 0.3, cex.axis = .9)
  
  axis_int <- seq(min(log10(con_temp[[1]]$data_p[con_temp[[1]]$data_p >0])), max(log10(con_temp[[1]]$data_p)), length.out = 4)
  labels <- sapply(round(axis_int,0),function(i) as.expression(bquote(10^ .(i))))
  axis(2, at = axis_int, labels = labels, lwd = 0.3, cex.axis = .9)
  
  
  c_ind_all <- c()
  
  for (y in 1:length(con_temp)) {
    c_ind <- which(c_names %in% c_uni[y])
    points(con_temp[[y]]$data_mid, log10(con_temp[[y]]$data_p), pch = 14+y, cex = 1, col = color_ind[y])
    
    lines(con_temp[[y]]$data_mid, log10(con_temp[[y]]$levy_q), col = color_ind[y], lwd = 1.5, lty = 1) # Levy fit
    
    c_ind_all[y] <- c_ind
  }
  
  legend("topright",  legend = c("Agriculture","Manufacturing","Information", "Finance"), pch = 15:18, col = color_ind[1:4], bty = "n", xpd = NA, cex = 1, ncol = 1)
  
  
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}


setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Ind_Levy_Fit_France", title = "", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Ind_Levy_Fit_Germany", title = "", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Ind_Levy_Fit_Italy", title = "", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Ind_Levy_Fit_Spain", title = "", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Ind_Levy_Fit_UK", title = "", dat_list = LP_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log (Non-Neg)")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_non_neg_Levy_Fit_France", title = "", dat_list = LP_ind_non_neg_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP ( > 0)", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_non_neg_Levy_Fit_Germany", title = "", dat_list = LP_ind_non_neg_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP ( > 0)", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_non_neg_Levy_Fit_Italy", title = "", dat_list = LP_ind_non_neg_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP ( > 0)", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_non_neg_Levy_Fit_Spain", title = "", dat_list = LP_ind_non_neg_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP ( > 0)", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_non_neg_Levy_Fit_UK", title = "", dat_list = LP_ind_non_neg_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP ( > 0)",con_ind = "United Kingdom")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Log")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_log_Levy_Fit_France", title = "", dat_list = LP_ind_log_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP (log)", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_log_Levy_Fit_Germany", title = "", dat_list = LP_ind_log_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP (log)", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_log_Levy_Fit_Italy", title = "", dat_list = LP_ind_log_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP (log)", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_log_Levy_Fit_Spain", title = "", dat_list = LP_ind_log_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP (log)", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_ind_log_Levy_Fit_UK", title = "", dat_list = LP_ind_log_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP (log)",con_ind = "United Kingdom")

### LP_Change and LP Growth (log and non-log)
# Change 
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change")
fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_Ind_Levy_Fit_France", title = "", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_Ind_Levy_Fit_Germany", title = "", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_Ind_Levy_Fit_Italy", title = "", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_Ind_Levy_Fit_Spain", title = "", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_Ind_Levy_Fit_UK", title = "", dat_list = LP_Change_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Change Non-Neg
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change (Non-Neg)")
fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_non_neg_Ind_Levy_Fit_France", title = "", dat_list = LP_Change_non_neg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP Change", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_non_neg_Ind_Levy_Fit_Germany", title = "", dat_list = LP_Change_non_neg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_non_neg_Ind_Levy_Fit_Italy", title = "", dat_list = LP_Change_non_neg_ind_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP_Change", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_non_neg_Ind_Levy_Fit_Spain", title = "", dat_list = LP_Change_non_neg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_Change_non_neg_Ind_Levy_Fit_UK", title = "", dat_list = LP_Change_non_neg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Change",con_ind = "United Kingdom")

# Growth
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Non-Log")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_g_Ind_Levy_Fit_France", title = "", dat_list = LP_g_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP Growth", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_g_Ind_Levy_Fit_Germany", title = "", dat_list = LP_g_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_g_Ind_Levy_Fit_Italy", title = "", dat_list = LP_g_ind_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_g_Ind_Levy_Fit_Spain", title = "", dat_list = LP_g_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_g_Ind_Levy_Fit_UK", title = "", dat_list = LP_g_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")

# Growth (Log)
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Log")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_lg_Ind_Levy_Fit_France", title = "", dat_list = LP_lg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth", con_ind = "France")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_lg_Ind_Levy_Fit_Germany", title = "", dat_list = LP_lg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth", con_ind = "Germany")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_lg_Ind_Levy_Fit_Italy", title = "", dat_list = LP_lg_ind_Levy_list_boot, c_names = ind_name_table$ind_names, x_lab = "LP_Growth", con_ind = "Italy")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_lg_Ind_Levy_Fit_Spain", title = "", dat_list = LP_lg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth", con_ind = "Spain")

fun_plot_levy_fit_ind(pdf_name = "Figure_LP_lg_Ind_Levy_Fit_UK", title = "", dat_list = LP_lg_ind_Levy_list_boot, c_names = ind_name_table$ind_names,  x_lab = "LP_Growth",con_ind = "United Kingdom")





###

############ 2. Plots for Estimated parameters ############


## 2.1 function (for five countries )

fun_levy_par <- function(pdf_name, title, dat_list, x_value, x_value_name, x_lab, leg_pos, leg_ind) { # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
 
  colores_this <- c(brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  

  five_ind <- which(country_names %in% country_names_five) # index for the top five countries


  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  for (k in five_ind) {
    c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(dat_list[[1]][[k]], function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }

  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()

  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
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
  plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1., cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
 # mtext(side = 1, text=paste0(x_lab), line = 1, cex = 0.7)

  axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85) # x-axis
  axis(side = 2, lwd = 0.3, cex.axis = 0.85) # y-axis


  for (k in five_ind) {
    points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[which(five_ind%in%k)], cex = 0.9, pch = 20 + which(five_ind%in%k), bg = colores_this[which(five_ind%in%k)], type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample

    polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]])) # error bar
    polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))

    polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[which(five_ind%in%k)], alpha.f = 0.4), border = NA)
  }

  for (l in 2:4) { # 2nd to 4th parameters
    plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1., cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])))) # Empty plot for the first parameter: x_value is the numeric value of all classes

    axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85)
    axis(side = 2, lwd = 0.3, cex.axis = 0.85)

    for (k in five_ind) {
      points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[which(five_ind%in%k)], cex = 0.9, pch = 20 + which(five_ind%in%k), bg = colores_this[which(five_ind%in%k)], type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample

      # error bars
      polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]]))
      polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))

      polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[which(five_ind%in%k)], alpha.f = 0.4), border = NA)
    }
    

  }
  if(leg_ind == 1){
    legend("topright", inset=c(1.3,-1.65), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[1:5], pch = c(21:26), pt.bg = colores_this[1:5], bty = "n", xpd = NA, cex = 1.1, ncol = 1, horiz = T, pt.cex = 1.8)
    
  }
  # legend outside the main plots
  
  mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.2)
  dev.off()
}


# fun_levy_par_alpha <- function(pdf_name, title, dat_list, x_value, x_value_name, x_lab, leg_pos, leg_ind) { # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
#   
#   colores_this <- c(brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
#   
#   
#   
#   five_ind <- which(country_names %in% country_names_five) # index for the top five countries
#   
#   
#   c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
#   for (k in five_ind) {
#     c_uni_list[[k]] <- dat_list[[3]][[k]] # the numeric value of the unique class
#     levy_par[[k]] <- lapply(dat_list[[1]][[k]], function(x) x$levy_para) # the corresponding levy parameters for each class
#     levy_par_sd[[k]] <- lapply(dat_list[[1]][[k]], function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
#   }
#   
#   par_a <- list(); par_a_sd <- list()
#   par_b <- list(); par_b_sd <- list()
#   par_g <- list(); par_g_sd <- list()
#   par_d <- list(); par_d_sd <- list()
#   
#   for (k in five_ind) {
#     par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
#     par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
#     par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
#     par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
#     par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
#     par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
#     par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
#     par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
#   }
#   
#   
#   par_list <- list(par_a, par_b, par_g, par_d)
#   par_sd_list <- list(par_a_sd, par_b_sd, par_g_sd, par_d_sd)
#   # parameter names
#   par_name_list <- list(
#     expression(paste(alpha, ": TFP Growth")),
#     expression(paste(beta, ": skewness")),
#     expression(paste(gamma, ": scale")),
#     expression(paste(delta, ": location"))
#   )
#   ylab_list <- list(expression(paste(alpha)), expression(paste(beta)), expression(paste(gamma)), expression(paste(delta))) ## for the y_lab
#   
#   # lw_list <- list(lw_par_a, lw_par_b, lw_par_g, lw_par_d)
#   
#   
#   ###
#   pdf(paste(pdf_name, ".pdf", sep = ""), height = 5, width = 7)
#   par(mfrow = c(1, 1), mar = c(3, 2.2, 1.3, 1), mgp = c(1.4, .3, 0), tck = -.01, oma = c(0, 0, 1, 0), las = 1)
#   
#   l <- 1 # first parameter
#   plot(x_value, x_value, yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.6, xlab = x_lab, ylab = ylab_list[[l]], cex.lab = 1., cex = 0, ylim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
#   # mtext(side = 1, text=paste0(x_lab), line = 1, cex = 0.7)
#   
#   axis(side = 1, at = x_value, label = x_value_name, lwd = 0.3, cex.axis = 0.85) # x-axis
#   axis(side = 2, lwd = 0.3, cex.axis = 0.85) # y-axis
#   
#   
#   for (k in five_ind) {
#     points(c_uni_list[[k]], par_list[[l]][[k]], col = colores_this[which(five_ind%in%k)], cex = 0.9, pch = 20, type = "b", lty = 1, lwd = 1.3) # c_uni_list is the numeric value of the unique classes in the subsample
#     
#     polygon.x <- c(c_uni_list[[k]], rev(c_uni_list[[k]])) # error bar
#     polygon.y <- c(c(par_list[[l]][[k]] - par_sd_list[[l]][[k]]), rev(c(par_list[[l]][[k]] + par_sd_list[[l]][[k]])))
#     
#     polygon(x = polygon.x, y = polygon.y, col = adjustcolor(colores_this[which(five_ind%in%k)], alpha.f = 0.4), border = NA)
#   }
# 
#   if(leg_ind == 1){
#     legend("topright", legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[1:5], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.1, ncol = 1, horiz = T, pt.cex = 2.4)
#     
#   }
#   # legend outside the main plots
#   
#   #mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.2)
#   dev.off()
# }
## 2.1.2 plot

## Estimated Parameter for Year class
# LP
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year", title = "Labor Productivity", dat_list = LP_year_Levy_list_boot, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft", leg_ind = 1)

# uncut
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year_uncut", title = "Labor Productivity", dat_list = LP_year_Levy_list_boot_uncut, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft", leg_ind = 1)


setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Non-Log (Non-Neg)")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year_non_neg", title = "Labor Productivity", dat_list = LP_year_non_neg_Levy_list_boot, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP/Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Year_log", title = "Labor Productivity (Log)", dat_list = LP_year_log_Levy_list_boot, x_value = 1:10, x_value_name = year_names, x_lab = "Year", leg_pos = "topleft", leg_ind = 1)


# LP Change and Growth

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Change_Year", title = "Labor Productivity Change", dat_list = LP_Change_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Change/LP Change (Non-Neg)")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Change_Year_non_neg", title = "Labor Productivity Change", dat_list = LP_Change_non_neg_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Non-Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_g_Year", title = "Labor Productivity Growth", dat_list = LP_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Year)/LP Growth/Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_lg_Year", title = "Labor Productivity Growth", dat_list = LP_lg_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

#fun_levy_par(pdf_name = "Figure_Levy_Para_TFP_g_Year_neg_ind", title = "TFP Growth", dat_list = TFP_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)


# LP_Change alpha
# fun_levy_par_alpha(pdf_name = "Figure_Levy_alpha_LP_g_Year", title = "Labor Productivity Growth", dat_list = LP_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)
# 
# fun_levy_par_alpha(pdf_name = "Figure_Levy_alpha_TFP_g_Year", title = "TFP Growth", dat_list = TFP_g_year_Levy_list_boot, x_value = 2:10, x_value_name = year_names[-c(1)], x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)


## Estimated Parameter for Size class

# LP
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Non-Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Size", title = "Labor Productivity", dat_list = LP_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "topright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Non-Log (Non-Neg)")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Size_non_neg", title = "Labor Productivity", dat_list = LP_size_non_neg_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "topright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP/Log")
fun_levy_par(pdf_name = "Figure_Levy_log_Para_LP_Size", title = "Labor Productivity", dat_list = LP_size_log_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "topright", leg_ind = 1)

# LP_Change
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Change/LP Change")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_Change_Size", title = "Labor Productivity Change", dat_list = LP_Change_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "bottomright", leg_ind = 1)


setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Change/LP Change (Non-Neg)")
fun_levy_par(pdf_name = "Figure_Levy_non_neg_Para_LP_Change_Size", title = "Labor Productivity Change", dat_list = LP_Change_non_neg_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Size", leg_pos = "bottomright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Growth/Non-Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_g_Size", title = "Labor Productivity Growth", dat_list = LP_g_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Size)/LP Growth/Log")
fun_levy_par(pdf_name = "Figure_Levy_Para_LP_lg_Size", title = "Labor Productivity Growth", dat_list = LP_lg_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)

#fun_levy_par(pdf_name = "Figure_Levy_Para_TFP_g_Size", title = "TFP Growth", dat_list = TFP_g_size_Levy_list_boot, x_value = c(1:4), x_value_name = size_names, x_lab = "Year", leg_pos = "bottomright", leg_ind = 1)


############ 3. Productivity by ind chart ############

# the code is very similar to the above one except that the x and y axes are switched
fun_levy_par_ind <- function(pdf_name, title, dat_list, y_value, y_value_name, leg_pos) { # # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  colores_this <- c(brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  
  
  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  
  for (k in five_ind) {
    
    con_temp <- dat_list[[1]][[k]] # the levy parameters for each class
    c_uni_num <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0) # soofi ID
    
    # remove those distribution whose Soofi ID is below 50
    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni_num <- c_uni_num[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    c_uni_list[[k]] <- c_uni_num # the numeric value of the unique class
    levy_par[[k]] <- lapply(con_temp, function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(con_temp, function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }
  
  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()
  
  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
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
  comp_par <- function(c_name, x_ind, x_val, sd_val) {
    #this_ind <- x_value_name_full [which(c(1:19) %in% x_ind)]
    this_ind <- x_ind
    ok <- data.frame(Country = rep(c_name, length(this_ind)), Ind = this_ind, Value = x_val, Sd = sd_val)
    return(ok)
  }
  
  
  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 7, width = 7.)
  par(mfrow = c(2, 2), mar = c(3, 2.7, 1, 1), mgp = c(1.3, .3, 0), tck = -.01, oma = c(0, 0, 2, 0))
  
  l <- 1 # first parameter
  
  ok_frame <- c() # a long format for the five countries
  for (k in five_ind) {
    ok <- comp_par(country_names[k], x_ind = c_uni_list[[k]], x_val = par_list[[l]][[k]], sd_val = par_sd_list[[l]][[k]])
    
    ok_frame <- rbind(ok_frame, ok)
  }
  
  ok_frame_re <- ok_frame %>%
    group_by(Ind) %>%
    summarise(ind_mean = mean(Value)) %>%
    arrange(ind_mean)
  
  #y_ind_order <-  ok_frame_re[[1]]
  
  
  #y_value_name_2 <- y_value_name[y_ind_order]
  
  
  plot(1:length(y_value_name), 1:length(y_value_name), yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
  axis(side = 2, at = rev(1:length(y_value_name)), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
  axis(side = 1, lwd = 0.3, cex.axis = 0.85)
  
  abline(h = 1:length(y_value_name), lty = 3, col = "grey")
  
  #Add vertical grid
  abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")
  
  for (k in five_ind) {
    
    #k_order <- y_ind_order[which(y_ind_order%in%c_uni_list[[k]])]

    #y_order <- which(y_ind_order%in%k_order)
          # note taht y axis needs to be reversed
    for(z in 1:length(par_list[[l]][[k]])){
      lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(length(y_value_name) + 1 -c_uni_list[[k]][z], length(y_value_name) + 1-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[which(five_ind%in%k)])
    }
    
    points(par_list[[l]][[k]], length(y_value_name) + 1-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1., pch = 20 + which(five_ind%in%k),  lty = 2, bg = colores_this[which(five_ind%in%k)],lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
    
    }
    
    #points(par_list[[l]][[k]], 20-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
    
  
  #legend(leg_pos, legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), ncol = 1, cex = 1, bty = "n")

  
  for (l in 2:4) { # 2nd to 4th parameters
    
    
    #y_ind_order <-   ok_frame_re[[1]]
    
    #y_value_name_2 <- y_value_name[y_ind_order]
    
    plot(1:length(y_value_name), 1:length(y_value_name), yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
    axis(side = 2, at = rev(1:length(y_value_name)), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
    axis(side = 1, lwd = 0.3, cex.axis = 0.85)
    
    abline(h = 1:length(y_value_name), lty = 3, col = "grey")
    
    #Add vertical grid
    abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")
    
    
    for (k in five_ind) {
      
      #k_order <- y_ind_order[which(y_ind_order%in%c_uni_list[[k]])]
      
      #y_order <- which(y_ind_order%in%k_order)
      # note taht y axis needs to be reversed
      for(z in 1:length(par_list[[l]][[k]])){
        lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(length(y_value_name) + 1-c_uni_list[[k]][z], length(y_value_name) + 1-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[which(five_ind%in%k)])
      }
      
      points(par_list[[l]][[k]], length(y_value_name) + 1-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1., pch = 20 + which(five_ind%in%k),  lty = 2,bg = colores_this[which(five_ind%in%k)], lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
      
      }
      
      #points(par_list[[l]][[k]], 20-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
      

    legend("topright",  inset=c(1.2,-1.42), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[1:5], pch = c(21:26), pt.bg = colores_this[1:5], bty = "n", xpd = NA, cex = 1.1, ncol = 1, horiz = T, pt.cex = 2.)
    
  } 
  #legend("topright", inset=c(-0.35,0), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.4, ncol = 1)
  
  #mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}



# LP
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log")
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_Ind", title = "Labor Productivity", dat_list = LP_ind_Levy_list_boot,  y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log (Non-Neg)")
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_ind_non_neg", title = "Labor Productivity", dat_list = LP_ind_non_neg_Levy_list_boot, y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Log")
fun_levy_par_ind(pdf_name = "Figure_Levy_log_Para_LP_Ind", title = "Labor Productivity", dat_list = LP_ind_log_Levy_list_boot, y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

# LP_Change
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change")
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_Change_Ind", title = "Labor Productivity Change", dat_list = LP_Change_ind_Levy_list_boot,  y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change (Non-Neg)")
fun_levy_par_ind(pdf_name = "Figure_Levy_non_neg_Para_LP_Change_Ind", title = "Labor Productivity Change", dat_list = LP_Change_non_neg_ind_Levy_list_boot,  y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Non-Log")
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_g_Ind", title = "Labor Productivity Growth", dat_list = LP_g_ind_Levy_list_boot, y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")

setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Log")
fun_levy_par_ind(pdf_name = "Figure_Levy_Para_LP_lg_Ind", title = "Labor Productivity Growth", dat_list = LP_lg_ind_Levy_list_boot, y_value = c(1:19), y_value_name = ind_name_table$ind_names_short , leg_pos = "topleft")


############ 4. Productivity by ind chart (simplified) ############

## Industry class

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

fun_levy_par_ind_sim <- function(pdf_name, title, dat_list, y_value, y_value_name, leg_pos) { # # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  colores_this <- c(brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  
  
  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  
  for (k in five_ind) {
    
    con_temp <- dat_list[[1]][[k]] # the levy parameters for each class
    c_uni_num <- dat_list[[3]][[k]] # the numeric value of the unique class
    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0) # soofi ID
    
    # remove those distribution whose Soofi ID is below 50
    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 90) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni_num <- c_uni_num[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    c_uni_list[[k]] <- c_uni_num # the numeric value of the unique class
    levy_par[[k]] <- lapply(con_temp, function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(con_temp, function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }
  
  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()
  
  for (k in five_ind) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
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
  comp_par <- function(c_name, x_ind, x_val, sd_val) {
    #this_ind <- x_value_name_full [which(c(1:19) %in% x_ind)]
    this_ind <- x_ind
    ok <- data.frame(Country = rep(c_name, length(this_ind)), Ind = this_ind, Value = x_val, Sd = sd_val)
    return(ok)
  }
  
  
  ###
  pdf(paste(pdf_name, ".pdf", sep = ""), height = 4.5, width = 7.)
  par(mfrow = c(2, 2), mar = c(3, 2.7, 1, 1), mgp = c(1.3, .3, 0), tck = -.01, oma = c(0, 0, 2, 0))
  
  l <- 1 # first parameter
  
  ok_frame <- c() # a long format for the five countries
  for (k in five_ind) {
    ok <- comp_par(country_names[k], x_ind = c_uni_list[[k]], x_val = par_list[[l]][[k]], sd_val = par_sd_list[[l]][[k]])
    
    ok_frame <- rbind(ok_frame, ok)
  }
  
  #y_ind_order <-  ok_frame_re[[1]]
  
  
  #y_value_name_2 <- y_value_name[y_ind_order]
  y_value_name[which(y_value_name == "Arg")] <- "Agr"
  
  
  plot(1:length(y_value_name), 1:length(y_value_name), yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
  axis(side = 2, at = rev(1:length(y_value_name)), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
  axis(side = 1, lwd = 0.3, cex.axis = 0.85)
  
  abline(h = 1:length(y_value_name), lty = 3, col = "grey")
  
  #Add vertical grid
  abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")
  
  for (k in five_ind) {
    
    #k_order <- y_ind_order[which(y_ind_order%in%c_uni_list[[k]])]
    
    #y_order <- which(y_ind_order%in%k_order)
    # note taht y axis needs to be reversed
    for(z in 1:length(par_list[[l]][[k]])){
      lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(length(y_value_name) + 1 -c_uni_list[[k]][z], length(y_value_name) + 1-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[which(five_ind%in%k)])
    }
    
    points(par_list[[l]][[k]], length(y_value_name) + 1-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1., pch = 20 + which(five_ind%in%k),  lty = 2, bg = colores_this[which(five_ind%in%k)],lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
    
  }
  
  #points(par_list[[l]][[k]], 20-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
  
  
  #legend(leg_pos, legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), ncol = 1, cex = 1, bty = "n")
  
  
  for (l in 2:4) { # 2nd to 4th parameters
    
    
    #y_ind_order <-   ok_frame_re[[1]]
    
    #y_value_name_2 <- y_value_name[y_ind_order]
    
    plot(1:length(y_value_name), 1:length(y_value_name), yaxt = "n", xaxt = "n", main = par_name_list[[l]], cex.main = 1.2, xlab = ylab_list[[l]], ylab = "", cex.lab = 1.1, cex = 0, xlim = c(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0)) # Empty plot for the first parameter: x_value is the numeric value of all classes
    axis(side = 2, at = rev(1:length(y_value_name)), label = y_value_name, lwd = 0.3, cex.axis = 0.85, las = 1)
    axis(side = 1, lwd = 0.3, cex.axis = 0.85)
    
    abline(h = 1:length(y_value_name), lty = 3, col = "grey")
    
    #Add vertical grid
    abline(v = seq(min(unlist(par_list[[l]]) - unlist(par_sd_list[[l]])), max(unlist(par_list[[l]]) + unlist(par_sd_list[[l]])) * 1.0, length.out = 10),  lty = 3, col = "grey")
    
    
    for (k in five_ind) {
      
      #k_order <- y_ind_order[which(y_ind_order%in%c_uni_list[[k]])]
      
      #y_order <- which(y_ind_order%in%k_order)
      # note taht y axis needs to be reversed
      for(z in 1:length(par_list[[l]][[k]])){
        lines(c(c(par_list[[l]][[k]][z] - par_sd_list[[l]][[k]][z]), c(par_list[[l]][[k]][z] + par_sd_list[[l]][[k]][z])), c(length(y_value_name) + 1-c_uni_list[[k]][z], length(y_value_name) + 1-c_uni_list[[k]][z]),  lty = 1, lwd = 1.1, col = colores_this[which(five_ind%in%k)])
      }
      
      points(par_list[[l]][[k]], length(y_value_name) + 1-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1., pch = 20 + which(five_ind%in%k),  lty = 2, bg = colores_this[which(five_ind%in%k)],lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
      
    }
    
    #points(par_list[[l]][[k]], 20-c_uni_list[[k]], col = colores_this[which(five_ind%in%k)], cex = 1.2, pch = 20,  lty = 2, lwd = .8) # c_uni_list is the numeric value of the unique classes in the subsample
    
    
       
  } 
  legend("topright",  inset=c(1.4,-1.75), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[1:5], pch = c(21:26), bty = "n", xpd = NA, cex = 0.9, ncol = 1, horiz = T, pt.bg = colores_this[1:5], pt.cex = 1.7)
  
  #legend("topright", inset=c(-0.35,0), legend = c("FRA", "GER", "ITA", "SPA", "UK"), col = colores_this[five_ind], pch = rep(20, 5), bty = "n", xpd = NA, cex = 1.4, ncol = 1)
  
  #mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}



# LP
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_Para_LP_Ind_sim", title = "Labor Productivity", dat_list = LP_ind_Levy_list_boot_sim,  y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg), leg_pos = "topleft")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Non-Log (Non-Neg)")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_Para_LP_ind_non_neg_sim", title = "Labor Productivity", dat_list = LP_ind_non_neg_Levy_list_boot_sim, y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg) , leg_pos = "topleft")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP/Log")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_log_Para_LP_Ind_sim", title = "Labor Productivity", dat_list = LP_ind_log_Levy_list_boot_sim, y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg)  , leg_pos = "topleft")

# LP_Change
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_Para_LP_Change_Ind_sim", title = "Labor Productivity Change", dat_list = LP_Change_ind_Levy_list_boot_sim,  y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg), leg_pos = "topleft")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Change/LP Change (Non-Neg)")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_non_neg_Para_LP_Change_Ind_sim", title = "Labor Productivity Change", dat_list = LP_Change_non_neg_ind_Levy_list_boot_sim,  y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg), leg_pos = "topleft")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Non-Log")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_Para_LP_g_Ind_sim", title = "Labor Productivity Growth", dat_list = LP_g_ind_Levy_list_boot_sim, y_value = c(1:19), y_value_name = unique(ind_name_table$ind_agg), leg_pos = "topleft")

#
setwd("~/Desktop/Cleaned Rda/Productivity/Figure Final (Sector)/LP Growth/Log")
fun_levy_par_ind_sim(pdf_name = "Figure_Levy_Para_LP_lg_Ind_sim", title = "Labor Productivity Growth", dat_list = LP_lg_ind_Levy_list_boot_sim, y_value = c(1:19), y_value_name =unique(ind_name_table$ind_agg), leg_pos = "topleft")

######


fun_levy_par_ind_scatter <- function(pdf_name, title, dat_list, y_value, y_value_name, leg_pos) { # # this function has 7 arguments. 1) the name of the pdf file, 2) the title of the figure, 3) the data file that is generated from the fun_fit_levy function in "Fitting_Levy_Boot.R" script, 4) all class names, 5) unique class name for each subsample, 6) the name of x label, 7) the position of the legend
  
  colores_this <- c(brewer.pal(n = 7, name = "Set1"), brewer.pal(n = 7, name = "Set3"))
  
  
  five_ind <- which(country_names %in% country_names_five) # index for the top five countries
  
  
  
  c_uni_list <- list(); levy_par <- list(); levy_par_sd <- list()
  
  for (k in five_ind ) {
    
    con_temp <- dat_list[[1]][[k]] # the levy parameters for each class
    c_uni_num <- dat_list[[2]][[k]] # the numeric value of the unique class
    levy_soofi <- round(unlist(lapply(con_temp, function(x) x$levy_soofi)), 0) # soofi ID
    
    # remove those distribution whose Soofi ID is below 50
    if(sum(is.na(levy_soofi)) > 0 | sum(levy_soofi < 50) > 0 ){
      ok_ind <- which(is.na(levy_soofi) | levy_soofi< 50)
      con_temp <- con_temp[-ok_ind]
      c_uni_num <- c_uni_num[-ok_ind]
      levy_soofi <- levy_soofi[-ok_ind]
    }
    
    
    c_uni_list[[k]] <- c_uni_num # the numeric value of the unique class
    levy_par[[k]] <- lapply(con_temp, function(x) x$levy_para) # the corresponding levy parameters for each class
    levy_par_sd[[k]] <- lapply(con_temp, function(x) apply(x$est_levy_std_error$t, 2, sd)) # the corresponding standard deviation of levy parameters for each class
  }
  
  par_a <- list(); par_a_sd <- list()
  par_b <- list(); par_b_sd <- list()
  par_g <- list(); par_g_sd <- list()
  par_d <- list(); par_d_sd <- list()
  
  for (k in five_ind ) {
    par_a[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[1])) # parameter \alpha
    par_a_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[1])) # sd of parameter \alpha
    par_b[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[2])) # parameter \beta
    par_b_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[2]))
    par_g[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[3])) # parameter \gamma
    par_g_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[3]))
    par_d[[k]] <- unlist(lapply(levy_par[[k]], function(x) x[4])) # parameter \delta
    par_d_sd[[k]] <- unlist(lapply(levy_par_sd[[k]], function(x) x[4]))
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
  
  neg_cut <- 0.0025 # negative cut-off point
  pov_cut <- 0.9975 # positive cut-off point
  
  
  
  ok_frame <- c() # a long format for the five countries
  for (k in five_ind ) {
    ok <- data.frame(Country = rep(country_names[k], length(c_uni_list[[k]])), NACE_CAT = c_uni_list[[k]],  alpha = par_list[[1]][[k]], beta = par_list[[2]][[k]], gamma = par_list[[3]][[k]], delta = par_list[[4]][[k]], alpha_sd = par_sd_list[[1]][[k]], beta_sd = par_sd_list[[2]][[k]], gamma_sd = par_sd_list[[3]][[k]], delta_sd = par_sd_list[[4]][[k]])
    
    cap_inten <- All_list_Cleaned_cut[[k]] %>%
      select(IDNR, CP, LP, NACE_CAT, EMPL)%>%
      group_by(NACE_CAT) %>%
      filter(EMPL > 1) %>%
      mutate(Y = LP*EMPL,
             CAP = LP/CP) %>%
      mutate(K = Y/CP) %>%
      filter(K > 0) %>%
      #filter(!is.infinite(K)) %>%
      filter(K < quantile(K, .9975)) %>% # cut the tail
      
      mutate(CAP_log = log(CAP)) %>%
      group_by(NACE_CAT) %>%
      summarise(CAP_mean = mean(CAP, na.rm = T),
                CAP_medain = median(CAP, na.rm  = T),
                CAP_agg = sum(K, na.rm = T)/sum(EMPL, na.rm = T),
                CAP_log_mean = mean(CAP_log, na.rm = T),
                CAP_log_median = median(CAP_log, na.rm  = T),
                CAP_log_agg = log(CAP_agg))
    
    ok_frame[[k]] <- merge(ok, cap_inten, by = 'NACE_CAT')
    
  }
  
  ok_frame <- do.call("rbind", ok_frame)
  ###
  cairo_pdf(paste(pdf_name, ".pdf", sep = ""), height = 3.5, width = 8.5)
  par(mfrow = c(1, 2), mar = c(3, 2.7, 1, 1), mgp = c(1.3, .3, 0), tck = -.01, oma = c(0, 0, 1, 0))
  
  plot(ok_frame$CAP_agg/1000, ok_frame$alpha , pch=16, cex=1.3, yaxt = "n", xaxt = "n", xlab = "", ylab = expression(alpha), cex.main = .9,  main = "", col="rosybrown 2", cex.lab = 1, log = "x")
  box(lwd = 0.3)
  axis(side = 1, lwd = 0.3, cex.axis=0.8)
  axis(side = 2,lwd = 0.3, cex.axis=0.8)
  mtext(side = 1, text= paste("Capital Intensity"," (", euro(1000) ,"/Employee)", sep = ""), line = 1.8, cex =  1)
  
  # fit <- lm(ok_frame$alpha~ok_frame$CAP_agg)
  # x_seq <- seq(min(ok_frame$CAP_agg), max(ok_frame$CAP_agg), length.out = 1000)
  # y_pred <- fit$coefficients[1] + fit$coefficients[2]*  x_seq 
  # 
  # lines(x_seq,  y_pred)
  
  # abline(fit, lwd = 1.5) # regression line
  # r2 <- summary(fit)$adj.r.squared
  # mylabel = bquote(italic(R)^2 == .(format(r2, digits = 2)))
  # 
  # legend("topright", bty="n", legend = mylabel)
  # 
  
  plot(ok_frame$CAP_agg/1000, ok_frame$gamma , pch=16, cex=1.3, yaxt = "n", xaxt = "n", xlab = "", ylab = expression(gamma), cex.main = .9,  main = "", col="rosybrown 2", cex.lab = 1, log = "x")
  box(lwd = 0.3)
  axis(side = 1, lwd = 0.3, cex.axis=0.8)
  axis(side = 2,lwd = 0.3, cex.axis=0.8)
  mtext(side = 1, text= paste("Capital Intensity"," (", euro(1000) ,"/Employee)", sep = ""), line = 1.8, cex =  1)
  
  # fit <- lm(ok_frame$gamma~ok_frame$CAP_log_agg)
  # abline(fit, lwd = 1.5) # regression line
  # r2 <- summary(fit)$adj.r.squared
  # mylabel = bquote(italic(R)^2 == .(format(r2, digits = 2)))
  # 
  # legend("topright", bty="n", legend = mylabel)
  # 
  # 
  # 
  #mtext(paste(title), side = 3, line = 1, outer = TRUE, cex = 1.)
  dev.off()
}

fun_levy_par_ind_scatter(pdf_name = "Figure_Capital_Intesity", title = "Labor Productivity", dat_list = LP_ind_Levy_list_boot,  y_value = c(1:19), y_value_name = ind_name_table$ind_names_short, leg_pos = "topleft")


#### PP plot

neg_cut <- 0.0025 # negative cut-off point
pov_cut <- 0.9975 # positive cut-off point

levy_pp_plot <- function(dat, cond_ind, var_ind, c_names, cut_num, neg_cut, pov_cut, pdf_name, log_ind) { # the function takes 8 arguments: 1) data generated and cleaned in section 0.2, 2) the number of bins, 3) the index of the variable that is used for the conditional class, 4) the target variable name, 5) the name of class, 6) the minimum number of observations for each class,  7) the cutting point on the left tail, 8) the cutting point on the right tail
  result_list <- list()
  
  c_uni_list <- list()
  c_uni_num_list <- list()
  
   #length(dat)
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
    
    
    if (nrow(zz) == 0) {
      plot(1,1, yaxt = "n", xaxt = "n", main = "", cex.main = 1.2, xlab = "", ylab = "", cex = 0) 
      
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
      #length(c_uni_name)
      pdf(paste(pdf_name, "_",country_names[k], ".pdf", sep = ""), height = 10, width = 7.)
      par(mfrow = c(4, 3), mar = c(3, 2.7, 1, 1), mgp = c(1.3, .3, 0), tck = -.01, oma = c(0, 0, 2, 0))
      
      for (c in 1:length(c_uni_name)) {
        print(paste(length(c_uni), c))
        
        c_lp <- zz$Var[zz$Cond == c_uni_name[c]] # for each class
        
        if(log_ind == 1){
          c_lp <- log(c_lp[c_lp > 0])
        }else{
          c_lp <- c_lp
        }
        
        levy_result <- levy_fitting(dat_t = c_lp, bin_num = 100, include_bootstrap=FALSE) # Levy estimation
        
        est_levy <- levy_result$levy_para
        
        if(est_levy[1] == 0.5 | est_levy[2] == 0| est_levy[3] == 1 | est_levy[4] == 0){
          plot(1,1, yaxt = "n", xaxt = "n", xlab = "Stable Fit (CDF)", ylab = "Data (CDF)", main = paste(country_names[k],":",c_uni_name[c], sep = ""), cex = 0, cex.main = 0.9)
          
        } else{
          c_lp_sort <- sort(c_lp, decreasing = F)
          model_cdf <- pstable(c_lp_sort , alpha = est_levy[1], beta = est_levy[2], gamma = est_levy[3], delta = est_levy[4])
          data_cdf <- ecdf(c_lp)
          
          
          plot(model_cdf, data_cdf(c_lp_sort), cex = 0.2, yaxt = "n", xaxt = "n", xlab = "Stable Fit (CDF)", ylab = "Data (CDF)", main = paste(country_names[k],":",c_uni_name[c], sep = ""), cex.main = 0.9)
          
          axis(side = 2,lwd = 0.3, cex.axis = 1)
          axis(side = 1, lwd = 0.3, cex.axis = 1)
          
          abline(0,1, col = "red")
          
        }
           
        

      }
      
    dev.off()
    }
  }
  

 
}


levy_pp_plot(dat = All_list_Cleaned_cut, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut, pdf_name = "Figure_PP_Check_Levy_LP_Year", log_ind = 0)

levy_pp_plot(dat = All_list_Cleaned_cut, cond_ind = "Year", var_ind = "LP_diff", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut, pdf_name = "Figure_PP_Check_Levy_LP_Change_Year", log_ind = 0)

levy_pp_plot(dat = All_list_Cleaned_cut, cond_ind = "Year", var_ind = "LP", c_names = year_names, cut_num = 10000, neg_cut = neg_cut, pov_cut = pov_cut, pdf_name = "Figure_PP_Check_Levy_LP_log_Year", log_ind = 1)
