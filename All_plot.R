if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim,devtools)

devtools::load_all("fittinglevy")

load("All_list_Cleaned_cut.Rda") ## load the data file created from "Productivity_Analysis_Data.Rmd"
load("Labels.Rda")


k <- which(country_names =="Spain")

all_list <- list()
for(k in 1:15){
  all_list[[k]] <-  All_list_Cleaned_cut[[k]]%>%
    select(IDNR, Year, EMPL, LP, LP_diff) %>% # Firm ID, Year, Firm Size, Industry ID, Labor Produtivity, Labor Productivity Change, Employment
    na.omit() %>%
    filter(EMPL > 1) %>% # remove self-employed persons
    mutate(LP = LP/1000,
           LP_diff = LP_diff/1000) 
  
}

all_list_lp <- unlist(lapply(all_list, function(x) x$LP))
all_list_lp_change <- unlist(lapply(all_list, function(x) x$LP_diff))

neg_cut <- 0.005 # negative cut-off point
pov_cut <- 0.995 # positive cut-off point

LP_All <- subset(all_list_lp, all_list_lp > quantile(all_list_lp, neg_cut, na.rm = T) & all_list_lp < quantile(all_list_lp, pov_cut, na.rm = T))
LP_change_All <- subset(all_list_lp_change, all_list_lp_change > quantile(all_list_lp_change, neg_cut, na.rm = T) & all_list_lp_change < quantile(all_list_lp_change, pov_cut, na.rm = T))

LP_All_levy <- levy_fitting(dat_t = LP_All, bin_num = 100, include_bootstrap = FALSE)
LP_change_All_levy <- levy_fitting(dat_t = LP_change_All, bin_num = 100, include_bootstrap = FALSE)


##

pdf(paste("LP_All_All.pdf", sep = ""),  height = 5.5, width = 8.5)
par(mfrow = c(1, 1), mar = c(2, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0))


#plot(LP_All_levy$data_mid, LP_All_levy$data_p,log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Log Density", main = "Labor Productivity", pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex = 1.5)

#lines(LP_All_levy$data_mid, LP_All_levy$levy_q, col = "black", lwd = 2., lty = 1) # Levy fit

dd <- hist(LP_All, 300, freq = F,  col = rgb(0, 0, 0, alpha=0.3), yaxt = "n", xaxt = "n",  xlab = "", main = "Labor Productivity", )

#plot(LP_All_levy$data_mid, LP_All_levy$data_p, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Log Density", main = "Labor Productivity", pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex = 1.5)

lines(dd$mids, dstable(dd$mids, 
                       alpha = LP_All_levy$levy_para[1] ,
                       beta = LP_All_levy $levy_para[2],
                       gamma = LP_All_levy $levy_para[3],
                       delta = LP_All_levy $levy_para[4]), lwd = 2)

mtext(side = 1, text=paste0("Labor Productivity"), line = 2, cex = 1)
#mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)
axis(side = 1, lwd = 0.3, cex.axis = 0.9)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

# axis_int <- pretty(log(axTicks(2)) + 1, n = 5)
# labels <- sapply(axis_int,function(i) as.expression(bquote(10^ .(i))))
# axis(2, at = exp(axis_int), labels = labels, lwd = 0.3, cex.axis = .9, las = 1)

par(new=TRUE)
par(oma=c(1,1,1,1))
par(fig=c(.5, 0.95, .4, 0.85), new = TRUE)
par(mar=c(2,3,1,1), tck=-.03, mgp=c(1.5,.3,0))

q_seq <- c(seq(0.005,0.995,by=0.005))
a <- quantile(LP_All, q_seq)
z <- qstable(q_seq, 
             alpha = LP_All_levy$levy_para[1] ,
             beta = LP_All_levy $levy_para[2],
             gamma = LP_All_levy $levy_para[3],
             delta = LP_All_levy $levy_para[4])


plot(z, a, yaxt = "n", xaxt = "n", cex.main = 1.2, ylab = "Data Quantile", xlab = "", main = expression("Q-Q Plot"), pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex.lab = 1, cex.main = 1.2, ylim = c(-100,400))

abline(0,1, col = "black", lwd = 2., lty = 1) # Levy fit

mtext(side = 1,  text=paste0("L\uE9vy Stable Quantile"), line = 1, cex = 1)

axis(side = 1,  lwd = 0.3, cex.axis = 0.9)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

dev.off()


###


pdf(paste("LP_change_All_All.pdf", sep = ""),  height = 5.5, width = 8.5)
par(mfrow = c(1, 1), mar = c(2, 3.2, 1.5, 1), mgp = c(1.8, .2, 0), tck = -.01, oma = c(0, 0, 1, 0))


#plot(LP_change_All_levy$data_mid, LP_change_All_levy$data_p,log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Log Density", main = "Labor Productivity", pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex = 1.5)

#lines(LP_change_All_levy$data_mid, LP_change_All_levy$levy_q, col = "black", lwd = 2., lty = 1) # Levy fit

dd <- hist(LP_change_All, 300, freq = F, xlim = c(-100, 200), col = rgb(0, 0, 0, alpha=0.3), yaxt = "n", xaxt = "n",  xlab = "", main = "Labor Productivity Change", )

#plot(LP_change_All_levy$data_mid, LP_change_All_levy$data_p, log = "y", yaxt = "n", xaxt = "n", cex.main = 1.2, xlab = "", ylab = "Log Density", main = "Labor Productivity", pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex = 1.5)

lines(dd$mids, dstable(dd$mids, 
                       alpha = LP_change_All_levy$levy_para[1] ,
                       beta = LP_change_All_levy $levy_para[2],
                       gamma = LP_change_All_levy $levy_para[3],
                       delta = LP_change_All_levy $levy_para[4]), lwd = 2)

mtext(side = 1, text=paste0("Labor Productivity Change"), line = 2, cex = 1)
#mtext(side = 2, text=paste0("Log Density"), line = 2, las = 1, cex = 0.7)
axis(side = 1, lwd = 0.3, cex.axis = 0.9)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

# axis_int <- pretty(log(axTicks(2)) + 1, n = 5)
# labels <- sapply(axis_int,function(i) as.expression(bquote(10^ .(i))))
# axis(2, at = exp(axis_int), labels = labels, lwd = 0.3, cex.axis = .9, las = 1)

par(new=TRUE)
par(oma=c(1,1,1,1))
par(fig=c(.5, 0.95, .4, 0.85), new = TRUE)
par(mar=c(2,3,1,1), tck=-.03, mgp=c(1.5,.3,0))

q_seq <- c(seq(0.005,0.995,by=0.01))
a <- quantile(LP_change_All, q_seq)
z <- qstable(q_seq, 
             alpha = LP_change_All_levy$levy_para[1] ,
             beta = LP_change_All_levy $levy_para[2],
             gamma = LP_change_All_levy $levy_para[3],
             delta = LP_change_All_levy $levy_para[4])


plot(z, a, yaxt = "n", xaxt = "n", cex.main = 1.2, ylab = "Data Quantile", xlab = "", main = expression("Q-Q Plot"), pch = 16, col = rgb(0, 0, 0, alpha=0.3), cex.lab = 1, cex.main = 1.2, ylim = c(-300, 300))

abline(0,1, col = "black", lwd = 2., lty = 1) # Levy fit

mtext(side = 1,  text=paste0("L\uE9vy Stable Quantile"), line = 1, cex = 1)

axis(side = 1,  lwd = 0.3, cex.axis = 0.9)
axis(side = 2, lwd = 0.3, cex.axis = .9, las = 1)

dev.off()


