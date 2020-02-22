############ 0. Basic Set up ############
###  Required libraries
library(StableEstim)
library(RColorBrewer)
library(colorspace)


############ 1. Plot ############
col_8 <- brewer.pal(8,"Set2")
x_seq <- seq(-6, 6, by = 0.01)

pdf("PDF_Levy.pdf", height=5.7, width=8)
par(mfrow=c(2,2), mar=c(2, 3, 2, 2), mgp=c(1.,0,0), tck=-.01, oma=c(0,0,1,0), las= 1)

##
x_seq <- seq(-6, 6, by = 0.01)
plot(x_seq, dstable(x_seq, alpha = 0.5, beta = 0, gamma = 1, delta = 0), type = "l", xlab= expression(paste("x")), ylab = expression(paste("density")),
     main= expression(paste(beta," = 0, ", gamma , " = 1, ", delta, " = 0")) ,cex.main=1,  bty = "n", yaxt = "n", xaxt = "n", cex.axis=0.8, col = col_8[1], lwd = 2, lty = 1) 
axis(side = 1, lwd = 0.3, cex.axis=0.6)
axis(side = 2, lwd = 0.3, cex.axis=0.6)
#lines(x_seq, dstable(x_seq, alpha = 0.75, beta = 0, gamma = 1, delta = 0), col = col_8[2], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 0, gamma = 1, delta = 0), col = col_8[2], lwd = 2, lty = 2)
#lines(x_seq, dstable(x_seq, alpha = 1.5, beta = 0, gamma = 1, delta = 0), col = col_8[4], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 2, beta = 0, gamma = 1, delta = 0), col = col_8[3], lwd = 2, lty = 3)

legend("topright", legend = c(expression(paste(alpha,"=0.5")), expression(paste(alpha,"=1")), expression(paste(alpha,"=2"))), lty=c(1:3), col = col_8[1:3],cex = 0.85, box.lty=0)

##
x_seq <- seq(-6, 6, by = 0.01)
plot(x_seq, dstable(x_seq, alpha = 1, beta = -1, gamma = 1, delta = 0), type = "l", xlab= expression(paste("x")), ylab = expression(paste("density")),
     main= expression(paste(alpha," = 1, ", gamma , " = 1, ", delta, " = 0")) ,cex.main=1,  bty = "n", yaxt = "n", xaxt = "n", cex.axis=0.8, col = col_8[1], lwd = 2, ylim = c(0,0.35), lty = 1) 
axis(side = 1, lwd = 0.3, cex.axis=0.6)
axis(side = 2, lwd = 0.3, cex.axis=0.6)

#lines(x_seq, dstable(x_seq, alpha = 1, beta = -0.5, gamma = 1, delta = 0), col = col_8[2], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 0, gamma = 1, delta = 0), col = col_8[2], lwd = 2, lty = 2)
#lines(x_seq, dstable(x_seq, alpha = 1, beta = 0.5, gamma = 1, delta = 0), col = col_8[4], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 1, gamma = 1, delta = 0), col = col_8[3], lwd = 2, lty = 3)

legend("topright", legend = c(expression(paste(beta,"=-1")), expression(paste(beta,"=0")),expression(paste(beta,"=1"))), lty=c(1:3), col = col_8[1:3],cex = 0.85, box.lty=0)

###
x_seq <- seq(-6, 6, by = 0.01)
plot(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 0.5, delta = 0), type = "l", xlab= expression(paste("x")), ylab = expression(paste("density")),
     main= expression(paste(alpha," = 1, ", beta , " = 0, ", delta, " = 0")) ,cex.main=1,  bty = "n", yaxt = "n", xaxt = "n", cex.axis=0.8, col = col_8[1], lwd = 2, lty = 1) 
axis(side = 1, lwd = 0.3, cex.axis=0.6)
axis(side = 2, lwd = 0.3, cex.axis=0.6)

#lines(x_seq, dstable(x_seq, alpha = 1, beta = 0.5, gamma = .5, delta = 0), col = col_8[2], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 1, delta = 0), col = col_8[2], lwd = 2, lty = 2)
#lines(x_seq, dstable(x_seq, alpha = 1, beta = 0.5, gamma = 2, delta = 0), col = col_8[4], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = .0, gamma = 2, delta = 0), col = col_8[3], lwd = 2, lty = 3)

legend("topright", legend = c(expression(paste(gamma,"=0.5")),  expression(paste(gamma,"=1")), expression(paste(gamma,"=2"))), lty=c(1:3), col = col_8[1:3],cex = 0.85, box.lty=0)

###
x_seq <- seq(-10, 10, by = 0.01)
plot(x_seq, dstable(x_seq, alpha = 1, beta = 0, gamma = 1, delta = -3), type = "l", xlab= expression(paste("x")), ylab = expression(paste("density")), main= expression(paste(alpha," = 1, ", beta , " = 0, ", gamma, " = 1")) ,cex.main=1,  bty = "n", yaxt = "n", xaxt = "n", cex.axis=0.8, col = col_8[1], lwd = 2, lty = 1) 
axis(side = 1, lwd = 0.3, cex.axis=0.6)
axis(side = 2, lwd = 0.3, cex.axis=0.6)

#lines(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 1, delta = -1.5), col = col_8[2], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 1, delta = 0), col = col_8[2], lwd = 2, lty = 2)
#lines(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 1, delta = 1.5), col = col_8[4], lwd = 1.5)
lines(x_seq, dstable(x_seq, alpha = 1, beta = 0., gamma = 1, delta = 3), col = col_8[3], lwd = 2, lty = 3)

legend("topright", legend = c(expression(paste(delta,"=-3")), expression(paste(delta,"=0")),  expression(paste(delta,"=3"))), lty=c(1:3), col = col_8[1:3],cex = 0.85, box.lty=0)


#mtext(expression(paste("Stable Distributions")), side=3, line=1, outer=TRUE, cex=1.2)
dev.off()


theta <- c(1.2,1, 0)
pm <- 0

fake_d <- rstable(100000, alpha = 1.5, beta = 0, gamma = 1, delta = 0, pm = 0)




kk_par <- paraep4(kk, method= "A") #"DG"

