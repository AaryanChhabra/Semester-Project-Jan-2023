library(smatr)
setwd("~/This PC/NIH/meet 2/")


FWS <- read.csv("FWS_MINMOD_HaSherman.csv") # Storing the data in a variable FWS
#head(FWS)
#minmod$X <- NULL

head(FWS)
twovsone <- sma(AIRg~SI, log = "xy", data = FWS, slope.test = -1)
summary(twovsone) # Slope is not significantly different from -1

plot(twovsone, xlab = "SI(log)", ylab = "AIRg(log)", pch = 18, col = '#00008B', main = 'AIRg vs SI')

#Slope = coef(twovsone)[2]
Rsq = 0.3544825 # taken from summary
Pvalue = 3.5209e-12 # taken from summary

rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
#slope = bquote(slope == .(format(Slope, digits = 3)))
pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))

toshow <- c(rsq, pvalue)
legend('bottomleft', legend = toshow, bty = 'n')


fourvsthree <- sma(sigma~mSI, log = "xy", data = FWS, slope.test = -1)
summary(fourvsthree) # slope is not significantly different from -1
plot(fourvsthree, xlab = "mSI(log)", ylab = "sigma(log)", pch =18, col = "#00008B", main = "mSI vs sigma")


#Slope = coef(fourvsthree)[2]
Rsq = 0.148885 # taken from summary

Pvalue = 2.4349e-05 # taken from summary

rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
#slope = bquote(slope == .(format(Slope, digits = 3)))
pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))

toshow <- c("slope  = -0.96", rsq, pvalue)
legend('bottomleft', legend = toshow, bty = 'n')

