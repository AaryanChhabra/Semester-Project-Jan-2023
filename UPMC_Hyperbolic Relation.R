setwd("~/This PC/NIH/meet 3/UPMC")
FWS <- read.csv("UPMC_Combined(1).csv")

head(FWS)


twovsone <- sma(sigma~si, log = "xy", data = FWS, slope.test = -1)
summary(twovsone) # Slope is not significantly different from -1

plot(twovsone, xlab = "mSI(log)", ylab = "sigma(log)", pch =18, col = "#00008B", main = "mSI vs sigma")
Rsq = 0.03 # taken from summary
Pvalue = 3.5209e-12 # taken from summary

rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
#slope = bquote(slope == .(format(Slope, digits = 3)))
pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))
toshow <- c("slope  = -1.05", rsq, pvalue)
legend('bottomleft', legend = toshow, bty = 'n')

# Insulin sensitivity:

FWS$HOMA <- 405/(FWS$oins0 * FWS$oglu0)


mean_I <- (FWS$oins0 + (2*FWS$oins30) + (2*FWS$oins60) + (2*FWS$oins90) + FWS$oins120)/8
mean_G <- (FWS$oglu0 + (2*FWS$oglu30) + (2*FWS$oglu60) + (2*FWS$oglu90) + FWS$oglu120)/8

FWS$Mat_i <- 10000/sqrt((FWS$oins0 * FWS$oglu0 * (mean_I) * (mean_G)))

# Beta-cell function:

FWS$IGI <- (FWS$oins30- FWS$oins0)/(FWS$oglu30 - FWS$oglu0) #(I30 – I0)/(G30 – G0)
FWS$HOMA_B <- (360*FWS$oins0)/(FWS$oglu0 - 63)  #360*I0/(G0 – 63)


# To do the SMA - 4 combinations possible
library(smatr)

IGIvsHOMA <- sma(IGI~HOMA, log = "xy", data = FWS, slope.test = -1)
IGIvsMat <- sma(IGI~Mat_i, log = "xy", data = FWS, slope.test = -1)

HOMA_bvsHOMA <- sma(HOMA_B~HOMA, log = "xy", data = FWS, slope.test = -1)
HOMA_bvsMat <- sma(HOMA_B~Mat_i, log = "xy", data = FWS, slope.test = -1)

###

to_plot <- function(XvsY){
  Slope = coef(XvsY)[2]
  Rsq = XvsY$r2
  Pvalue = XvsY$pval
  rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
  slope = bquote(slope == .(format(Slope, digits = 3)))
  pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))
  
  toshow <- c(slope, rsq, pvalue)
  plot(XvsY, pch = 18, col = '#00008B')
  legend('bottomleft', legend = toshow, bty = 'n')
}

to_plot(IGIvsHOMA)
to_plot(IGIvsMat)
to_plot(HOMA_bvsHOMA)
to_plot(HOMA_bvsMat)

# Another Analysis:

FPIvsPSI <- sma(First_Phase_Insulin~Peripheral_IS, log = "xy", data = FWS, slope.test = -1)

SPIvsPSI <- sma(Second_Phase_Insulin~Peripheral_IS, log = "xy", data = FWS, slope.test = -1)

to_plot(FPIvsPSI)
to_plot(SPIvsPSI)