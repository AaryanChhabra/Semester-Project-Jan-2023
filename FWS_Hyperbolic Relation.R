setwd("~/This PC/NIH/meet 3/FWS")

raw <- read.csv("FWS_OGTT_Combined.csv", na.strings = c("NA","")) # will take empty space as NA as well

head(raw)

FWS <- raw[,9:19] # Updated data, taking only useful columns.

# looking at the incomplete data:
FWS_UC <- FWS[!complete.cases(FWS),]
rownames(FWS_UC) # 9th and 119th rows are incomplete - can remove the two.

FWS <- FWS[-as.numeric(rownames(FWS_UC)),] # Now, no incomplete data
FWS_UC <- FWS[!complete.cases(FWS),]
FWS_UC # It's empty

FWS$oinsbase <- (FWS$oins0 + FWS$oins_15)/2 # taking the averages of insulin measurement at (-15 mins) and (o mins) for the basal level insulin.

# Insulin sensitivity:

FWS$HOMA <- 405/(FWS$oinsbase * FWS$oglu_base)

#mean_I <- (FWS$oinsbase + FWS$oins30 + FWS$oins60 + FWS$oins90 + FWS$oins120)/5
#mean_G <- (FWS$oglu_base + FWS$oglu30 + FWS$oglu60 + FWS$oglu90 + FWS$oglu120)/5
mean_I <- (FWS$oinsbase + (2*FWS$oins30) + (2*FWS$oins60) + (2*FWS$oins90) + FWS$oins120)/8
mean_G <- (FWS$oglu_base + (2*FWS$oglu30) + (2*FWS$oglu60) + (2*FWS$oglu90) + FWS$oglu120)/8

FWS$Mat_i <- 10000/sqrt(FWS$oinsbase * FWS$oglu_base * mean_I * mean_G)

# Beta-cell function:

FWS$IGI <- (FWS$oins30- FWS$oinsbase)/(FWS$oglu30 - FWS$oglu_base) #(I30 – I0)/(G30 – G0)

FWS$HOMA_B <- (360*FWS$oinsbase)/(FWS$oglu_base - 63)  #360*I0/(G0 – 63)


# To do the SMA - 4 combinations possible
library(smatr)

IGIvsHOMA <- sma(IGI~HOMA, log = "xy", data = FWS, slope.test = -1)
IGIvsMat <- sma(IGI~Mat_i, log = "xy", data = FWS, slope.test = -1)

HOMA_bvsHOMA <- sma(HOMA_B~HOMA, log = "xy", data = FWS, slope.test = -1)
HOMA_bvsMat <- sma(HOMA_B~Mat_i, log = "xy", data = FWS, slope.test = -1)

#define a function to plot the different graphs

to_plot <- function(XvsY){
  Slope = coef(XvsY)[2]
  Rsq = XvsY$r2
  Pvalue = XvsY$pval
  rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
  slope = bquote(slope == .(format(Slope, digits = 3)))
  pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))
  
  toshow <- c(slope, rsq, pvalue)
  plot(XvsY, pch = 18, col = '#00008B', xlab = "", ylab = "" )
  legend('bottomleft', legend = toshow, bty = 'n')
}

###
to_plot(IGIvsHOMA)
to_plot(IGIvsMat)
to_plot(HOMA_bvsHOMA)
to_plot(HOMA_bvsMat)

# Analysis 2

AIRgvsSI_bolus <-sma(airg_bolus~si_bolus, log = "xy", data = raw, slope.test = -1)
to_plot(AIRgvsSI_bolus)