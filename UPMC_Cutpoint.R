library(smatr)
library(ggplot2)
library(cutpointr)
library(tidyr)
library(dplyr)

raw <- read.csv("UPMC_Combined.csv", stringsAsFactors = T)
oglu <- raw[,c(2, 19:23, 34, 35)]


new_GT <- c()
for(i in seq(1:nrow(oglu))){
  if(oglu$oglu120[i] < 140){
    new_GT[i] = "NGT"
  }  else if(oglu$oglu120[i] > 200){
    new_GT[i] = "T2D"
  }  else{
    new_GT[i] = "IGT"
  }
  i = i+1
  
}
oglu$new_GT <- new_GT


oglu_UC <- oglu[!complete.cases(oglu),]
oglu <- oglu[-as.numeric(rownames(oglu_UC)),] # making the data set complete

# Calculating the mDI:
oglu$mDI <- oglu$si * oglu$sigma

# Calculating AUC_G:
oglu$AUC_G <- 30*0.5*(oglu$oglu0 + 2*oglu$oglu30 + 2*oglu$oglu60 + 2*oglu$oglu90 + oglu$oglu120)

p <- ggplot(data = oglu, aes(x = mDI, y = AUC_G, color = Glu_Tol_Status)) + geom_point()
p + geom_smooth(fill = NA, se = FALSE, aes(x = mDI, y = AUC_G), inherit.aes = F, color = "black")

# Taking new GT as colors
#head(oglu)
#ggplot(data = oglu, aes(x = mDI, y = oglu120, color = new_GT)) + geom_point(size = 3)
#p + geom_smooth(fill = NA, se = FALSE, aes(x = mDI, y = oglu120),method = "nls", formula = y~s*x/(F+x), method.args=list(start=c(s=20, F = -20)), inherit.aes = F, color = "black")


lin <- sma(AUC_G~mDI, log = "x", data = oglu)
plot(lin, pch = 4)
summary(lin) 

#0.8706352 
#elevation     slope
#10650.65 -7685.119 


# taking only two classes:
#oglu_NI <- oglu[oglu$Glu_Tol_Status == "NGT" | oglu$Glu_Tol_Status == "T2DM-Ab-",]
oglu_NI <- oglu[oglu$Glu_Tol_Status == "NGT" | oglu$Glu_Tol_Status == "IGT",]
opt_cut <- cutpointr(oglu_NI, x = mDI, class = Glu_Tol_Status, method = maximize_metric, metric = sum_sens_spec, tol_metric = 0.05, break_ties = c)

summary(opt_cut) # 0.2325
plot(opt_cut)
plot_metric(opt_cut)

opt_cut |> 
  select(optimal_cutpoint, sum_sens_spec) |> 
  unnest(cols = c(optimal_cutpoint, sum_sens_spec))


# To perform a simple regression:
oglu$logmDI <- log10(oglu$mDI)
lin <- lm(oglu120~logmDI, data = oglu)
plot(oglu$logmDI, oglu$oglu120, pch = 4, col = 'Blue', xlab = "mDI(log)", ylab = "AUC_G")
abline(lin)
summary(lin)

# Found values:
#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  11091.9      246.6   44.97   <2e-16 ***
#  logmDI       -7170.8      245.3  -29.24   <2e-16 ***


ggplot(data = oglu, aes(x = logmDI, y = oglu120, color = new_GT)) + geom_point() +geom_abline()

lin <- sma(oglu120~mDI*new_GT, log = "x", data = oglu)
lin2 <- sma(oglu120~mDI, log = "x", data = oglu)

#col = rgb(red =0 , green = 0, blue = 1, alpha = 0.5)
to_plot <- function(XvsY){
  Slope = coef(XvsY)[2]
  Rsq = XvsY$r2
  Pvalue = XvsY$pval
  rsq = bquote(italic(R)^2 == .(format(Rsq, digits = 3)))
  slope = bquote(slope == .(format(Slope, digits = 3)))
  pvalue = bquote(italic(p) == .(format(Pvalue, digits = 3)))
  
  toshow <- c(rsq, pvalue)
  plot(XvsY)
  legend('bottomleft', legend = toshow, bty = 'n')
}

to_plot(lin2)

summary(lin2) 
