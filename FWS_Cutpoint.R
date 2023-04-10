# working on FWS data:

raw <- read.csv("FWS_OGTT_Combined.csv")
oglu <- raw[,c(9:13, 34, 36,37)]


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



# Calculating the mDI:
oglu$mDI <- oglu$si * oglu$sigma

# Calculating AUC_G:
oglu$AUC_G <- 30*0.5*(oglu$oglu_base + 2*oglu$oglu30 + 2*oglu$oglu60 + 2*oglu$oglu90 + oglu$oglu120)

p <- ggplot(data = oglu, aes(x = mDI, y = AUC_G, color = GT)) + geom_point()
p + geom_smooth(fill = NA, se = FALSE, aes(x = mDI, y = AUC_G), inherit.aes = F, color = "black")

#ggplot(data = oglu, aes(x = mDI, y = oglu120, color = new_GT)) + geom_point(size = 3)

lin <- sma(AUC_G~mDI, log = "x", data = oglu)
plot(lin, pch = 4)
summary(lin)

lin$r2 # 0.8763999

#elevation     slope
#10097.971   -8169.599

opt_cut <- cutpointr(oglu, x = mDI, class = GT, method = maximize_metric, metric = sum_sens_spec, tol_metric = 0.05, break_ties = c)
summary(opt_cut) #0.1823

plot(opt_cut)
plot_metric(opt_cut)

opt_cut |> 
  select(optimal_cutpoint, sum_sens_spec) |> 
  unnest(cols = c(optimal_cutpoint, sum_sens_spec))


oglu$logmDI <- log10(oglu$mDI)
lin <- lm(AUC_G~logmDI, data = oglu)
plot(oglu$logmDI, oglu$AUC_G, pch = 4, col = 'Blue', xlab = "mDI(log)", ylab = "AUC_G")
abline(lin)

plot(lin, pch = 4)
summary(lin)

"""
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  10459.6      194.7   53.73   <2e-16 ***
  logmDI       -7648.1      252.9  -30.24   <2e-16 ***
"""


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

lin2 <- sma(oglu120~mDI, log = "x", data = oglu)
to_plot(lin2)
summary(lin2)
