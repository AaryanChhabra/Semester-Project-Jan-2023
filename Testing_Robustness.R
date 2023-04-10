
# many failures taking log when mdi < 0

par(mfrow=c(1,1))

slope_not_zero_mdi <- 0
slope_not_zero_auc <- 0
x <- 0
total <- 1000 # No. of samples
n = 5 # original was 30


mdi0 = seq(0.8, 0.25, len=n)
log_mdi0 <- log10(mdi0)
t = seq(1, 5, len=n)




slope = -7926
elevation = 10373
  
AUC_G0 <- slope*log_mdi0 + elevation

plot(t,AUC_G0, pch = 16)
plot(t, mdi0, pch = 16)
  
#sd = 0.3*median(mdi0) # with a reasonable noise level.

sd = 0.5 # now a fractional change, applied to each value of mdi below

plot(mdi0, AUC_G0, main="Reference Case", pch = 16, col = 'dark blue')


t_mdi = numeric(total)
t_AUC = numeric(total)

halt = 0
  
while(x<total){
  noise <- runif(length(mdi0), min = -sd, max = sd)
  mdi <- mdi0*(1 + noise)
  
  log_mdi <- log10(mdi)
  AUC_G <- slope*log_mdi + elevation
  
  lin1 <- lm(mdi~t)
  lin2 <- lm(AUC_G~t)
  
  # if the t-value is greater than 1.96, 
  # we can reject the null that there slope = 0
  
  if(summary(lin2)$coefficients[2,4] < 0.05){
    slope_not_zero_auc <- slope_not_zero_auc + 1
  }
  
  if(summary(lin1)$coefficients[2,4] < 0.05){
    slope_not_zero_mdi <- slope_not_zero_mdi + 1
  }
  
  # collect t values
  t_mdi[x] = abs(summary(lin1)$coefficients[2,3])
  t_AUC[x] = abs(summary(lin2)$coefficients[2,3])
  
  
  
  x <- x+1
}

summary(lin2)$coefficients

fraction_mdi <- slope_not_zero_mdi/total
fraction_auc <- slope_not_zero_auc/total

cat(paste("Fraction non-zero slope cases in mDI:", fraction_mdi,
          "Fraction non-zero slope cases in AUC_G:",fraction_auc, sep = "\n"))
cat("\n")

successes = c(slope_not_zero_mdi, slope_not_zero_auc)
attempts = c(total, total)


p = prop.test(successes, attempts)$p.value


cat(paste("p value for difference of proportions:", round(p, 3),"\n"))


plot(mdi, AUC_G, main="Perturbed case", pch = 16)

par(mfrow=c(2,1))

plot(t, mdi0, pch = 16, col = 'dark green')
plot(t, mdi, pch = 16, col = 'dark green')

plot(t, AUC_G0, pch = 16, col = 'dark blue')
plot(t, AUC_G, pch = 16, col = 'dark blue')


hist(t_mdi)
hist(t_AUC)

par(mfrow=c(1,1))



