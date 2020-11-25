setwd("C:\\Users\\Inda\\Google Drive\\Victoria University 2\\NIWA Research Project\\R_files")
rm(list = ls())
memory.limit(8056*3)
load("results\\chl.stack.list.yearly.Rda")
source('1.2 get_xy.R') #to get function get x,y values

get_reg_vals <- function(data_list,x,y){ #x=c(start=coord,end=coord)
  if(length(x)!=1){
    x <- seq(from=range(x)[1],to=range(x)[2], by=500)}
  if(length(y)!=1){
    y <- seq(from=range(y)[1],to=range(y)[2], by=500)}
  xy <- data.frame(expand.grid(x=x,y=y))
  year <- 2003:2019
  expanded <- xy[rep(row.names(xy), 17), ]
  mydf <- data.frame(expanded, year=sort(rep(year,nrow(xy))), chl=NA) #pre-set df
  location <- xy #DF
  coordinates(location) <- ~ x+ y #Convert to spatial points df format
  for(i in 1 :length(data_list)){ #1-17
    vals <- raster::extract(data_list[[i]], location) #Extract values
    mydf[((i-1)*nrow(xy)+1):(i*nrow(xy)),"chl"] <- vals}
  est.df <- data.frame(xy, est=NA)
  for (j in 1:length(x)){ 
    for (k in 1:length(y)) {
      cut <- mydf[mydf$x == x[j] & mydf$y == y[k], ] 
      reg <- summary(glm(chl~as.factor(year), cut, family = gaussian))
      est <- reg$coefficients[2]
      est.df[est.df$x==x[j] & est.df$y==y[k],"est"] <- est
    }} 
  return(est.df)
}
# example
get_reg_vals(data_list=ls_chl_yr, x=1164228:(1164228+500), y=6156750:(6156750+500))
dat <- get_reg_vals(data_list=ls_chl_yr, 
                   x=(1088228+(500*143)):(1088228+(500*(143+9))), 
                   y=(6260250-(500*207)):(6260250-(500*(207+9))))
ggplot() +
  geom_raster(data = dat, aes(x = x, y = y, fill = est)) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = 'Chlorophyll heatmap', fill = 'coefficient') +
  # scale_fill_gradient(low = "yellow", high = "red", limits = c(min(r2$SST), 15), oob = squish)
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey")



##########################################################
#100*100 raster
dat <- get_xy_vals(data_list=ls_chl_yr, 
                   x=(1088228+(500*143)):(1088228+(500*(143+9))), 
                   y=(6260250-(500*207)):(6260250-(500*(207+9))))
head(dat)
dat$location <- paste(dat$x,"-",dat$y)
head(dat)
length(unique(dat$location))

#fitting a fixed effects randomised block design 
fit <- lm(chl~ as.factor(location) + as.factor(year), data = dat)

par(mfrow=c(1,2))
plot(fit$res ~fit$fitted, xlab = "Fitted values", ylab = "Residuals")
abline(h=0, lty=2, col="grey")
qqnorm(fit$res, main="", ylab = "Residuals", xlab = "Quantiles of Standard Normal")
qqline(fit$res)
#anova is likely to be invalid
anova(fit)
# Analysis of Variance Table
# 
# Response: chl
#                       Df Sum Sq Mean Sq F value Pr(>F)    
# as.factor(location)   99 0.4912 0.00496  1.0172 0.4365    
# as.factor(year)       16 7.3629 0.46018 94.3461 <2e-16 ***
#   Residuals           1584 7.7261 0.00488                   
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# CONCLUSION
# The anova results for testing the hypothesis
#$H_0:\alpha_{1},...,alpha_{17}$ vs $H_1:\ \alpha_{i} \ne 0$ for atleast one i
#indicates there is strong evidence against the null hypothesis.

#This means there is strong evidence that effect of years (treatment effect) 
#differ after controlling for pixel-to-pixel variation in chlorophyll levels.

# However if we fit a one-way ANOVA ignoring blocks
fit1 <- lm(chl~ as.factor(year), data = dat)
anova(fit1)
# Analysis of Variance Table
# 
# Response: chl
#                   Df Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(year)   16 7.3629 0.46018  94.251 < 2.2e-16 ***
#   Residuals       1683 8.2172 0.00488                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# the unexplained error (Mean Sq) has not increased compared to 
# the blocked design (MSE = 0.00488). It has remained the same. WHY??

# back to block model 'Fit'
summary(fit)
# Call:
#   lm(formula = chl ~ as.factor(location) + as.factor(year), data = dat)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.12649 -0.02084 -0.00353  0.01315  1.32358 
# 
# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           0.4918697  0.0182434  26.962  < 2e-16 ***
# as.factor(location)1159728 - 6152750  0.0091052  0.0239548   0.380  0.70392    
# as.factor(location)1159728 - 6153250 -0.0119931  0.0239548  -0.501  0.61668    
### above estimate of -0.0119931 is \mu_{L3} - \mu_{L1}
# as.factor(location)1159728 - 6153750 -0.0116795  0.0239548  -0.488  0.62593    
# as.factor(location)1159728 - 6154250 -0.0037133  0.0239548  -0.155  0.87683    
# as.factor(location)1159728 - 6154750 -0.0050569  0.0239548  -0.211  0.83284    
# as.factor(location)1159728 - 6155250 -0.0159069  0.0239548  -0.664  0.50676    
# as.factor(location)1159728 - 6155750 -0.0179537  0.0239548  -0.749  0.45368    
# as.factor(location)1159728 - 6156250 -0.0194419  0.0239548  -0.812  0.41714    
# as.factor(location)1159728 - 6156750 -0.0194823  0.0239548  -0.813  0.41617    
# as.factor(location)1160228 - 6152250  0.0118850  0.0239548   0.496  0.61986    
# as.factor(location)1160228 - 6152750  0.0013786  0.0239548   0.058  0.95412    
# as.factor(location)1160228 - 6153250 -0.0012439  0.0239548  -0.052  0.95859    
# as.factor(location)1160228 - 6153750 -0.0027783  0.0239548  -0.116  0.90768    
# as.factor(location)1160228 - 6154250 -0.0084840  0.0239548  -0.354  0.72326    
# as.factor(location)1160228 - 6154750 -0.0091846  0.0239548  -0.383  0.70146    
# as.factor(location)1160228 - 6155250 -0.0155711  0.0239548  -0.650  0.51577    
# as.factor(location)1160228 - 6155750 -0.0086351  0.0239548  -0.360  0.71854    
# as.factor(location)1160228 - 6156250 -0.0025614  0.0239548  -0.107  0.91486    
# as.factor(location)1160228 - 6156750 -0.0130009  0.0239548  -0.543  0.58739    
# as.factor(location)1160728 - 6152250  0.0058913  0.0239548   0.246  0.80576    
# as.factor(location)1160728 - 6152750  0.0010375  0.0239548   0.043  0.96546    
# as.factor(location)1160728 - 6153250  0.0065257  0.0239548   0.272  0.78534    
# as.factor(location)1160728 - 6153750 -0.0132221  0.0239548  -0.552  0.58105    
# as.factor(location)1160728 - 6154250 -0.0189918  0.0239548  -0.793  0.42800    
# as.factor(location)1160728 - 6154750 -0.0159926  0.0239548  -0.668  0.50448    
# as.factor(location)1160728 - 6155250 -0.0099081  0.0239548  -0.414  0.67921    
# as.factor(location)1160728 - 6155750 -0.0152346  0.0239548  -0.636  0.52489    
# as.factor(location)1160728 - 6156250 -0.0053028  0.0239548  -0.221  0.82483    
# as.factor(location)1160728 - 6156750  0.0014372  0.0239548   0.060  0.95216    
# as.factor(location)1161228 - 6152250  0.0039870  0.0239548   0.166  0.86783    
# as.factor(location)1161228 - 6152750 -0.0036265  0.0239548  -0.151  0.87969    
# as.factor(location)1161228 - 6153250  0.0014041  0.0239548   0.059  0.95326    
# as.factor(location)1161228 - 6153750 -0.0048988  0.0239548  -0.205  0.83799    
# as.factor(location)1161228 - 6154250 -0.0067811  0.0239548  -0.283  0.77715    
# as.factor(location)1161228 - 6154750 -0.0032811  0.0239548  -0.137  0.89107    
# as.factor(location)1161228 - 6155250 -0.0108354  0.0239548  -0.452  0.65110    
# as.factor(location)1161228 - 6155750 -0.0156298  0.0239548  -0.652  0.51419    
# as.factor(location)1161228 - 6156250 -0.0156202  0.0239548  -0.652  0.51445    
# as.factor(location)1161228 - 6156750 -0.0058572  0.0239548  -0.245  0.80687    
# as.factor(location)1161728 - 6152250  0.0011817  0.0239548   0.049  0.96066    
# as.factor(location)1161728 - 6152750  0.0293772  0.0239548   1.226  0.22025    
# as.factor(location)1161728 - 6153250  0.0026687  0.0239548   0.111  0.91131    
# as.factor(location)1161728 - 6153750 -0.0047310  0.0239548  -0.197  0.84346    
# as.factor(location)1161728 - 6154250 -0.0027218  0.0239548  -0.114  0.90955    
# as.factor(location)1161728 - 6154750 -0.0064665  0.0239548  -0.270  0.78724    
# as.factor(location)1161728 - 6155250 -0.0103810  0.0239548  -0.433  0.66481    
# as.factor(location)1161728 - 6155750 -0.0012804  0.0239548  -0.053  0.95738    
# as.factor(location)1161728 - 6156250 -0.0106014  0.0239548  -0.443  0.65815    
# as.factor(location)1161728 - 6156750 -0.0150348  0.0239548  -0.628  0.53034    
# as.factor(location)1162228 - 6152250  0.0051962  0.0239548   0.217  0.82830    
# as.factor(location)1162228 - 6152750  0.0044532  0.0239548   0.186  0.85255    
# as.factor(location)1162228 - 6153250 -0.0064205  0.0239548  -0.268  0.78871    
# as.factor(location)1162228 - 6153750 -0.0100192  0.0239548  -0.418  0.67582    
# as.factor(location)1162228 - 6154250 -0.0169439  0.0239548  -0.707  0.47947    
# as.factor(location)1162228 - 6154750 -0.0027132  0.0239548  -0.113  0.90983    
# as.factor(location)1162228 - 6155250 -0.0009090  0.0239548  -0.038  0.96974    
# as.factor(location)1162228 - 6155750  0.0034061  0.0239548   0.142  0.88695    
# as.factor(location)1162228 - 6156250 -0.0061301  0.0239548  -0.256  0.79806    
# as.factor(location)1162228 - 6156750  0.0543773  0.0239548   2.270  0.02334 *  
# as.factor(location)1162728 - 6152250 -0.0066922  0.0239548  -0.279  0.78000    
# as.factor(location)1162728 - 6152750  0.0040336  0.0239548   0.168  0.86630    
# as.factor(location)1162728 - 6153250 -0.0033029  0.0239548  -0.138  0.89035    
# as.factor(location)1162728 - 6153750 -0.0114348  0.0239548  -0.477  0.63318    
# as.factor(location)1162728 - 6154250 -0.0199572  0.0239548  -0.833  0.40490    
# as.factor(location)1162728 - 6154750 -0.0175384  0.0239548  -0.732  0.46419    
# as.factor(location)1162728 - 6155250  0.0459727  0.0239548   1.919  0.05515 .  
# as.factor(location)1162728 - 6155750  0.0543038  0.0239548   2.267  0.02353 *  
# as.factor(location)1162728 - 6156250 -0.0065711  0.0239548  -0.274  0.78388    
# as.factor(location)1162728 - 6156750 -0.0020018  0.0239548  -0.084  0.93341    
# as.factor(location)1163228 - 6152250 -0.0090372  0.0239548  -0.377  0.70603    
# as.factor(location)1163228 - 6152750 -0.0047251  0.0239548  -0.197  0.84365    
# as.factor(location)1163228 - 6153250  0.0004067  0.0239548   0.017  0.98646    
# as.factor(location)1163228 - 6153750  0.0002376  0.0239548   0.010  0.99209    
# as.factor(location)1163228 - 6154250 -0.0102103  0.0239548  -0.426  0.67000    
# as.factor(location)1163228 - 6154750 -0.0144647  0.0239548  -0.604  0.54604    
# as.factor(location)1163228 - 6155250  0.0615561  0.0239548   2.570  0.01027 *  
# as.factor(location)1163228 - 6155750 -0.0221768  0.0239548  -0.926  0.35470    
# as.factor(location)1163228 - 6156250 -0.0079285  0.0239548  -0.331  0.74070    
# as.factor(location)1163228 - 6156750 -0.0110883  0.0239548  -0.463  0.64351    
# as.factor(location)1163728 - 6152250  0.0002819  0.0239548   0.012  0.99061    
# as.factor(location)1163728 - 6152750  0.0050941  0.0239548   0.213  0.83162    
# as.factor(location)1163728 - 6153250 -0.0044617  0.0239548  -0.186  0.85227    
# as.factor(location)1163728 - 6153750 -0.0032717  0.0239548  -0.137  0.89138    
# as.factor(location)1163728 - 6154250 -0.0020103  0.0239548  -0.084  0.93313    
# as.factor(location)1163728 - 6154750 -0.0031924  0.0239548  -0.133  0.89400    
# as.factor(location)1163728 - 6155250 -0.0091993  0.0239548  -0.384  0.70101    
# as.factor(location)1163728 - 6155750 -0.0116847  0.0239548  -0.488  0.62577    
# as.factor(location)1163728 - 6156250 -0.0184361  0.0239548  -0.770  0.44164    
# as.factor(location)1163728 - 6156750 -0.0153096  0.0239548  -0.639  0.52285    
# as.factor(location)1164228 - 6152250  0.0118694  0.0239548   0.495  0.62032    
# as.factor(location)1164228 - 6152750  0.0415809  0.0239548   1.736  0.08279 .  
# as.factor(location)1164228 - 6153250  0.0034287  0.0239548   0.143  0.88621    
# as.factor(location)1164228 - 6153750  0.0036547  0.0239548   0.153  0.87876    
# as.factor(location)1164228 - 6154250 -0.0096476  0.0239548  -0.403  0.68719    
# as.factor(location)1164228 - 6154750  0.0756420  0.0239548   3.158  0.00162 ** 
# as.factor(location)1164228 - 6155250  0.0021032  0.0239548   0.088  0.93005    
# as.factor(location)1164228 - 6155750 -0.0077590  0.0239548  -0.324  0.74606    
# as.factor(location)1164228 - 6156250 -0.0165481  0.0239548  -0.691  0.48979    
# as.factor(location)1164228 - 6156750 -0.0113883  0.0239548  -0.475  0.63456    
### above estimate of 0.0239548 is \mu_{L100} - \mu_{L1}
# as.factor(year)2004                  -0.0209889  0.0098768  -2.125  0.03374 *  
# as.factor(year)2005                   0.1287076  0.0098768  13.031  < 2e-16 ***
# as.factor(year)2006                  -0.0079428  0.0098768  -0.804  0.42141    
# as.factor(year)2007                   0.1511382  0.0098768  15.302  < 2e-16 ***
# as.factor(year)2008                   0.0285702  0.0098768   2.893  0.00387 ** 
# as.factor(year)2009                   0.1694651  0.0098768  17.158  < 2e-16 ***
# as.factor(year)2010                   0.2176804  0.0098768  22.040  < 2e-16 ***
# as.factor(year)2011                   0.0538972  0.0098768   5.457 5.61e-08 ***
# as.factor(year)2012                   0.0784418  0.0098768   7.942 3.74e-15 ***
# as.factor(year)2013                   0.0833499  0.0098768   8.439  < 2e-16 ***
# as.factor(year)2014                   0.0417774  0.0098768   4.230 2.47e-05 ***
# as.factor(year)2015                   0.0760708  0.0098768   7.702 2.35e-14 ***
# as.factor(year)2016                   0.1517833  0.0098768  15.368  < 2e-16 ***
# as.factor(year)2017                   0.1056598  0.0098768  10.698  < 2e-16 ***
# as.factor(year)2018                   0.0433252  0.0098768   4.387 1.23e-05 ***
# as.factor(year)2019                   0.1373661  0.0098768  13.908  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.06984 on 1584 degrees of freedom
# Multiple R-squared:  0.5041,	Adjusted R-squared:  0.4681 
# F-statistic:    14 on 115 and 1584 DF,  p-value: < 2.2e-16


# FITTING A MIXED EFFECTS MODEL

# Fixed effects imply that the effect of each location (block) is of 
# interest to us. But we are only interested in the vatiation it causes in Y.
# Blocks are regarded as a random sample of all possible blocks. The variation
# of block effects is estimated, but the effect of individual blocks is not.
# We will consider Treatment as fixed effect and block as random effect.

library(nlme)
### change treatment and block to factors
dat$year <- as.factor(dat$year)
dat$location <- as.factor(dat$location)
fit2 <- lme(chl ~ year, random=~1|location, data=dat)

par(mfrow=c(1,2))
plot(fit2$res ~fit2$fitted, xlab = "Fitted values", ylab = "Residuals")
abline(h=0, lty=2, col="grey")
qqnorm(fit2$res, main="", ylab = "Residuals", xlab = "Quantiles of Standard Normal")
qqline(fit2$res)
#ANOVA assumptions not met

#Testing the same hypothesis as in fixed effects model
#$H_0:\alpha_{1},...,alpha_{17}$ vs $H_1:\ \alpha_{i} \ne 0$ for atleast one i

anova(fit2)
#               numDF denDF   F-value p-value
# (Intercept)     1  1584 113027.42  <.0001
# year           16  1584     94.35  <.0001

#The F-statistic, df and p-value for year match what we had before in 
#the fixed effects model (with both year and location fixed)

summary(fit2)
# Linear mixed-effects model fit by REML
# Data: dat 
#     AIC       BIC   logLik
# -4064.669 -3961.531 2051.335
# 
# Random effects:
#   Formula: ~1 | location
#         (Intercept)   Residual
# StdDev: 0.002222395 0.06983954
# 
# Fixed effects: chl ~ year 
#                 Value   Std.Error   DF  t-value p-value
# (Intercept)  0.4897419 0.006987489 1584 70.08840  0.0000
### Value 0.4897419 above represents $\hat\mu_1$. 
### Mean chl for year 2003
# year2004    -0.0209889 0.009876802 1584 -2.12507  0.0337
### Value -0.0209889 above represents \mu_2-\mu_1 = -0.0209889
### Mean chl for year 2004 = -0.0209889 + 0.4897419 = 0.468753
# year2005     0.1287076 0.009876802 1584 13.03130  0.0000
# year2006    -0.0079428 0.009876802 1584 -0.80419  0.4214
# year2007     0.1511382 0.009876802 1584 15.30234  0.0000
# year2008     0.0285702 0.009876802 1584  2.89266  0.0039
# year2009     0.1694651 0.009876802 1584 17.15789  0.0000
# year2010     0.2176804 0.009876802 1584 22.03957  0.0000
# year2011     0.0538972 0.009876802 1584  5.45695  0.0000
# year2012     0.0784418 0.009876802 1584  7.94202  0.0000
# year2013     0.0833499 0.009876802 1584  8.43896  0.0000
# year2014     0.0417774 0.009876802 1584  4.22985  0.0000
# year2015     0.0760708 0.009876802 1584  7.70196  0.0000
# year2016     0.1517833 0.009876802 1584 15.36765  0.0000
# year2017     0.1056598 0.009876802 1584 10.69777  0.0000
# year2018     0.0433252 0.009876802 1584  4.38657  0.0000
# year2019     0.1373661 0.009876802 1584 13.90796  0.0000
# Correlation: 
#   (Intr) yr2004 yr2005 yr2006 yr2007 yr2008 yr2009 yr2010 yr2011 yr2012
# year2004 -0.707                                                               
# year2005 -0.707  0.500                                                        
# year2006 -0.707  0.500  0.500                                                 
# year2007 -0.707  0.500  0.500  0.500                                          
# year2008 -0.707  0.500 ...................................

# Total variation after allowing for fixed effects of year

# Estimated proportion of variation due to block\location effects
# after allowing for year = 0.03 or 3%

# Estimated proportion of residual variation
# after allowing for year = 0.969 or 97%

##INCLUDE INTERACTION

fit3 <- lm(chl~ as.factor(location)*as.factor(year), data = dat)
anova(fit3)
# Analysis of Variance Table
# 
# Response: chl
# Df Sum Sq Mean Sq F value Pr(>F)
# as.factor(location)                   99 0.4912 0.00496               
# as.factor(year)                       16 7.3629 0.46018               
# as.factor(location):as.factor(year) 1584 7.7261 0.00488               
# Residuals                              0 0.0000                       
# Warning message:
#   In anova.lm(fit3) :
#   ANOVA F-tests on an essentially perfect fit are unreliable
summary(fit3)
