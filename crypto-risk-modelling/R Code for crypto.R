library(quantmod)
library(ggplot2)
library(scales)
library(tidyverse)
ETH <- read.csv("~/Documents/ETH.csv")
# Rename columns for convenience
colnames(ETH) <- c("Date", "open", "high", "low", "close", "volume")
ETH$Date <-  as.Date(ETH$Date, format = "%m/%d/%Y")

ggplot(ETH, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = low, yend = high), color = "black") +  
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = Date - 0.3, xmax = Date + 0.3, 
                fill = factor(close > open, labels = c("Down", "Up"))), 
            color = "black") +
  scale_fill_manual(values = c("red", "green"), name = "Trend") +
  theme_minimal() +
  labs(title = "Ethereum Candlestick Chart", x = "Date", y = "Price") +
  
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", 
               expand = c(0.01, 0)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

BTC <- read.csv("~/BTC.csv")
colnames(BTC) <- c("Date", "open", "high", "low", "close", "volume")
BTC$Date <-  as.Date(BTC$Date, format = "%m/%d/%Y")
ggplot(BTC, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = low, yend = high), color = "black") +  
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = Date - 0.3, xmax = Date + 0.3, 
                fill = factor(close > open, labels = c("Down", "Up"))), 
            color = "black") +
  scale_fill_manual(values = c("red", "green"), name = "Trend") +
  theme_minimal() +
  labs(title = "Bitcoin Candlestick Chart", x = "Date", y = "Price") +
  
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", 
               expand = c(0.01, 0)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

XRP <- read.csv("~/XRP.csv")
colnames(XRP) <- c("Date", "close", "open", "high", "low", "volume")
XRP$Date <-  as.Date(XRP$Date, format = "%m/%d/%Y")
ggplot(XRP, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = low, yend = high), color = "black") +  
  geom_rect(aes(ymin = pmin(open, close), ymax = pmax(open, close),
                xmin = Date - 0.3, xmax = Date + 0.3, 
                fill = factor(close > open, labels = c("Down", "Up"))), 
            color = "black") +
  scale_fill_manual(values = c("red", "green"), name = "Trend") +
  theme_minimal() +
  labs(title = "Ripple Candlestick Chart", x = "Date", y = "Price") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", 
               expand = c(0.01, 0)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

A2: Code for Figure 1.3
BTC2 <- read.csv("C:/Users/LENOVO/Downloads/Cleaned_Bitcoin_Data_2015_to_2021.csv")
BTC2_ts <- ts(BTC2[,2], start = c(2015,1), frequency = 365)
XRP <- read.csv("C:/Users/LENOVO/Downloads/XRP Historical Data.csv")
XRP_ts <- ts(XRP[,2], c(2020,1), end = c(2024,365), frequency = 365)
Data <- read.csv("C:/Users/LENOVO/Downloads/eth (correct).csv")
Data_ts <- ts(Data[,5], c(2020,1), end = c(2024,365), frequency = 365)
Data2 <- read.csv("C:/Users/LENOVO/Downloads/btc (1).csv")
Data2_ts <- ts(Data2[,5], start = c(2020,1), end = c(2024,365), frequency = 365)
plot(Data_ts,lwd = 2, col = "red",ylim = c(0, 100000), ylab = "US Dollar",  main = "Ethereum vs Bitcoin vs XRP")
lines(XRP_ts, col = "darkgreen", lwd = 2)
lines(Data2_ts, col = "blue", lwd = 2)
legend("topleft", legend = c("Ethereum", "Bitcoin", "XRP"), col = c("red","blue", "darkgreen"), bty = 'n', pch = c(NA,1, 1), lty = c(1,NA, 2), lwd = 2)
A3: Code for Figure 1.4
plot(Data2_ts, col = "blue", ylab = "US Dollar",main = "Bitcoin[2015 - 2021] and Bitcoin[2020 - 2024]", lwd = 2, xlim = c(2015,2025), ylim = c(0,120000))
lines(BTC2_ts, col = "red")
abline(v = 2020,  col = "blue", lwd = 2)
abline(v = 2021.57, col = "red", lwd = 2)
legend("topleft", legend = c("BTC[2015-2021] ", "BTC[2020-2024]"), col = c("red", "blue"), bty = 'n',
       pch = c(NA,1), lty = c(1,NA), lwd = 2)
boxplot(list(BTC2_ts, Data2_ts), col = c("red", "blue"), ylab = "US Dollar")
A4: Code for R packages used for Chapter 3 Analysis
install.packages("moments")
library(moments)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("actuar")
library(actuar)
install.packages("readxl")
library(readxl)
install.packages("gendist")
library(gendist)
install.packages("pracma")
library(pracma)
install.packages("numDeriv")
library(numDeriv)
install.packages("ReIns")
library(ReIns)
A5: Code to split the data into gains and losses separately (for BTC only)
btc <- read.csv("C:/Users/2021923558/Downloads/btc (1).csv")
btc$logreturns = 0
btc$logreturns
#Assign to 'nrows' the total number of rows in the data set
nrows = nrow(btc)
nrows
for(i in 2:nrows){
  btc$logreturns[i] = log(btc$close[i])-log(btc$close[i-1])
}
btc$logreturns
btcreturns = btc$logreturns[-1]
#Create columns for the losses and gains in the BTC dataframe.
btc$losses = NA
btc$gains = NA
#Allocate the losses and gains to their respective columns.
for(i in 1:length(btcreturns)){
  if(btcreturns[i]>0){
    btc$gains[i+1] = btcreturns[i]
  }
  else {
    btc$losses[i+1] = btcreturns[i]
  }
}
btclosses = na.omit(btc$losses)
btclosses = as.vector(btclosses*(-1))
btcgains = na.omit(btc$gains)
btcgains = as.vector(btcgains)

A6: Code for descriptive statistics of BTC
LOGRETURNS
> mean(btcreturns)
[1] 0.00140675
> median(btcreturns)
[1] 0.0007492552
> min(btcreturns)
[1] -0.5018366
> max(btcreturns)
[1] 0.1781121
> sd(btcreturns)
[1] 0.03466402
> var(btcreturns)
[1] 0.001201594
> skewness(btcreturns)
[1] -1.650635
> kurtosis(btcreturns)
[1] 29.20851

A7: Code to fully analyse BTC gains and losses separately 
The following BTC codes are similar for ETH and XRP, thus the latter two are omitted to preserve space.

A7.1: Code for Losses
> #########LOSSES
  > #Empirical (dataset)
  > #VaR 70%
  > (var_70_btclosses <- quantile(btclosses, 0.7))
70% 
0.0245717 
> #VaR 80%
  > (var_80_btclosses <- quantile(btclosses, 0.8))
80% 
0.03449438 
> #VaR 90%
  > (var_90_btclosses <- quantile(btclosses, 0.9))
90% 
0.05238474 
> #VaR 95%
  > (var_95_btclosses <- quantile(btclosses, 0.95))
95% 
0.0645793 
> #VaR 99%
  > (var_99_btclosses <- quantile(btclosses, 0.99))
99% 
0.1164727 
> #VaR 99.5%
  > (var_99.5_btclosses <- quantile(btclosses, 0.995))
99.5% 
0.1363176 
> #TVaR 70%
  > (tvar_70_btclosses <- mean(btclosses[btclosses>var_70_btclosses]))
[1] 0.05091179
> #TVaR 80%
  > (tvar_80_btclosses <- mean(btclosses[btclosses>var_80_btclosses]))
[1] 0.06180669
> #TVaR 90%
  > (tvar_90_btclosses <- mean(btclosses[btclosses>var_90_btclosses]))
[1] 0.08145158
> #TVaR 95%
  > (tvar_95_btclosses <- mean(btclosses[btclosses>var_95_btclosses]))
[1] 0.1029313
> #TVaR 99%
  > (tvar_99_btclosses <- mean(btclosses[btclosses>var_99_btclosses]))
[1] 0.1786643
> #TVaR 99.5%
  > (tvar_99.5_btclosses <- mean(btclosses[btclosses>var_99.5_btclosses]))
[1] 0.2233853


EXPONENTIAL:
  Goodness of fit
> btclosses_exp = fitdist(btclosses, "exp", method = "mle")
> summary(btclosses_exp)
Fitting of the distribution ' exp ' by maximum likelihood 
Parameters : 
  estimate Std. Error
rate 45.10454   1.516172
Loglikelihood:  2485.95   AIC:  -4969.9   BIC:  -4965.114 
> gofstat(btclosses_exp, fitnames = "Fitting exponential to BTC losses")
Goodness-of-fit statistics
Fitting exponential to BTC losses
Kolmogorov-Smirnov statistic                        0.04477105
Cramer-von Mises statistic                          0.42878385
Anderson-Darling statistic                          2.02114697

Goodness-of-fit criteria
Fitting exponential to BTC losses
Akaike's Information Criterion                         -4969.900
Bayesian Information Criterion                         -4965.114
Risk measures
> #VaR 70%
> (var_70_exp_btclosses <- qexp(0.70, rate = lambda.loss_exp))
[1] 0.02669294
> #VaR 80%
> (var_80_exp_btclosses <- qexp(0.80, rate = lambda.loss_exp))
[1] 0.03568239
> #VaR 90%
> (var_90_exp_btclosses <- qexp(0.90, rate = lambda.loss_exp))
[1] 0.05104997
> #VaR 95%
> (var_95_exp_btclosses <- qexp(0.95, rate = lambda.loss_exp))
[1] 0.06641754
> #VaR 99%
> (var_99_exp_btclosses <- qexp(0.99, rate = lambda.loss_exp))
[1] 0.1020999
> #VaR 99.5%
> (var_95.5_exp_btclosses <- qexp(0.995, rate = lambda.loss_exp))
[1] 0.1174675
> f = function(x) qexp(x, rate = lambda.loss_exp)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.0146591 with absolute error < 0.00000000000000025
> (tvar_70_exp_btclosses <- (1/(1-0.7))*0.0146591)
[1] 0.04886367
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.0115705 with absolute error < 0.0001
> (tvar_80_exp_btclosses <- (1/(1-0.8))*0.0115705)
[1] 0.0578525
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.00732195 with absolute error < 0.0001
> (tvar_90_exp_btclosses <- (1/(1-0.9))*0.00732195)
[1] 0.0732195
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004429295 with absolute error < 1e-04
> (tvar_95_exp_btclosses <- (1/(1-0.95))*0.004429295)
[1] 0.0885859
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001242612 with absolute error < 8.1e-05
> (tvar_99_exp_btclosses <- (1/(1-0.99))*0.001242612)
[1] 0.1242612
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0006981438 with absolute error < 4.1e-05
> (tvar_99.5_exp_btclosses <- (1/(1-0.995))*0.0006981438)
[1] 0.1396288

 
GAMMA:
Goodness of fit
> btclosses_gamma = fitdist(btclosses, "gamma", start = list(shape = 1, scale = 1),  method = "mle")
> summary(btclosses_gamma)
Fitting of the distribution ' gamma ' by maximum likelihood 
Parameters : 
        estimate  Std. Error
shape 0.91965652 0.037963862
scale 0.02410799 0.001295395
Loglikelihood:  2488.03   AIC:  -4972.06   BIC:  -4962.489 
Correlation matrix:
           shape      scale
shape  1.0000000 -0.7608023
scale -0.7608023  1.0000000
> gofstat(btclosses_gamma, fitnames = "Fitting gamma to BTC losses")
Goodness-of-fit statistics
                             Fitting gamma to BTC losses
Kolmogorov-Smirnov statistic                  0.03038022
Cramer-von Mises statistic                    0.21320032
Anderson-Darling statistic                    1.14446270

Goodness-of-fit criteria
                               Fitting gamma to BTC losses
Akaike's Information Criterion                   -4972.060
Bayesian Information Criterion                   -4962.489
Risk measures
> #VaR 70%
  > (var_70_gamma_btclosses <- qgamma(0.70, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.02651949
> #VaR 80%
  > (var_80_gamma_btclosses <- qgamma(0.80, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.03591322
> #VaR 90%
  > (var_90_gamma_btclosses <- qgamma(0.90, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.0521128
> #VaR 95%
  > (var_95_gamma_btclosses <- qgamma(0.95, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.06842215
> #VaR 99%
  > (var_99_gamma_btclosses <- qgamma(0.99, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.106527
> #VaR 99.5%
  > (var_95.5_gamma_btclosses <- qgamma(0.995, shape = alpha.loss_gamma, scale = lambda.loss_gamma))
[1] 0.1230013
> f = function(x) qgamma(x, shape = alpha.loss_gamma, scale = lambda.loss_gamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.0149682 with absolute error < 0.00000000045
> (tvar_70_gamma_btclosses <- (1/(1-0.7))*0.0149682)
[1] 0.049894
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01187875 with absolute error < 0.00011
> (tvar_80_gamma_btclosses <- (1/(1-0.8))*0.01187875)
[1] 0.05939375
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.007571448 with absolute error < 0.00011
> (tvar_90_gamma_btclosses <- (1/(1-0.9))*0.007571448)
[1] 0.07571448
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.004605158 with absolute error < 0.00011
> (tvar_95_gamma_btclosses <- (1/(1-0.95))*0.004605158)
[1] 0.09210316
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001303135 with absolute error < 8.8e-05
> (tvar_99_gamma_btclosses <- (1/(1-0.99))*0.001303135)
[1] 0.1303135
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0007340977 with absolute error < 4.4e-05
> (tvar_99.5_gamma_btclosses <- (1/(1-0.995))*0.0007340977)
[1] 0.1468195


WEIBULL:
  Goodness of fit
> btclosses_weib = fitdist(btclosses, "weibull",  method = "mle")
> summary(btclosses_weib)
Fitting of the distribution ' weibull ' by maximum likelihood 
Parameters : 
  estimate   Std. Error
shape 0.92778066 0.0234000826
scale 0.02135865 0.0008110146
Loglikelihood:  2490.592   AIC:  -4977.184   BIC:  -4967.613 
Correlation matrix:
  shape     scale
shape 1.0000000 0.3185615
scale 0.3185615 1.0000000
> gofstat(btclosses_weib, fitnames = "Fitting Weibull to BTC losses")
Goodness-of-fit statistics
Fitting Weibull to BTC losses
Kolmogorov-Smirnov statistic                    0.02354307
Cramer-von Mises statistic                      0.14265710
Anderson-Darling statistic                      0.94938760

Goodness-of-fit criteria
Fitting Weibull to BTC losses
Akaike's Information Criterion                     -4977.184
Bayesian Information Criterion                     -4967.613
Risk measures
> #VaR 70%
> (var_70_weib_btclosses <- qweibull(0.70, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.0260895
> #VaR 80%
> (var_80_weib_btclosses <- qweibull(0.80, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.03567268
> #VaR 90%
> (var_90_weib_btclosses <- qweibull(0.90, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.0524789
> #VaR 95%
> (var_95_weib_btclosses <- qweibull(0.95, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.06968965
> #VaR 99%
> (var_99_weib_btclosses <- qweibull(0.99, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.1107764
> #VaR 99.5%
> (var_95.5_weib_btclosses <- qweibull(0.995, shape = shape.loss_weib, scale = scale.loss_weib))
[1] 0.1288485
> f = function(x) qweibull(x, shape = shape.loss_weib, scale = scale.loss_weib)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01512909 with absolute error < 0.0000000034
> (tvar_70_weib_btclosses <- (1/(1-0.7))*0.01512909)
[1] 0.0504303
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01207507 with absolute error < 0.0000000021
> (tvar_80_weib_btclosses <- (1/(1-0.8))*0.01207507)
[1] 0.06037535
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007767592 with absolute error < 0.000062
> (tvar_90_weib_btclosses <- (1/(1-0.9))*0.007767592)
[1] 0.07767592
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004764149 with absolute error < 0.000062
> (tvar_95_weib_btclosses <- (1/(1-0.95))*0.004764149)
[1] 0.09528298
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001370655 with absolute error < 9.8e-05
> (tvar_99_weib_btclosses <- (1/(1-0.99))*0.001370655)
[1] 0.1370655
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0007769126 with absolute error < 4.9e-05
> (tvar_99.5_weib_btclosses <- (1/(1-0.995))*0.0007769126)
[1] 0.1553825

 
BURR:
Goodness of fit
> btclosses_burr = fitdist(btclosses, "burr", start = list(shape1 = 57, scale = 1.5, shape2 = 1), method = "mle")
> summary(btclosses_burr)
Fitting of the distribution ' burr ' by maximum likelihood 
Parameters : 
        estimate Std. Error
shape1 5.6160354 1.86149347
scale  0.0973585 0.03975707
shape2 1.0472491 0.04293749
Loglikelihood:  2498.542   AIC:  -4991.084   BIC:  -4976.727 
Correlation matrix:
           shape1      scale     shape2
shape1  1.0000000  0.9920380 -0.7600885
scale   0.9920380  1.0000000 -0.8102987
shape2 -0.7600885 -0.8102987  1.0000000
> gofstat(btclosses_burr, fitnames = "Fitting Burr to BTC losses")
Goodness-of-fit statistics
                             Fitting Burr to BTC losses
Kolmogorov-Smirnov statistic                 0.01431975
Cramer-von Mises statistic                   0.02145659
Anderson-Darling statistic                   0.17215374

Goodness-of-fit criteria
                               Fitting Burr to BTC losses
Akaike's Information Criterion                  -4991.084
Bayesian Information Criterion                  -4976.727
Risk measures
> #VaR 70%
  > (var_70_burr_btclosses <- qburr(0.70, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.02483028
> #VaR 80%
  > (var_80_burr_btclosses <- qburr(0.80, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.03395833
> #VaR 90%
  > (var_90_burr_btclosses <- qburr(0.90, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.05087968
> #VaR 95%
  > (var_95_burr_btclosses <- qburr(0.95, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.06970622
> #VaR 99%
  > (var_99_burr_btclosses <- qburr(0.99, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.1223659
> #VaR 99.5%
  > (var_95.5_burr_btclosses <- qburr(0.995, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr))
[1] 0.1496614
> f = function(x) qburr(x, shape1.loss_burr, shape2.loss_burr, scale = scale.loss_burr)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01518306 with absolute error < 0.0000000081
> (tvar_70_burr_btclosses <- (1/(1-0.7))*0.01518306)
[1] 0.0506102
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01227874 with absolute error < 0.0000000052
> (tvar_80_burr_btclosses <- (1/(1-0.8))*0.01227874)
[1] 0.0613937
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.008148385 with absolute error < 0.0000000025
> (tvar_90_burr_btclosses <- (1/(1-0.9))*0.008148385)
[1] 0.08148385
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.005195831 with absolute error < 7.6e-05
> (tvar_95_burr_btclosses <- (1/(1-0.95))*0.005195831)
[1] 0.1039166
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001668701 with absolute error < 0.00011
> (tvar_99_burr_btclosses <- (1/(1-0.99))*0.001668701)
[1] 0.1668701
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0009977366 with absolute error < 0.00011
> (tvar_99.5_burr_btclosses <- (1/(1-0.995))*0.0009977366)
[1] 0.1995473

GENERALIZED PARETO:
  Goodness of fit
> btclosses_GenPar = fitdist(btclosses, "genpareto",start = list(shape1=1 ,shape2=1 ,rate=1),
                             +                             method = "mle")
> summary(btclosses_GenPar)
Fitting of the distribution ' genpareto ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape1 6.578095 1.86037229
shape2 1.056480 0.05933925
rate   8.577980 3.08798584
Loglikelihood:  2498.4   AIC:  -4990.801   BIC:  -4976.444 
Correlation matrix:
  shape1     shape2       rate
shape1  1.0000000 -0.5469156 -0.9835765
shape2 -0.5469156  1.0000000  0.6614291
rate   -0.9835765  0.6614291  1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_GenPar, fitnames = "Fitting Gen Pareto to BTC losses")
Goodness-of-fit statistics
Fitting Gen Pareto to BTC losses
Kolmogorov-Smirnov statistic                       0.01391343
Cramer-von Mises statistic                         0.02183925
Anderson-Darling statistic                         0.17670263

Goodness-of-fit criteria
Fitting Gen Pareto to BTC losses
Akaike's Information Criterion                        -4990.801
Bayesian Information Criterion                        -4976.444
Risk measures
> #VaR 70%
> (var_70_genpareto_btclosses <- qgenpareto(0.70, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.02485309
> #VaR 80%
> (var_80_genpareto_btclosses <- qgenpareto(0.80, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.03404922
> #VaR 90%
> (var_90_genpareto_btclosses <- qgenpareto(0.90, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.05107841
> #VaR 95%
> (var_95_genpareto_btclosses <- qgenpareto(0.95, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.0699489
> #VaR 99%
> (var_99_genpareto_btclosses <- qgenpareto(0.99, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.1221383
> #VaR 99.5%
> (var_95.5_genpareto_btclosses <- qgenpareto(0.995, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar))
[1] 0.1488308
> f = function(x) qgenpareto(x, shape1.loss_genpar, shape2.loss_genpar, rate.loss_genpar)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.0152026 with absolute error < 0.00000000046
> (tvar_70_genpareto_btclosses <- (1/(1-0.7))*0.0152026)
[1] 0.05067533
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01229288 with absolute error < 0.00000000028
> (tvar_80_genpareto_btclosses <- (1/(1-0.8))*0.01229288)
[1] 0.0614644
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.008148387 with absolute error < 0.00000000012
> (tvar_90_genpareto_btclosses <- (1/(1-0.9))*0.008148387)
[1] 0.08148387
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.00518459 with absolute error < 7.1e-05
> (tvar_95_genpareto_btclosses <- (1/(1-0.95))*0.00518459)
[1] 0.1036918
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001651509 with absolute error < 0.00011
> (tvar_99_genpareto_btclosses <- (1/(1-0.99))*0.001651509)
[1] 0.1651509
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0009828906 with absolute error < 0.00011
> (tvar_99.5_genpareto_btclosses <- (1/(1-0.995))*0.0009828906)
[1] 0.1965781

INVERSE BURR:
Goodness of fit
> btclosses_invburr = fitdist(btclosses, "invburr",start = list(shape1=1 ,shape2=1 ,rate=1),
+                            method = "mle")
> summary(btclosses_invburr)
Fitting of the distribution ' invburr ' by maximum likelihood 
Parameters : 
       estimate Std. Error
shape1  0.39680 0.04475195
shape2  2.23067 0.15638863
rate   34.42124 2.92955318
Loglikelihood:  2493.896   AIC:  -4981.793   BIC:  -4967.436 
Correlation matrix:
           shape1     shape2       rate
shape1  1.0000000 -0.9068814  0.8949923
shape2 -0.9068814  1.0000000 -0.7822765
rate    0.8949923 -0.7822765  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_invburr, fitnames = "Fitting Inv Burr to BTC losses")
Goodness-of-fit statistics
                             Fitting Inv Burr to BTC losses
Kolmogorov-Smirnov statistic                     0.02828496
Cramer-von Mises statistic                       0.10601982
Anderson-Darling statistic                       0.68718090

Goodness-of-fit criteria
                               Fitting Inv Burr to BTC losses
Akaike's Information Criterion                      -4981.793
Bayesian Information Criterion                      -4967.436
Risk measures
> #VaR 70%
  > (var_70_invburr_btclosses <- qinvburr(0.70, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.02454233
> #VaR 80%
  > (var_80_invburr_btclosses <- qinvburr(0.80, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.03295639
> #VaR 90%
  > (var_90_invburr_btclosses <- qinvburr(0.90, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.04953644
> #VaR 95%
  > (var_95_invburr_btclosses <- qinvburr(0.95, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.07059436
> #VaR 99%
  > (var_99_invburr_btclosses <- qinvburr(0.99, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.1500922
> #VaR 99.5%
  > (var_95.5_invburr_btclosses <- qinvburr(0.995, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr))
[1] 0.2056071
> f = function(x) qinvburr(x, shape1.loss_invburr, shape2.loss_invburr, rate.loss_invburr)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01633221 with absolute error < 0.000000012
> (tvar_70_invburr_btclosses <- (1/(1-0.7))*0.01633221 )
[1] 0.0544407
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.013491 with absolute error < 0.0000000061
> (tvar_80_invburr_btclosses <- (1/(1-0.8))*0.013491 )
[1] 0.067455
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.009489987 with absolute error < 0.0000000021
> (tvar_90_invburr_btclosses <- (1/(1-0.9))*0.009489987 )
[1] 0.09489987
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.006569703 with absolute error < 0.00000000071
> (tvar_95_invburr_btclosses <- (1/(1-0.95))*0.006569703)
[1] 0.1313941
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.002734502 with absolute error < 0.000000000059
> (tvar_99_invburr_btclosses <- (1/(1-0.99))*0.002734502)
[1] 0.2734502
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.001865349 with absolute error < 0.00012
> (tvar_99.5_invburr_btclosses <- (1/(1-0.995))*0.001865349)
[1] 0.3730698


INVERSE EXPONENTIAL:
  Goodness of fit
> btclosses_InvExp = fitdist(btclosses, "invexp",start = list(scale=1), method = "mle")
> summary(btclosses_InvExp)
Fitting of the distribution ' invexp ' by maximum likelihood 
Parameters : 
  estimate       Std. Error
scale 0.0009607289 0.00000004315836
Loglikelihood:  1229.478   AIC:  -2456.955   BIC:  -2452.17 
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_InvExp, fitnames = "Fitting inverse exponential to BTC losses")
Goodness-of-fit statistics
Fitting inverse exponential to BTC losses
Kolmogorov-Smirnov statistic                                 0.6191886
Cramer-von Mises statistic                                 149.6135883
Anderson-Darling statistic                                 932.8604541

Goodness-of-fit criteria
Fitting inverse exponential to BTC losses
Akaike's Information Criterion                                 -2456.955
Bayesian Information Criterion                                 -2452.170
Risk measures
> #VaR 70%
> (var_70_invexp_btclosses <- qinvexp(0.70, scale = scale.loss_invexp))
[1] 0.00269357
> #VaR 80%
> (var_80_invexp_btclosses <- qinvexp(0.80, scale = scale.loss_invexp))
[1] 0.00430543
> #VaR 90%
> (var_90_invexp_btclosses <- qinvexp(0.90, scale = scale.loss_invexp))
[1] 0.009118491
> #VaR 95%
> (var_95_invexp_btclosses <- qinvexp(0.95, scale = scale.loss_invexp))
[1] 0.01873011
> #VaR 99%
> (var_99_invexp_btclosses <- qinvexp(0.99, scale = scale.loss_invexp))
[1] 0.09559172
> #VaR 99.5%
> (var_95.5_invexp_btclosses <- qinvexp(0.995, scale = scale.loss_invexp))
[1] 0.191665
> f = function(x) qinvexp(x, scale = scale.loss_invexp)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent

 
INVERSE GAMMA:
Goodness of fit
> btclosses_InvGamma = fitdist(btclosses, "invgamma",start = list(shape=1 ,rate=1), method = "mle")
> summary(btclosses_InvGamma)
Fitting of the distribution ' invgamma ' by maximum likelihood 
Parameters : 
          estimate   Std. Error
shape    0.3521329   0.01342437
rate  1630.2507159 112.08032806
Loglikelihood:  1879.94   AIC:  -3755.881   BIC:  -3746.309 
Correlation matrix:
           shape       rate
shape  1.0000000 -0.5603377
rate  -0.5603377  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_InvGamma, fitnames = "Fitting inverse gamma to BTC losses")
Goodness-of-fit statistics
                             Fitting inverse gamma to BTC losses
Kolmogorov-Smirnov statistic                           0.2744726
Cramer-von Mises statistic                            24.8119966
Anderson-Darling statistic                           124.6890469

Goodness-of-fit criteria
                               Fitting inverse gamma to BTC losses
Akaike's Information Criterion                           -3755.881
Bayesian Information Criterion                           -3746.309
Risk measures
> #VaR 70%
  > (var_70_invgamma_btclosses <- qinvgamma(0.70, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 0.02554936
> #VaR 80%
  > (var_80_invgamma_btclosses <- qinvgamma(0.80, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 0.08179503
> #VaR 90%
  > (var_90_invgamma_btclosses <- qinvgamma(0.90, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 0.588402
> #VaR 95%
  > (var_95_invgamma_btclosses <- qinvgamma(0.95, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 4.215396
> #VaR 99%
  > (var_99_invgamma_btclosses <- qinvgamma(0.99, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 407.2355
> #VaR 99.5%
  > (var_95.5_invgamma_btclosses <- qinvgamma(0.995, shape = shape.loss_invgamma, rate = rate.loss_invgamma))
[1] 2915.561
> f = function(x) qinvgamma(x, shape = shape.loss_invgamma, rate = rate.loss_invgamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent


INVERSE GAUSSIAN:
  Goodness of fit
> btclosses_InvGauss = fitdist(btclosses, "invgauss",start = list(mean = 1 , shape = 0.1), method = "mle")
> summary(btclosses_InvGauss)
Fitting of the distribution ' invgauss ' by maximum likelihood 
Parameters : 
  estimate       Std. Error
mean  0.022177308 0.00251773339049
shape 0.001899049 0.00000004315835
Loglikelihood:  1869.704   AIC:  -3735.408   BIC:  -3725.837 
Correlation matrix:
  mean          shape
mean  1.000000000000 0.000000340831
shape 0.000000340831 1.000000000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_InvGauss, fitnames = "Fitting inverse Gaussian to BTC losses")
Goodness-of-fit statistics
Fitting inverse Gaussian to BTC losses
Kolmogorov-Smirnov statistic                              0.3792175
Cramer-von Mises statistic                               52.6005588
Anderson-Darling statistic                              245.5463082

Goodness-of-fit criteria
Fitting inverse Gaussian to BTC losses
Akaike's Information Criterion                              -3735.408
Bayesian Information Criterion                              -3725.837
Risk measures
> #VaR 70%
> (var_70_invgauss_btclosses <- qinvgauss(0.70, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.0089956
> #VaR 80%
> (var_80_invgauss_btclosses <- qinvgauss(0.80, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.01723483
> #VaR 90%
> (var_90_invgauss_btclosses <- qinvgauss(0.90, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.04474686
> #VaR 95%
> (var_95_invgauss_btclosses <- qinvgauss(0.95, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.09731119
> #VaR 99%
> (var_99_invgauss_btclosses <- qinvgauss(0.99, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.3436876
> #VaR 99.5%
> (var_95.5_invgauss_btclosses <- qinvgauss(0.995, mean.loss_invgauss, shape.loss_invgauss))
[1] 0.5015982
> f = function(x) qinvgauss(x, mean.loss_invgauss, shape.loss_invgauss)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.02029713 with absolute error < 0.00000064
> (tvar_70_invgauss_btclosses <- (1/(1-0.7))*0.02029713 )
[1] 0.0676571
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01904968 with absolute error < 0.00000038
> (tvar_80_invgauss_btclosses <- (1/(1-0.8))*0.01904968 )
[1] 0.0952484
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.01627678 with absolute error < 0.00000015
> (tvar_90_invgauss_btclosses <- (1/(1-0.9))*0.01627678 )
[1] 0.1627678
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.01299628 with absolute error < 0.000000062
> (tvar_95_invgauss_btclosses <- (1/(1-0.95))*0.01299628 )
[1] 0.2599256
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.00601428 with absolute error < 0.000088
> (tvar_99_invgauss_btclosses <- (1/(1-0.99))*0.00601428 )
[1] 0.601428
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.003957892 with absolute error < 0.000088
> (tvar_99.5_invgauss_btclosses <- (1/(1-0.995))*0.003957892 )
[1] 0.7915784

 
PARALOGISTIC:
Goodness of fit
> btclosses_para = fitdist(btclosses, "paralogis",start = list(shape=1 ,rate=1), method = "mle")
> summary(btclosses_para)
Fitting of the distribution ' paralogis ' by maximum likelihood 
Parameters : 
      estimate Std. Error
shape  1.34296 0.02941359
rate  56.23715 2.52330331
Loglikelihood:  2477.025   AIC:  -4950.05   BIC:  -4940.479 
Correlation matrix:
           shape       rate
shape  1.0000000 -0.4726025
rate  -0.4726025  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_para, fitnames = "Fitting Paralogistic to BTC losses")
Goodness-of-fit statistics
                             Fitting Paralogistic to BTC losses
Kolmogorov-Smirnov statistic                         0.02979169
Cramer-von Mises statistic                           0.20788559
Anderson-Darling statistic                           2.45012240

Goodness-of-fit criteria
                               Fitting Paralogistic to BTC losses
Akaike's Information Criterion                          -4950.050
Bayesian Information Criterion                          -4940.479
Risk measures
> #VaR 70%
  > (var_70_para_btclosses <- qparalogis(0.70, shape.loss_para, rate.loss_para))
[1] 0.02346194
> #VaR 80%
  > (var_80_para_btclosses <- qparalogis(0.80, shape.loss_para, rate.loss_para))
[1] 0.0332212
> #VaR 90%
  > (var_90_para_btclosses <- qparalogis(0.90, shape.loss_para, rate.loss_para))
[1] 0.05498549
> #VaR 95%
  > (var_95_para_btclosses <- qparalogis(0.95, shape.loss_para, rate.loss_para))
[1] 0.08601816
> #VaR 99%
  > (var_99_para_btclosses <- qparalogis(0.99, shape.loss_para, rate.loss_para))
[1] 0.2229716
> #VaR 99.5%
  > (var_95.5_para_btclosses <- qparalogis(0.995, shape.loss_para, rate.loss_para))
[1] 0.3307491
> f = function(x) qparalogis(x, shape.loss_para, rate.loss_para)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.02058915 with absolute error < 0.00000018
> (tvar_70_para_btclosses <- (1/(1-0.7))*0.02058915  )
[1] 0.0686305
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01780268 with absolute error < 0.00000011
> (tvar_80_para_btclosses <- (1/(1-0.8))*0.01780268 )
[1] 0.0890134
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.01357855 with absolute error < 0.000000047
> (tvar_90_para_btclosses <- (1/(1-0.9))*0.01357855  )
[1] 0.1357855
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.01018862 with absolute error < 0.000000021
> (tvar_95_para_btclosses <- (1/(1-0.95))*0.01018862 )
[1] 0.2037724
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.00508243 with absolute error < 0.0000000031
> (tvar_99_para_btclosses <- (1/(1-0.99))*0.00508243 )
[1] 0.508243
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.003745865 with absolute error < 0.0000000013
> (tvar_99.5_para_btclosses <- (1/(1-0.995))*0.003745865 )
[1] 0.749173


LOGNORMAL:
  Goodness of fit
> btclosses_lnorm = fitdist(btclosses, "lnorm", method = "mle")
> summary(btclosses_lnorm)
Fitting of the distribution ' lnorm ' by maximum likelihood 
Parameters : 
  estimate Std. Error
meanlog -4.442977 0.04411197
sdlog    1.312285 0.03119179
Loglikelihood:  2435.758   AIC:  -4867.516   BIC:  -4857.944 
Correlation matrix:
  meanlog sdlog
meanlog       1     0
sdlog         0     1
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_lnorm, fitnames = "Fitting Lognormal to BTC losses")
Goodness-of-fit statistics
Fitting Lognormal to BTC losses
Kolmogorov-Smirnov statistic                      0.06628279
Cramer-von Mises statistic                        1.25217009
Anderson-Darling statistic                        7.96672223

Goodness-of-fit criteria
Fitting Lognormal to BTC losses
Akaike's Information Criterion                       -4867.516
Bayesian Information Criterion                       -4857.944
Risk measures
> #VaR 70%
> (var_70_lnorm_btclosses <- qlnorm(0.70, mulog.loss, sdlog.loss))
[1] 0.0234048
> #VaR 80%
> (var_80_lnorm_btclosses <- qlnorm(0.80, mulog.loss, sdlog.loss))
[1] 0.03548908
> #VaR 90%
> (var_90_lnorm_btclosses <- qlnorm(0.90, mulog.loss, sdlog.loss))
[1] 0.06321483
> #VaR 95%
> (var_95_lnorm_btclosses <- qlnorm(0.95, mulog.loss, sdlog.loss))
[1] 0.101829
> #VaR 99%
> (var_99_lnorm_btclosses <- qlnorm(0.99, mulog.loss, sdlog.loss))
[1] 0.249039
> #VaR 99.5%
> (var_95.5_lnorm_btclosses <- qlnorm(0.995, mulog.loss, sdlog.loss))
[1] 0.3455025
> f = function(x) qlnorm(x, mulog.loss, sdlog.loss)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.02182956 with absolute error < 0.0000028
> (tvar_70_lnorm_btclosses <- (1/(1-0.7))*0.02182956)
[1] 0.0727652
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01894838 with absolute error < 0.0000019
> (tvar_80_lnorm_btclosses <- (1/(1-0.8))*0.01894838)
[1] 0.0947419
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.01425202 with absolute error < 0.000001
> (tvar_90_lnorm_btclosses <- (1/(1-0.9))*0.01425202)
[1] 0.1425202
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.01028659 with absolute error < 0.00000055
> (tvar_95_lnorm_btclosses <- (1/(1-0.95))*0.01028659)
[1] 0.2057318
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.004320085 with absolute error < 0.00000013
> (tvar_99_lnorm_btclosses <- (1/(1-0.99))*0.004320085)
[1] 0.4320085
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.002870681 with absolute error < 0.000081
> (tvar_99.5_lnorm_btclosses <- (1/(1-0.995))*0.002870681)
[1] 0.5741362

 
INVERSE WEIBULL:
Goodness of fit
> btclosses_invweib = fitdist(btclosses, "invweibull", method = "mle")
> summary(btclosses_invweib)
Fitting of the distribution ' invweibull ' by maximum likelihood 
Parameters : 
         estimate   Std. Error
shape 0.560141279 0.0111318163
scale 0.005793205 0.0003518907
Loglikelihood:  2183.079   AIC:  -4362.157   BIC:  -4352.586 
Correlation matrix:
           shape      scale
shape  1.0000000 -0.3187477
scale -0.3187477  1.0000000

> shape.loss_invweib = btclosses_invweib$estimate[1]
> scale.loss_invweib = btclosses_invweib$estimate[2]
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_invweib, fitnames = "Fitting Inverse Weibull to BTC losses")
Goodness-of-fit statistics
                             Fitting Inverse Weibull to BTC losses
Kolmogorov-Smirnov statistic                             0.1795516
Cramer-von Mises statistic                               8.5526456
Anderson-Darling statistic                              52.2973373

Goodness-of-fit criteria
                               Fitting Inverse Weibull to BTC losses
Akaike's Information Criterion                             -4362.157
Bayesian Information Criterion                             -4352.586
Risk measures
> #VaR 70%
  > (var_70_invweib_btclosses <- qinvweibull(0.70, shape.loss_invweib, 1/scale.loss_invweib))
[1] 0.03649476
> #VaR 80%
  > (var_80_invweib_btclosses <- qinvweibull(0.80, shape.loss_invweib, 1/scale.loss_invweib))
[1] 0.08430781
> #VaR 90%
  > (var_90_invweib_btclosses <- qinvweibull(0.90, shape.loss_invweib, 1/scale.loss_invweib))
[1] 0.3218821
> #VaR 95%
  > (var_95_invweib_btclosses <- qinvweibull(0.95, shape.loss_invweib, 1/scale.loss_invweib))
[1] 1.163593
> #VaR 99%
  > (var_99_invweib_btclosses <- qinvweibull(0.99, shape.loss_invweib, 1/scale.loss_invweib))
[1] 21.35767
> #VaR 99.5%
  > (var_95.5_invweib_btclosses <- qinvweibull(0.995, shape.loss_invweib, 1/scale.loss_invweib))
[1] 73.9473
> f = function(x) qinvweibull(x, shape.loss_invweib, 1/scale.loss_invweib)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent


INVERSE PARALOGISTIC:
  Goodness of fit
> btclosses_invpara = fitdist(btclosses, "invparalogis", start = list(shape = 1, rate = 1), method = "mle")
> summary(btclosses_invpara)
Fitting of the distribution ' invparalogis ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape  1.237412 0.02497298
rate  99.148228 4.71812167
Loglikelihood:  2447.017   AIC:  -4890.035   BIC:  -4880.464 
Correlation matrix:
  shape     rate
shape 1.000000 0.401689
rate  0.401689 1.000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_invpara, fitnames = "Fitting Inverse Paralogistic to BTC losses")
Goodness-of-fit statistics
Fitting Inverse Paralogistic to BTC losses
Kolmogorov-Smirnov statistic                                  0.0630530
Cramer-von Mises statistic                                    0.5821397
Anderson-Darling statistic                                    6.0390007

Goodness-of-fit criteria
Fitting Inverse Paralogistic to BTC losses
Akaike's Information Criterion                                  -4890.035
Bayesian Information Criterion                                  -4880.464
Risk measures
> #VaR 70%
> (var_70_invpara_btclosses <- qinvparalogis(0.70, shape.loss_invpara, rate.loss_invpara))
[1] 0.02446294
> #VaR 80%
> (var_80_invpara_btclosses <- qinvparalogis(0.80, shape.loss_invpara, rate.loss_invpara))
[1] 0.0373932
> #VaR 90%
> (var_90_invpara_btclosses <- qinvparalogis(0.90, shape.loss_invpara, rate.loss_invpara))
[1] 0.07132522
> #VaR 95%
> (var_95_invpara_btclosses <- qinvparalogis(0.95, shape.loss_invpara, rate.loss_invpara))
[1] 0.1299061
> #VaR 99%
> (var_99_invpara_btclosses <- qinvparalogis(0.99, shape.loss_invpara, rate.loss_invpara))
[1] 0.4915501
> #VaR 99.5%
> (var_95.5_invpara_btclosses <- qinvparalogis(0.995, shape.loss_invpara, rate.loss_invpara))
[1] 0.863853
> f = function(x) qinvparalogis(x, shape.loss_invpara, rate.loss_invpara)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.04777828 with absolute error < 0.0000011
> (tvar_70_invpara_btclosses <- (1/(1-0.7))*0.04777828 )
[1] 0.1592609
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.04476146 with absolute error < 0.00000071
> (tvar_80_invpara_btclosses <- (1/(1-0.8))*0.04476146 )
[1] 0.2238073
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.03966956 with absolute error < 0.00000031
> (tvar_90_invweib_btclosses <- (1/(1-0.9))*0.03966956 )
[1] 0.3966956
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.03493848 with absolute error < 0.00000014
> (tvar_95_invpara_btclosses <- (1/(1-0.95))*0.03493848 )
[1] 0.6987696
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.02577846 with absolute error < 0.00000002
> (tvar_99_invpara_btclosses <- (1/(1-0.99))*0.02577846 )
[1] 2.577846
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.02258171 with absolute error < 0.0000000088
> (tvar_99.5_invpara_btclosses <- (1/(1-0.995))*0.02258171 )
[1] 4.516342

 
PARETO:
Goodness of fit
> btclosses_pareto = fitdist(btclosses, "pareto", method = "mle")
> summary(btclosses_pareto)
Fitting of the distribution ' pareto ' by maximum likelihood 
Parameters : 
       estimate Std. Error
shape 7.6883099  2.0607047
scale 0.1476386  0.0443583
Loglikelihood:  2497.913   AIC:  -4991.826   BIC:  -4982.255 
Correlation matrix:
          shape     scale
shape 1.0000000 0.9921046
scale 0.9921046 1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_pareto, fitnames = "Fitting Pareto to BTC losses")
Goodness-of-fit statistics
                             Fitting Pareto to BTC losses
Kolmogorov-Smirnov statistic                   0.02080646
Cramer-von Mises statistic                     0.04721055
Anderson-Darling statistic                     0.35598551

Goodness-of-fit criteria
                               Fitting Pareto to BTC losses
Akaike's Information Criterion                    -4991.826
Bayesian Information Criterion                    -4982.255
Risk measures
> #VaR 70%
  > (var_70_pareto_btclosses <- qpareto(0.70, shape.loss_pareto, scale.loss_pareto))
[1] 0.02502846
> #VaR 80%
  > (var_80_pareto_btclosses <- qpareto(0.80, shape.loss_pareto, scale.loss_pareto))
[1] 0.03437894
> #VaR 90%
  > (var_90_pareto_btclosses <- qpareto(0.90, shape.loss_pareto, scale.loss_pareto))
[1] 0.05155138
> #VaR 95%
  > (var_95_pareto_btclosses <- qpareto(0.95, shape.loss_pareto, scale.loss_pareto))
[1] 0.07034395
> #VaR 99%
  > (var_99_pareto_btclosses <- qpareto(0.99, shape.loss_pareto, scale.loss_pareto))
[1] 0.1211031
> #VaR 99.5%
  > (var_95.5_pareto_btclosses <- qpareto(0.995, shape.loss_pareto, scale.loss_pareto))
[1] 0.1464575
> f = function(x) qpareto(x, shape.loss_pareto, scale.loss_pareto)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01525341 with absolute error < 0.00000000000000063
> (tvar_70_pareto_btclosses <- (1/(1-0.7))*0.01525341 )
[1] 0.0508447
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01231864 with absolute error < 0.00000000000000058
> (tvar_80_pareto_btclosses <- (1/(1-0.8))*0.01231864 )
[1] 0.0615932
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.008132988 with absolute error < 0.00011
> (tvar_90_pareto_btclosses <- (1/(1-0.9))*0.008132988 )
[1] 0.08132988
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.005146445 with absolute error < 0.00011
> (tvar_95_pareto_btclosses <- (1/(1-0.95))*0.005146445 )
[1] 0.1029289
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001612567 with absolute error < 0.000094
> (tvar_99_pareto_btclosses <- (1/(1-0.99))*0.001612567 )
[1] 0.1612567
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0009518736 with absolute error < 0.000094
> (tvar_99.5_pareto_btclosses <- (1/(1-0.995))*0.0009518736 )
[1] 0.1903747


BETA:
  Goodness of fit
> btclosses_beta = fitdist(btclosses, "beta", method = "mle")
> summary(btclosses_beta)
Fitting of the distribution ' beta ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape1  0.892219 0.03696612
shape2 38.970246 2.11165982
Loglikelihood:  2480.6   AIC:  -4957.201   BIC:  -4947.63 
Correlation matrix:
  shape1    shape2
shape1 1.0000000 0.7548865
shape2 0.7548865 1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_beta, fitnames = "Fitting Beta to BTC losses")
Goodness-of-fit statistics
Fitting Beta to BTC losses
Kolmogorov-Smirnov statistic                   0.03459756
Cramer-von Mises statistic                     0.31206644
Anderson-Darling statistic                     1.67937671

Goodness-of-fit criteria
Fitting Beta to BTC losses
Akaike's Information Criterion                    -4957.201
Bayesian Information Criterion                    -4947.630
Risk measures
> #VaR 70%
> (var_70_beta_btclosses <- qbeta(0.70, shape1.loss_beta, shape2.loss_beta))
[1] 0.02697743
> #VaR 80%
> (var_80_beta_btclosses <- qbeta(0.80, shape1.loss_beta, shape2.loss_beta))
[1] 0.03653191
> #VaR 90%
> (var_90_beta_btclosses <- qbeta(0.90, shape1.loss_beta, shape2.loss_beta))
[1] 0.05284167
> #VaR 95%
> (var_95_beta_btclosses <- qbeta(0.95, shape1.loss_beta, shape2.loss_beta))
[1] 0.06902322
> #VaR 99%
> (var_99_beta_btclosses <- qbeta(0.99, shape1.loss_beta, shape2.loss_beta))
[1] 0.1058423
> #VaR 99.5%
> (var_95.5_beta_btclosses <- qbeta(0.995, shape1.loss_beta, shape2.loss_beta))
[1] 0.121328
> f = function(x) qbeta(x, shape1.loss_beta, shape2.loss_beta)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01511004 with absolute error < 0.00000000054
> (tvar_70_beta_btclosses <- (1/(1-0.7))*0.01511004  )
[1] 0.0503668
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01196682 with absolute error < 0.000099
> (tvar_80_beta_btclosses <- (1/(1-0.8))*0.01196682  )
[1] 0.0598341
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007590916 with absolute error < 0.000099
> (tvar_90_beta_btclosses <- (1/(1-0.9))*0.007590916  )
[1] 0.07590916
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004589985 with absolute error < 0.000099
> (tvar_95_beta_btclosses <- (1/(1-0.95))*0.004589985  )
[1] 0.0917997
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001278522 with absolute error < 0.000079
> (tvar_99_beta_btclosses <- (1/(1-0.99))*0.001278522  )
[1] 0.1278522
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0007149756 with absolute error < 0.000039
> (tvar_99.5_beta_btclosses <- (1/(1-0.995))*0.0007149756 )
[1] 0.1429951

 
UNIFORM:
Goodness of fit
> btclosses_uni = fitdist(btclosses, "unif", method = "mle")
> summary(btclosses_uni)
Fitting of the distribution ' unif ' by maximum likelihood 
Parameters : 
          estimate
min 0.000004631763
max 0.501836583000
Loglikelihood:  610.1986   AIC:  -1216.397   BIC:  -1206.826 
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_uni, fitnames = "Fitting Uniform to BTC losses")
Goodness-of-fit statistics
                             Fitting Uniform to BTC losses
Kolmogorov-Smirnov statistic                     0.8226198
Cramer-von Mises statistic                     239.6601160
Anderson-Darling statistic                             Inf

Goodness-of-fit criteria
                               Fitting Uniform to BTC losses
Akaike's Information Criterion                     -1216.397
Bayesian Information Criterion                     -1206.826
Risk measures
> #VaR 70%
  > (var_70_uni_btclosses <- qunif(0.70, min.loss_uni, max.loss_uni))
[1] 0.351287
> #VaR 80%
  > (var_80_uni_btclosses <- qunif(0.80, min.loss_uni, max.loss_uni))
[1] 0.4014702
> #VaR 90%
  > (var_90_uni_btclosses <- qunif(0.90, min.loss_uni, max.loss_uni))
[1] 0.4516534
> #VaR 95%
  > (var_95_uni_btclosses <- qunif(0.95, min.loss_uni, max.loss_uni))
[1] 0.476745
> #VaR 99%
  > (var_99_uni_btclosses <- qunif(0.99, min.loss_uni, max.loss_uni))
[1] 0.4968183
> #VaR 99.5%
  > (var_95.5_uni_btclosses <- qunif(0.995, min.loss_uni, max.loss_uni))
[1] 0.4993274
> f = function(x) qunif(x, min.loss_uni, max.loss_uni)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.1279685 with absolute error < 0.0000000000000014
> (tvar_70_uni_btclosses <- (1/(1-0.7))*0.1279685)
[1] 0.4265617
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.09033068 with absolute error < 0.000000000000001
> (tvar_80_uni_btclosses <- (1/(1-0.8))*0.09033068)
[1] 0.4516534
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.0476745 with absolute error < 0.00000000000000053
> (tvar_90_uni_btclosses <- (1/(1-0.9))*0.0476745)
[1] 0.476745
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.02446454 with absolute error < 0.00000000000000027
> (tvar_95_uni_btclosses <- (1/(1-0.95))*0.02446454)
[1] 0.4892908
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.004993274 with absolute error < 0.000000000000000055
> (tvar_99_uni_btclosses <- (1/(1-0.99))*0.004993274)
[1] 0.4993274
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.00250291 with absolute error < 0.000000000000000028
> (tvar_99.5_uni_btclosses <- (1/(1-0.995))*0.00250291)
[1] 0.500582


INVERSE PARETO:
  Goodness of fit
> btclosses_invpare = fitdist(btclosses, "invpareto", start = list(shape = 1, scale = 1), method = "mle")
> summary(btclosses_invpare)
Fitting of the distribution ' invpareto ' by maximum likelihood 
Parameters : 
  estimate   Std. Error
shape 1.492800432 0.0989459576
scale 0.007526883 0.0007165126
Loglikelihood:  2413.787   AIC:  -4823.573   BIC:  -4814.002 
Correlation matrix:
  shape      scale
shape  1.0000000 -0.8618609
scale -0.8618609  1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_invpare, fitnames = "Fitting Inverse Pareto to BTC losses")
Goodness-of-fit statistics
Fitting Inverse Pareto to BTC losses
Kolmogorov-Smirnov statistic                            0.1030406
Cramer-von Mises statistic                              1.8314585
Anderson-Darling statistic                             14.4763088

Goodness-of-fit criteria
Fitting Inverse Pareto to BTC losses
Akaike's Information Criterion                            -4823.573
Bayesian Information Criterion                            -4814.002
Risk measures
> (var_70_invpare_btclosses <- qinvpareto(0.70, shape.loss_invpare, scale.loss_invpare))
[1] 0.02788873
> #VaR 80%
> (var_80_invpare_btclosses <- qinvpareto(0.80, shape.loss_invpare, scale.loss_invpare))
[1] 0.04668412
> #VaR 90%
> (var_90_invpare_btclosses <- qinvpareto(0.90, shape.loss_invpare, scale.loss_invpare))
[1] 0.1029255
> #VaR 95%
> (var_95_invpare_btclosses <- qinvpareto(0.95, shape.loss_invpare, scale.loss_invpare))
[1] 0.2153147
> #VaR 99%
> (var_99_invpare_btclosses <- qinvpareto(0.99, shape.loss_invpare, scale.loss_invpare))
[1] 1.114227
> #VaR 99.5%
> (var_95.5_invpare_btclosses <- qinvpareto(0.995, shape.loss_invpare, scale.loss_invpare))
[1] 2.237843
> f = function(x) qinvpareto(x, shape.loss_invpare, scale.loss_invpare)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : non-finite function value
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : non-finite function value
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : non-finite function value
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  non-finite function value
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  non-finite function value
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  non-finite function value

 
TRANSFORMED GAMMA:
Goodness of fit
> btclosses_trgamma = fitdist(btclosses, "trgamma", start = list(shape1 = 1, shape2 = 1, rate = 1), method = "mle")
> summary(btclosses_trgamma)
Fitting of the distribution ' trgamma ' by maximum likelihood 
Parameters : 
         estimate  Std. Error
shape1   1.709075  0.32549367
shape2   0.686191  0.07530251
rate   116.439937 47.16020545
Loglikelihood:  2495.601   AIC:  -4985.202   BIC:  -4970.845 
Correlation matrix:
          shape1     shape2       rate
shape1  1.000000 -0.9733330  0.9930650
shape2 -0.973333  1.0000000 -0.9832235
rate    0.993065 -0.9832235  1.0000000
> rate.loss_trgamma = btclosses_trgamma$estimate[3]
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_trgamma, fitnames = "Fitting Transformed Gamma to BTC losses")
Goodness-of-fit statistics
                             Fitting Transformed Gamma to BTC losses
Kolmogorov-Smirnov statistic                              0.01900452
Cramer-von Mises statistic                                0.03756444
Anderson-Darling statistic                                0.28230356

Goodness-of-fit criteria
                               Fitting Transformed Gamma to BTC losses
Akaike's Information Criterion                               -4985.202
Bayesian Information Criterion                               -4970.845
Risk measures
> (var_70_trgamma_btclosses <- qtrgamma(0.70, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.02511389
> #VaR 80%
  > (var_80_trgamma_btclosses <- qtrgamma(0.80, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.0346809
> #VaR 90%
  > (var_90_trgamma_btclosses <- qtrgamma(0.90, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.05221285
> #VaR 95%
  > (var_95_trgamma_btclosses <- qtrgamma(0.95, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.0710793
> #VaR 99%
  > (var_99_trgamma_btclosses <- qtrgamma(0.99, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.1193655
> #VaR 99.5%
  > (var_95.5_trgamma_btclosses <- qtrgamma(0.995, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma))
[1] 0.1418868
> f = function(x) qtrgamma(x, shape1.loss_trgamma, shape2.loss_trgamma, rate.loss_trgamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01528083 with absolute error < 0.000000022
> (tvar_70_trgamma_btclosses <- (1/(1-0.7))*0.01528083  )
[1] 0.0509361
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01232781 with absolute error < 0.000000014
> (tvar_80_trgamma_btclosses <- (1/(1-0.8))*0.01232781  )
[1] 0.06163905
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.008095172 with absolute error < 0.000084
> (tvar_90_trgamma_btclosses <- (1/(1-0.9))*0.008095172  )
[1] 0.08095172
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.005072148 with absolute error < 0.000084
> (tvar_95_trgamma_btclosses <- (1/(1-0.95))*0.005072148  )
[1] 0.101443
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001530719 with absolute error < 0.000068
> (tvar_99_trgamma_btclosses <- (1/(1-0.99))*0.001530719  )
[1] 0.1530719
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0008844442 with absolute error < 0.000068
> (tvar_99.5_trgamma_btclosses <- (1/(1-0.995))*0.0008844442  )
[1] 0.1768888

TRANSFORMED BETA:
  Goodness of fit
> btclosses_trbeta = fitdist(btclosses, "trbeta", method = "mle")
> summary(btclosses_trbeta)
Fitting of the distribution ' trbeta ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape1 3.95434838 2.18181785
shape2 1.19573361 0.24191176
shape3 0.83530087 0.22507090
scale  0.06981239 0.03578934
Loglikelihood:  2498.737   AIC:  -4989.474   BIC:  -4970.332 
Correlation matrix:
  shape1     shape2     shape3      scale
shape1  1.0000000 -0.9238630  0.8654053  0.9879800
shape2 -0.9238630  1.0000000 -0.9791795 -0.8745255
shape3  0.8654053 -0.9791795  1.0000000  0.7918103
scale   0.9879800 -0.8745255  0.7918103  1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_trbeta, fitnames = "Fitting Transformed Beta to BTC losses")
Goodness-of-fit statistics
Fitting Transformed Beta to BTC losses
Kolmogorov-Smirnov statistic                              0.0171564
Cramer-von Mises statistic                                0.0279132
Anderson-Darling statistic                                0.2015707

Goodness-of-fit criteria
Fitting Transformed Beta to BTC losses
Akaike's Information Criterion                              -4989.474
Bayesian Information Criterion                              -4970.332
Risk measures
> (var_70_trbeta_btclosses <- qtrbeta(0.70, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.02485286
> #VaR 80%
> (var_80_trbeta_btclosses <- qtrbeta(0.80, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.03384403
> #VaR 90%
> (var_90_trbeta_btclosses <- qtrbeta(0.90, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.05050474
> #VaR 95%
> (var_95_trbeta_btclosses <- qtrbeta(0.95, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.06917157
> #VaR 99%
> (var_99_trbeta_btclosses <- qtrbeta(0.99, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.1226958
> #VaR 99.5%
> (var_95.5_trbeta_btclosses <- qtrbeta(0.995, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta))
[1] 0.1513101
> f = function(x) qtrbeta(x, shape1.loss_trbeta, shape2.loss_trbeta, shape3.loss_trbeta, 1/scale.loss_trbeta)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01515602 with absolute error < 0.000000033
> (tvar_70_trbeta_btclosses <- (1/(1-0.7))*0.01515602  )
[1] 0.05052007
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01225552 with absolute error < 0.000000021
> (tvar_80_trbeta_btclosses <- (1/(1-0.8))*0.01225552  )
[1] 0.0612776
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.00814833 with absolute error < 0.00000001
> (tvar_90_trbeta_btclosses <- (1/(1-0.9))*0.00814833  )
[1] 0.0814833
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.00521862 with absolute error < 0.000089
> (tvar_95_trbeta_btclosses <- (1/(1-0.95))*0.00521862  )
[1] 0.1043724
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001707588 with absolute error < 0.000075
> (tvar_99_trbeta_btclosses <- (1/(1-0.99))*0.001707588  )
[1] 0.1707588
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.001032367 with absolute error < 0.000075
> (tvar_99.5_trbeta_btclosses <- (1/(1-0.995))*0.001032367  )
[1] 0.2064734

GENERALIZED BETA:
Goodness of fit
> btclosses_genbeta = fitdist(btclosses, "genbeta", method = "mse")
> summary(btclosses_genbeta)
Parameters : 
          estimate
shape1  756.723463
shape2 7567.208695
shape3    3.000000
scale     8.583274
Loglikelihood:  -Inf   AIC:  Inf   BIC:  Inf 
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_genbeta, fitnames = "Fitting Generalised Beta to BTC losses")
Goodness-of-fit statistics
                             Fitting Generalised Beta to BTC losses
Kolmogorov-Smirnov statistic                                      1
Cramer-von Mises statistic                                      295
Anderson-Darling statistic                                      Inf

Goodness-of-fit criteria
                               Fitting Generalized Beta to BTC losses
Akaike's Information Criterion                                    Inf
Bayesian Information Criterion                                    Inf
Risk measures
> (var_70_genbeta_btclosses <- qgenbeta(0.70, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.882331
> #VaR 80%
  > (var_80_genbeta_btclosses <- qgenbeta(0.80, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.896455
> #VaR 90%
  > (var_90_genbeta_btclosses <- qgenbeta(0.90, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.916023
> #VaR 95%
  > (var_95_genbeta_btclosses <- qgenbeta(0.95, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.932167
> #VaR 99%
  > (var_99_genbeta_btclosses <- qgenbeta(0.99, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.962411
> #VaR 99.5%
  > (var_95.5_genbeta_btclosses <- qgenbeta(0.995, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta))
[1] 3.973469
> f = function(x) qgenbeta(x, shape1.loss_genbeta, shape2.loss_genbeta, shape3.loss_genbeta, 1/scale.loss_genbeta)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
1.173167 with absolute error < 0.00009
> (tvar_70_genbeta_btclosses <- (1/(1-0.7))*1.173167   )
[1] 3.910557
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.7842534 with absolute error < 0.00012
> (tvar_80_genbeta_btclosses <- (1/(1-0.8))*0.7842534   )
[1] 3.921267
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.3937048 with absolute error < 0.00012
> (tvar_90_genbeta_btclosses <- (1/(1-0.9))*0.3937048   )
[1] 3.937048
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.1975354 with absolute error < 0.00012
> (tvar_95_genbeta_btclosses <- (1/(1-0.95))*0.1975354   )
[1] 3.950708
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.03977416 with absolute error < 0.000096
> (tvar_99_genbeta_btclosses <- (1/(1-0.99))*0.03977416   )
[1] 3.977416
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.01993729 with absolute error < 0.000047
> (tvar_99.5_genbeta_btclosses <- (1/(1-0.995))*0.01993729   )
[1] 3.987458


INVERSE TRANSFORMED GAMMA:
  Goodness of fit
> btclosses_invtrgamma = fitdist(btclosses, "invtrgamma", method = "mle")
> summary(btclosses_invtrgamma)
Fitting of the distribution ' invtrgamma ' by maximum likelihood 
Parameters : 
  estimate
shape1     0.005864962
shape2  9765.738733479
scale  97414.517242449
Loglikelihood:  -2147483647   AIC:  4294967300   BIC:  4294967314 
> #Goodness of fit tests (excluding NLL)
  > gofstat(btclosses_invtrgamma, fitnames = "Fitting Inverse Transformed Gamma to BTC losses")
Goodness-of-fit statistics
Fitting Inverse Transformed Gamma to BTC losses
Kolmogorov-Smirnov statistic                                             1
Cramer-von Mises statistic                                             295
Anderson-Darling statistic                                             Inf

Goodness-of-fit criteria
Fitting Inverse Transformed Gamma to BTC losses
Akaike's Information Criterion                                  4294967300
Bayesian Information Criterion                                  4294967314
Risk measures
> (var_70_invtrgamma_btclosses <- qinvtrgamma(0.70, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] 99489.74
> #VaR 80%
> (var_80_invtrgamma_btclosses <- qinvtrgamma(0.80, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] 100196.5
> #VaR 90%
> (var_90_invtrgamma_btclosses <- qinvtrgamma(0.90, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] 101416.5
> #VaR 95%
> (var_95_invtrgamma_btclosses <- qinvtrgamma(0.95, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] 102651.3
> #VaR 99%
> (var_99_invtrgamma_btclosses <- qinvtrgamma(0.99, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] Inf
> #VaR 99.5%
> (var_95.5_invtrgamma_btclosses <- qinvtrgamma(0.995, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma))
[1] Inf
> f = function(x) qinvtrgamma(x, shape1.loss_invtrgamma, shape2.loss_invtrgamma, 1/scale.loss_invtrgamma)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : non-finite function value

 
LOGLOGISTIC:
Goodness of fit
> btclosses_llogis = fitdist(btclosses, "llogis",start = list(shape=1 ,rate=1), method = "mle")
> summary(btclosses_llogis)
Fitting of the distribution ' llogis ' by maximum likelihood 
Parameters : 
       estimate Std. Error
shape  1.408849 0.03957683
rate  77.006052 3.18249810
Loglikelihood:  2462.576   AIC:  -4921.153   BIC:  -4911.582 
Correlation matrix:
            shape        rate
shape  1.00000000 -0.04675275
rate  -0.04675275  1.00000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btclosses_llogis, fitnames = "Fitting Loglogistic to BTC losses")
Goodness-of-fit statistics
                             Fitting Loglogistic to BTC losses
Kolmogorov-Smirnov statistic                        0.04581041
Cramer-von Mises statistic                          0.32449332
Anderson-Darling statistic                          3.88538447

Goodness-of-fit criteria
                               Fitting Loglogistic to BTC losses
Akaike's Information Criterion                         -4921.153
Bayesian Information Criterion                         -4911.582
Risk measures
> #VaR 70%
  > (var_70_llogis_btclosses <- qllogis(0.70, shape.loss_llogis, rate.loss_llogis))
[1] 0.02369545
> #VaR 80%
  > (var_80_llogis_btclosses <- qllogis(0.80, shape.loss_llogis, rate.loss_llogis))
[1] 0.03473898
> #VaR 90%
  > (var_90_llogis_btclosses <- qllogis(0.90, shape.loss_llogis, rate.loss_llogis))
[1] 0.06177262
> #VaR 95%
  > (var_95_llogis_btclosses <- qllogis(0.95, shape.loss_llogis, rate.loss_llogis))
[1] 0.1049866
> #VaR 99%
  > (var_99_llogis_btclosses <- qllogis(0.99, shape.loss_llogis, rate.loss_llogis))
[1] 0.3388255
> #VaR 99.5%
  > (var_95.5_llogis_btclosses <- qllogis(0.995, shape.loss_llogis, rate.loss_llogis))
[1] 0.5561602
> f = function(x) qllogis(x, shape.loss_llogis, rate.loss_llogis)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.0300006 with absolute error < 0.00000027
> (tvar_70_llogis_btclosses <- (1/(1-0.7))*0.0300006   )
[1] 0.100002
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.02713891 with absolute error < 0.00000016
> (tvar_80_llogis_btclosses <- (1/(1-0.8))*0.02713891  )
[1] 0.1356946
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.02256979 with absolute error < 0.000000065
> (tvar_90_llogis_btclosses <- (1/(1-0.9))*0.02256979   )
[1] 0.2256979
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.01860897 with absolute error < 0.000000027
> (tvar_95_llogis_btclosses <- (1/(1-0.95))*0.01860897  )
[1] 0.3721794
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.01174036 with absolute error < 0.0000000033
> (tvar_99_llogis_btclosses <- (1/(1-0.99))*0.01174036  )
[1] 1.174036
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.009608819 with absolute error < 0.0000000014
> (tvar_99.5_llogis_btclosses <- (1/(1-0.995))*0.009608819  )
[1] 1.921764


A7.2: Code for Gains
EMPIRICAL:
  > #########GAINS
  > #Empirical (dataset) 
  > #VaR 70%
  > (var_70_btcgains <- quantile(btcgains, 0.7))
70% 
0.02822963 
> #VaR 80%
  > (var_80_btcgains <- quantile(btcgains, 0.8))
80% 
0.03806874 
> #VaR 90%
  > (var_90_btcgains <- quantile(btcgains, 0.9))
90% 
0.05192224 
> #VaR 95%
  > (var_95_btcgains <- quantile(btcgains, 0.95))
95% 
0.07444624 
> #VaR 99%
  > (var_99_btcgains <- quantile(btcgains, 0.99))
99% 
0.1055778 
> #VaR 99.5%
  > (var_99.5_btcgains <- quantile(btcgains, 0.995))
99.5% 
0.1163957 
> #TVaR 70%
  > (tvar_70_btcgains <- mean(btcgains[btcgains>var_70_btcgains]))
[1] 0.05205079
> #TVaR 80%
  > (tvar_80_btcgains <- mean(btcgains[btcgains>var_80_btcgains]))
[1] 0.0619
> #TVaR 90%
  > (tvar_90_btcgains <- mean(btcgains[btcgains>var_90_btcgains]))
[1] 0.07935107
> #TVaR 95%
  > (tvar_95_btcgains <- mean(btcgains[btcgains>var_95_btcgains]))
[1] 0.09603359
> #TVaR 99%
  > (tvar_99_btcgains <- mean(btcgains[btcgains>var_99_btcgains]))
[1] 0.127442
> #TVaR 99.5%
  > (tvar_99.5_btcgains <- mean(btcgains[btcgains>var_99.5_btcgains]))
[1] 0.1443427


EXPONENTIAL:
  Goodness of fit
> btcgains_exp = fitdist(btcgains, "exp", method = "mle")
> summary(btcgains_exp)
Fitting of the distribution ' exp ' by maximum likelihood 
Parameters : 
  estimate Std. Error
rate  42.7457   1.387584
Loglikelihood:  2614.75   AIC:  -5227.5   BIC:  -5222.644
> gofstat(btcgains_exp, fitnames = "Fitting exponential to BTC gains")
Goodness-of-fit statistics
Fitting exponential to BTC gains
Kolmogorov-Smirnov statistic                       0.01780361
Cramer-von Mises statistic                         0.04973666
Anderson-Darling statistic                         0.40585675

Goodness-of-fit criteria
Fitting exponential to BTC gains
Akaike's Information Criterion                        -5227.500
Bayesian Information Criterion                        -5222.644
Risk measures
> #VaR 70%
> (var_70_exp_btcgains <- qexp(0.70, rate = lambda.loss_exp))
[1] 0.02669294
> #VaR 80%
> (var_80_exp_btcgains <- qexp(0.80, rate = lambda.loss_exp))
[1] 0.03568239
> #VaR 90%
> (var_90_exp_btcgains <- qexp(0.90, rate = lambda.loss_exp))
[1] 0.05104997 
> #VaR 95%
> (var_95_exp_btcgains <- qexp(0.95, rate = lambda.gain_exp))
[1] 0.07008266
> #VaR 99%
> (var_99_exp_btcgains <- qexp(0.99, rate = lambda.gain_exp))
[1] 0.1077341
> #VaR 99.5%
> (var_95.5_exp_btcgains <- qexp(0.995, rate = lambda.gain_exp))
[1] 0.1239497
> f = function(x) qexp(x, rate = lambda.gain_exp)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01546803 with absolute error < 0.00000000000000015
> (tvar_70_exp_btcgains <- (1/(1-0.7))*0.01546803)
[1] 0.0515601
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.012209 with absolute error < 0.00011
> (tvar_80_exp_btcgains <- (1/(1-0.8))*0.012209)
[1] 0.061045
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007725998 with absolute error < 0.00011
> (tvar_90_exp_btcgains <- (1/(1-0.9))*0.007725998)
[1] 0.07725998
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004673717 with absolute error < 0.00011
> (tvar_95_exp_btcgains <- (1/(1-0.95))*0.004673717)
[1] 0.09347434
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001311183 with absolute error < 8.6e-05
> (tvar_99_exp_btcgains <- (1/(1-0.99))*0.001311183)
[1] 0.1311183
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0007366695 with absolute error < 4.3e-05
> (tvar_99.5_exp_btcgains <- (1/(1-0.995))*0.0007366695)
[1] 0.1473339

 
GAMMA:
Goodness of fit
> summary(btcgains_gamma)
Fitting of the distribution ' gamma ' by maximum likelihood 
Parameters : 
        estimate Std. Error
shape  0.9610848 0.03871105
rate  41.0838449 2.14218483
Loglikelihood:  2615.246   AIC:  -5226.492   BIC:  -5216.781 
Correlation matrix:
          shape      rate
shape 1.0000000 0.7724803
rate  0.7724803 1.0000000
> gofstat(btcgains_exp, fitnames = "Fitting gamma to BTC gains")
Goodness-of-fit statistics
                             Fitting gamma to BTC gains
Kolmogorov-Smirnov statistic                 0.01780361
Cramer-von Mises statistic                   0.04973666
Anderson-Darling statistic                   0.40585675

Goodness-of-fit criteria
                               Fitting gamma to BTC gains
Akaike's Information Criterion                  -5227.500
Bayesian Information Criterion                  -5222.644
Risk measures
> #VaR 70%
  > (var_70_gamma_btcgains <- qgamma(0.70, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.02808288
> #VaR 80%
  > (var_80_gamma_btcgains <- qgamma(0.80, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.03776849
> #VaR 90%
  > (var_90_gamma_btcgains <- qgamma(0.90, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.05439388
> #VaR 95%
  > (var_95_gamma_btcgains <- qgamma(0.95, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.071072
> #VaR 99%
  > (var_99_gamma_btcgains <- qgamma(0.99, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.1099106
> #VaR 99.5%
  > (var_95.5_gamma_btcgains <- qgamma(0.995, shape = alpha.gain_gamma, rate = lambda.gain_gamma))
[1] 0.126668
> f = function(x) qgamma(x, shape = alpha.gain_gamma, rate = lambda.gain_gamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01562096 with absolute error < 0.00000000022
> (tvar_70_gamma_btcgains <- (1/(1-0.7))*0.01562096)
[1] 0.05206987
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01236122 with absolute error < 0.00011
> (tvar_80_gamma_btcgains <- (1/(1-0.8))*0.01236122)
[1] 0.0618061
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.007848936 with absolute error < 0.00011
> (tvar_90_gamma_btcgains <- (1/(1-0.9))*0.007848936)
[1] 0.07848936
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.004760245 with absolute error < 0.00011
> (tvar_95_gamma_btcgains <- (1/(1-0.95))*0.004760245)
[1] 0.0952049
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001340902 with absolute error < 8.9e-05
> (tvar_99_gamma_btcgains <- (1/(1-0.99))*0.001340902)
[1] 0.1340902
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0007543141 with absolute error < 4.4e-05
> (tvar_99.5_gamma_btcgains <- (1/(1-0.995))*0.0007543141)
[1] 0.1508628


WEIBULL:
  Goodness of fit
> btcgains_weib = fitdist(btcgains, "weibull", method = "mle")
> summary(btcgains_weib)
Fitting of the distribution ' weibull ' by maximum likelihood 
Parameters : 
  estimate   Std. Error
shape 0.97597817 0.0247385403
scale 0.02314816 0.0008062541
Loglikelihood:  2615.221   AIC:  -5226.441   BIC:  -5216.73 
Correlation matrix:
  shape     scale
shape 1.0000000 0.3133558
scale 0.3133558 1.0000000
> gofstat(btcgains_weib, fitnames = "Fitting Weibull to BTC gains")
Goodness-of-fit statistics
Fitting Weibull to BTC gains
Kolmogorov-Smirnov statistic                   0.01681818
Cramer-von Mises statistic                     0.02972500
Anderson-Darling statistic                     0.23450412

Goodness-of-fit criteria
Fitting Weibull to BTC gains
Akaike's Information Criterion                    -5226.441
Bayesian Information Criterion                    -5216.730
Risk measures
> #VaR 70%
> (var_70_weib_btcgains <- qweibull(0.70, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.02799738
> #VaR 80%
> (var_80_weib_btcgains <- qweibull(0.80, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.03769447
> #VaR 90%
> (var_90_weib_btcgains <- qweibull(0.90, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.05440609
> #VaR 95%
> (var_95_weib_btcgains <- qweibull(0.95, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.07124391
> #VaR 99%
> (var_99_weib_btcgains <- qweibull(0.99, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.1106845
> #VaR 99.5%
> (var_95.5_weib_btcgains <- qweibull(0.995, shape = shape.gain_weib, scale = scale.gain_weib))
[1] 0.1277844
> f = function(x) qweibull(x, shape = shape.gain_weib, scale = scale.gain_weib)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01564041 with absolute error < 0.00000000097
> (tvar_70_weib_btcgains <- (1/(1-0.7))*0.01564041)
[1] 0.0521347
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01238893 with absolute error < 0.00011
> (tvar_80_weib_btcgains <- (1/(1-0.8))*0.01238893)
[1] 0.06194465
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007880884 with absolute error < 0.00011
> (tvar_90_weib_btcgains <- (1/(1-0.9))*0.007880884)
[1] 0.07880884
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004788329 with absolute error < 0.00011
> (tvar_95_weib_btcgains <- (1/(1-0.95))*0.004788329)
[1] 0.09576658
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001354114 with absolute error < 9.1e-05
> (tvar_99_weib_btcgains <- (1/(1-0.99))*0.001354114)
[1] 0.1354114
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0007629201 with absolute error < 4.6e-05
> (tvar_99.5_weib_btcgains <- (1/(1-0.995))*0.0007629201)
[1] 0.152584

 
BURR:
Goodness of fit
> btcgains_burr = fitdist(btcgains, "burr", start = list(shape1 = 2548, scale = 71, shape2 = 1),method = "mle")
> summary(btcgains_burr)
Fitting of the distribution ' burr ' by maximum likelihood 
Parameters : 
           estimate   Std. Error
shape1 2774.5025774 2192.7754508
scale    78.0552043   65.3190420
shape2    0.9759562    0.0247756
Loglikelihood:  2615.219   AIC:  -5224.438   BIC:  -5209.871 
Correlation matrix:
            shape1      scale      shape2
shape1  1.00000000  0.9715807 -0.01493666
scale   0.97158073  1.0000000 -0.24783080
shape2 -0.01493666 -0.2478308  1.00000000
> gofstat(btcgains_burr, fitnames = "Fitting Burr to BTC gains")
Goodness-of-fit statistics
                             Fitting Burr to BTC gains
Kolmogorov-Smirnov statistic                0.01688192
Cramer-von Mises statistic                  0.02980488
Anderson-Darling statistic                  0.23454175

Goodness-of-fit criteria
                               Fitting Burr to BTC gains
Akaike's Information Criterion                 -5224.438
Bayesian Information Criterion                 -5209.871
Risk measures
> #VaR 70%
  > (var_70_burr_btcgains <- qburr(0.70, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.02799556
> #VaR 80%
  > (var_80_burr_btcgains <- qburr(0.80, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.03769509
> #VaR 90%
  > (var_90_burr_btcgains <- qburr(0.90, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.05441439
> #VaR 95%
  > (var_95_burr_btcgains <- qburr(0.95, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.07126434
> #VaR 99%
  > (var_99_burr_btcgains <- qburr(0.99, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.1107502
> #VaR 99.5%
  > (var_95.5_burr_btcgains <- qburr(0.995, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr))
[1] 0.1278771
> f = function(x) qburr(x, shape1.gain_burr, shape2.gain_burr, scale = scale.gain_burr)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01564387 with absolute error < 0.00000000098
> (tvar_70_burr_btcgains <- (1/(1-0.7))*0.01564387)
[1] 0.05214623
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01239248 with absolute error < 0.00011
> (tvar_80_burr_btcgains <- (1/(1-0.8))*0.01239248)
[1] 0.0619624
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.007884069 with absolute error < 0.00011
> (tvar_90_burr_btcgains <- (1/(1-0.9))*0.007884069)
[1] 0.07884069
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.004790849 with absolute error < 0.00011
> (tvar_95_burr_btcgains <- (1/(1-0.95))*0.004790849)
[1] 0.09581698
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001355221 with absolute error < 0.000092
> (tvar_99_burr_btcgains <- (1/(1-0.99))*0.001355221)
[1] 0.1355221
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0007636406 with absolute error < 0.000046
> (tvar_99.5_burr_btcgains <- (1/(1-0.995))*0.0007636406)
[1] 0.1527281

GENERALIZED PARETO:
  Goodness of fit
> btcgains_GenPar = fitdist(btcgains, "genpareto",start = list(shape1=1 ,shape2=1 ,rate=1),
                            +                            method = "mse")
> summary(btcgains_GenPar)
Parameters : 
  estimate
shape1 36.9343761
shape2  0.9706627
rate    1.1462454
Loglikelihood:  2615.004   AIC:  -5224.008   BIC:  -5209.441
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_GenPar, fitnames = "Fitting Gen Pareto to BTC gains")
Goodness-of-fit statistics
Fitting Gen Pareto to BTC gains
Kolmogorov-Smirnov statistic                      0.01807951
Cramer-von Mises statistic                        0.03287375
Anderson-Darling statistic                        0.24398610

Goodness-of-fit criteria
Fitting Gen Pareto to BTC gains
Akaike's Information Criterion                       -5224.008
Bayesian Information Criterion                       -5209.441
Risk measures
> #VaR 70%
> (var_70_genpareto_btcgains <- qgenpareto(0.70, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.02799551
> #VaR 80%
> (var_80_genpareto_btcgains <- qgenpareto(0.80, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.03779885
> #VaR 90%
> (var_90_genpareto_btcgains <- qgenpareto(0.90, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.05485807
#VaR 95%
> (var_95_genpareto_btcgains <- qgenpareto(0.95, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.07227861
> #VaR 99%
> (var_99_genpareto_btcgains <- qgenpareto(0.99, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.1140937
> #VaR 99.5%
> (var_95.5_genpareto_btcgains <- qgenpareto(0.995, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar))
[1] 0.1326936
> f = function(x) qgenpareto(x, shape1.gain_genpar, shape2.gain_genpar, rate.gain_genpar)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01583504 with absolute error < 0.00000000019
> (tvar_70_genpareto_btcgains <- (1/(1-0.7))*0.01583504 )
[1] 0.05278347
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01257948 with absolute error < 0.00000000011
> (tvar_80_genpareto_btcgains <- (1/(1-0.8))*0.01257948 )
[1] 0.0628974
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.008047345 with absolute error < 0.000066
> (tvar_90_genpareto_btcgains <- (1/(1-0.9))*0.008047345 )
[1] 0.08047345
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004920314 with absolute error < 6.6e-05
> (tvar_95_genpareto_btcgains <- (1/(1-0.95))*0.004920314)
[1] 0.09840628
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001414122 with absolute error < 1e-04
> (tvar_99_genpareto_btcgains <- (1/(1-0.99))*0.001414122)
[1] 0.1414122
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0008027029 with absolute error < 5.3e-05
> (tvar_99.5_genpareto_btcgains <- (1/(1-0.995))*0.0008027029)
[1] 0.1605406

 
INV BURR:
Goodness of fit
> btcgains_invburr = fitdist(btcgains, "invburr",start = list(shape1=1 ,shape2=1 ,rate=1),
+                            method = "mle")
> summary(btcgains_invburr)
Fitting of the distribution ' invburr ' by maximum likelihood 
Parameters : 
         estimate Std. Error
shape1  0.3090257 0.03376197
shape2  2.6439601 0.19576664
rate   26.8350995 1.88106555
Loglikelihood:  2607.482   AIC:  -5208.964   BIC:  -5194.398 
Correlation matrix:
           shape1     shape2       rate
shape1  1.0000000 -0.9197366  0.8705860
shape2 -0.9197366  1.0000000 -0.7678963
rate    0.8705860 -0.7678963  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_invburr, fitnames = "Fitting Inv Burr to BTC gains")
Goodness-of-fit statistics
                             Fitting Inv Burr to BTC gains
Kolmogorov-Smirnov statistic                    0.02779145
Cramer-von Mises statistic                      0.11360948
Anderson-Darling statistic                      0.71771311

Goodness-of-fit criteria
                               Fitting Inv Burr to BTC gains
Akaike's Information Criterion                     -5208.964
Bayesian Information Criterion                     -5194.398
Risk measures
#VaR 70%
> (var_70_invburr_btcgains <- qinvburr(0.70, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.02779268
> #VaR 80%
  > (var_80_invburr_btcgains <- qinvburr(0.80, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.03646894
> #VaR 90%
  > (var_90_invburr_btcgains <- qinvburr(0.90, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.05239009
> #VaR 95%
  > (var_95_invburr_btcgains <- qinvburr(0.95, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.07119723
> #VaR 99%
  > (var_99_invburr_btcgains <- qinvburr(0.99, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.1353132
> #VaR 99.5%
  > (var_95.5_invburr_btcgains <- qinvburr(0.995, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr))
[1] 0.1765849
> f = function(x) qinvburr(x, shape1.gain_invburr, shape2.gain_invburr, rate.gain_invburr)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01641325 with absolute error < 0.0000000055
> (tvar_70_invburr_btcgains <- (1/(1-0.7))*0.01641325 )
[1] 0.05471083
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01323077 with absolute error < 0.0000000028
> (tvar_80_invburr_btcgains <- (1/(1-0.8))*0.01323077 )
[1] 0.06615385
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.008895923 with absolute error < 0.00000000092
> (tvar_90_invburr_btcgains <- (1/(1-0.9))*0.008895923 )
[1] 0.08895923
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.005875311 with absolute error < 0.0000000003
> (tvar_95_invburr_btcgains <- (1/(1-0.95))*0.005875311 )
[1] 0.1175062
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.002185781 with absolute error < 0.000086
> (tvar_99_invburr_btcgains <- (1/(1-0.99))*0.002185781 )
[1] 0.2185781
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.001422204 with absolute error < 0.000086
> (tvar_99.5_invburr_btcgains <- (1/(1-0.995))*0.001422204 )
[1] 0.2844408

INVERSE EXPONENTIAL:
  Goodness of fit
> btcgains_InvExp = fitdist(btcgains, "invexp",start = list(scale=1), method = "mle")
> summary(btcgains_InvExp)
Fitting of the distribution ' invexp ' by maximum likelihood 
Parameters : 
  estimate    Std. Error
scale 0.00263148 0.00006993057
Loglikelihood:  1686.106   AIC:  -3370.212   BIC:  -3365.357 
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_InvExp, fitnames = "Fitting inverse exponential to BTC gains")
Goodness-of-fit statistics
Fitting inverse exponential to BTC gains
Kolmogorov-Smirnov statistic                                0.4363816
Cramer-von Mises statistic                                 76.0997626
Anderson-Darling statistic                                402.3710904

Goodness-of-fit criteria
Fitting inverse exponential to BTC gains
Akaike's Information Criterion                                -3370.212
Bayesian Information Criterion                                -3365.357
Risk measures
> #VaR 70%
> (var_70_invexp_btcgains <- qinvexp(0.70, scale = scale.gain_invexp))
[1] 0.00737781
> #VaR 80%
> (var_80_invexp_btcgains <- qinvexp(0.80, scale = scale.gain_invexp))
[1] 0.01179277
> #VaR 90%
> (var_90_invexp_btcgains <- qinvexp(0.90, scale = scale.gain_invexp))
[1] 0.02497596
> #VaR 95%
> (var_95_invexp_btcgains <- qinvexp(0.95, scale = scale.gain_invexp))
[1] 0.05130261
> #VaR 99%
> (var_99_invexp_btcgains <- qinvexp(0.99,scale = scale.gain_invexp))
[1] 0.26183
> #VaR 99.5%
> (var_95.5_invexp_btcgains <- qinvexp(0.995, scale = scale.gain_invexp))
[1] 0.5249791
> f = function(x) qinvexp(x, scale = scale.gain_invexp)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent

 
INVERSE GAMMA:
Goodness of fit
> btcgains_InvGamma = fitdist(btcgains, "invgamma",start = list(shape=1 ,rate=1), method = "mle")
> summary(btcgains_InvGamma)
Fitting of the distribution ' invgamma ' by maximum likelihood 
Parameters : 
         estimate  Std. Error
shape   0.4146762  0.01546378
rate  918.4904229 57.72883704
Loglikelihood:  2052.893   AIC:  -4101.785   BIC:  -4092.075 
Correlation matrix:
           shape       rate
shape  1.0000000 -0.5953913
rate  -0.5953913  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_InvGamma, fitnames = "Fitting inverse gamma to BTC gains")
Goodness-of-fit statistics
                             Fitting inverse gamma to BTC gains
Kolmogorov-Smirnov statistic                          0.2433187
Cramer-von Mises statistic                           21.6291081
Anderson-Darling statistic                          109.6237206

Goodness-of-fit criteria
                               Fitting inverse gamma to BTC gains
Akaike's Information Criterion                          -4101.785
Bayesian Information Criterion                          -4092.075
Risk measures
> #VaR 70%
  > (var_70_invgamma_btcgains <- qinvgamma(0.70, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 0.02576717
> #VaR 80%
  > (var_80_invgamma_btcgains <- qinvgamma(0.80, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 0.06979557
> #VaR 90%
  > (var_90_invgamma_btcgains <- qinvgamma(0.90, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 0.3746743
> #VaR 95%
  > (var_95_invgamma_btcgains <- qinvgamma(0.95, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 1.996724
> #VaR 99%
  > (var_99_invgamma_btcgains <- qinvgamma(0.99, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 96.84165
> #VaR 99.5%
  > (var_95.5_invgamma_btcgains <- qinvgamma(0.995, shape = shape.gain_invgamma, rate = rate.gain_invgamma))
[1] 515.2344
> f = function(x) qinvgamma(x, shape = shape.gain_invgamma, rate = rate.gain_invgamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent


INVERSE GAUSSIAN:
  Goodness of fit
> btcgains_InvGauss = fitdist(btcgains, "invgauss",start = list(mean = 1 , shape = 0.1), method = "mle")
> summary(btcgains_InvGauss)
Fitting of the distribution ' invgauss ' by maximum likelihood 
Parameters : 
  estimate   Std. Error
mean  0.023437462 0.0021809301
shape 0.002805906 0.0001090103
Loglikelihood:  2095.137   AIC:  -4186.274   BIC:  -4176.563 
Correlation matrix:
  mean          shape
mean   1.00000000000 -0.00001104544
shape -0.00001104544  1.00000000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_InvGauss, fitnames = "Fitting inverse Gaussian to BTC gains")
Goodness-of-fit statistics
Fitting inverse Gaussian to BTC gains
Kolmogorov-Smirnov statistic                             0.3364879
Cramer-von Mises statistic                              42.9306241
Anderson-Darling statistic                             200.6331398

Goodness-of-fit criteria
Fitting inverse Gaussian to BTC gains
Akaike's Information Criterion                             -4186.274
Bayesian Information Criterion                             -4176.563
Risk measures
> #VaR 70%
> (var_70_invgauss_btcgains <- qinvgauss(0.70, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.01187728
> #VaR 80%
> (var_80_invgauss_btcgains <- qinvgauss(0.80, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.02173567
> #VaR 90%
> (var_90_invgauss_btcgains <- qinvgauss(0.90, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.05184012
> #VaR 95%
> (var_95_invgauss_btcgains <- qinvgauss(0.95, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.1039665
> #VaR 99%
> (var_99_invgauss_btcgains <- qinvgauss(0.99, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.3204911
> #VaR 99.5%
> (var_95.5_invgauss_btcgains <- qinvgauss(0.995, mean.gain_invgauss, shape.gain_invgauss))
[1] 0.4506822
> f = function(x) qinvgauss(x, mean.gain_invgauss, shape.gain_invgauss)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.02084679 with absolute error < 0.00000043
> (tvar_70_invgauss_btcgains <- (1/(1-0.7))*0.02084679 )
[1] 0.0694893
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01923777 with absolute error < 0.00000025
> (tvar_80_invgauss_btcgains <- (1/(1-0.8))*0.01923777 )
[1] 0.09618885
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.01589041 with absolute error < 0.0000001
> (tvar_90_invgauss_btcgains <- (1/(1-0.9))*0.01589041 )
[1] 0.1589041
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.01224472 with absolute error < 0.000000042
> (tvar_95_invgauss_btcgains <- (1/(1-0.95))*0.01224472 )
[1] 0.2448944
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.005288726 with absolute error < 0.000068
> (tvar_99_invgauss_btcgains <- (1/(1-0.99))*0.005288726 )
[1] 0.5288726
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.003406123 with absolute error < 0.000068
> (tvar_99.5_invgauss_btcgains <- (1/(1-0.995))*0.003406123 )
[1] 0.6812246

 
PARALOGISTIC:
Goodness of fit
> btcgains_para = fitdist(btcgains, "paralogis",start = list(shape=1 ,rate=1), method = "mle")
> summary(btcgains_para)
Fitting of the distribution ' paralogis ' by maximum likelihood 
Parameters : 
      estimate Std. Error
shape  1.35181 0.02899734
rate  50.30953 2.17366305
Loglikelihood:  2573.238   AIC:  -5142.475   BIC:  -5132.765 
Correlation matrix:
           shape       rate
shape  1.0000000 -0.4838212
rate  -0.4838212  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_para, fitnames = "Fitting Paralogistic to BTC gains")
Goodness-of-fit statistics
                             Fitting Paralogistic to BTC gains
Kolmogorov-Smirnov statistic                        0.03718968
Cramer-von Mises statistic                          0.47322647
Anderson-Darling statistic                          4.93889251

Goodness-of-fit criteria
                               Fitting Paralogistic to BTC gains
Akaike's Information Criterion                         -5142.475
Bayesian Information Criterion                         -5132.765
Risk measures
> #VaR 70%
  > (var_70_para_btcgains <- qparalogis(0.70, shape.gain_para, rate.gain_para))
[1] 0.02598708
> #VaR 80%
  > (var_80_para_btcgains <- qparalogis(0.80, shape.gain_para, rate.gain_para))
[1] 0.03667717
> #VaR 90%
  > (var_90_para_btcgains <- qparalogis(0.90, shape.gain_para, rate.gain_para))
[1] 0.060396
> #VaR 95%
  > (var_95_para_btcgains <- qparalogis(0.95, shape.gain_para, rate.gain_para))
[1] 0.09402001
> #VaR 99%
  > (var_99_para_btcgains <- qparalogis(0.99, shape.gain_para, rate.gain_para))
[1] 0.2409772
> #VaR 99.5%
  > (var_95.5_para_btcgains <- qparalogis(0.995, shape.gain_para, rate.gain_para))
[1] 0.3557109
> f = function(x) qparalogis(x, shape.gain_para, rate.gain_para)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.02240692 with absolute error < 0.00000018
> (tvar_70_para_btcgains <- (1/(1-0.7))*0.02240692  )
[1] 0.07468973
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01932559 with absolute error < 0.00000011
> (tvar_80_para_btcgains <- (1/(1-0.8))*0.01932559  )
[1] 0.09662795
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.01467369 with absolute error < 0.00000005
> (tvar_90_para_btcgains <- (1/(1-0.9))*0.01467369  )
[1] 0.1467369
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.01095897 with absolute error < 0.000000022
> (tvar_95_para_btcgains <- (1/(1-0.95))*0.01095897  )
[1] 0.2191794
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.005405732 with absolute error < 0.0000000032
> (tvar_99_para_btcgains <- (1/(1-0.99))*0.005405732  )
[1] 0.5405732
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.003964589 with absolute error < 0.0000000014
> (tvar_99.5_para_btcgains <- (1/(1-0.995))*0.003964589  )
[1] 0.7929178


LOGNORMAL:
  Goodness of fit
> btcgains_lnorm = fitdist(btcgains, "lnorm", method = "mle")
> summary(btcgains_lnorm)
Fitting of the distribution ' lnorm ' by maximum likelihood 
Parameters : 
  estimate Std. Error
meanlog -4.358834 0.04289044
sdlog    1.321276 0.03032804
Loglikelihood:  2525.571   AIC:  -5047.142   BIC:  -5037.432 
Correlation matrix:
  meanlog sdlog
meanlog       1     0
sdlog         0     1
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_lnorm, fitnames = "Fitting Lognormal to BTC gains")
Goodness-of-fit statistics
Fitting Lognormal to BTC gains
Kolmogorov-Smirnov statistic                     0.08715681
Cramer-von Mises statistic                       2.05127398
Anderson-Darling statistic                      12.45093653

Goodness-of-fit criteria
Fitting Lognormal to BTC gains
Akaike's Information Criterion                      -5047.142
Bayesian Information Criterion                      -5037.432
Risk measures
> #VaR 70%
> (var_70_lnorm_btcgains <- qlnorm(0.70, mulog.gain, sdlog.gain))
[1] 0.0255797
> #VaR 80%
> (var_80_lnorm_btcgains <- qlnorm(0.80, mulog.gain, sdlog.gain))
[1] 0.03889771
> #VaR 90%
> (var_90_lnorm_btcgains <- qlnorm(0.90, mulog.gain, sdlog.gain))
[1] 0.06956106
> #VaR 95%
> (var_95_lnorm_btcgains <- qlnorm(0.95, mulog.gain, sdlog.gain))
[1] 0.1124184
> #VaR 99%
> (var_99_lnorm_btcgains <- qlnorm(0.99, mulog.gain, sdlog.gain))
[1] 0.2766268
> #VaR 99.5%
> (var_95.5_lnorm_btcgains <- qlnorm(0.995, mulog.gain, sdlog.gain))
[1] 0.3846382
> f = function(x) qlnorm(x, mulog.gain, sdlog.gain)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.02410893 with absolute error < 0.0000032
> (tvar_70_lnorm_btcgains <- (1/(1-0.7))*0.02410893   )
[1] 0.0803631
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.02095536 with absolute error < 0.0000022
> (tvar_80_lnorm_btcgains <- (1/(1-0.8))*0.02095536   )
[1] 0.1047768
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.01579755 with absolute error < 0.0000012
> (tvar_90_lnorm_btcgains <- (1/(1-0.9))*0.01579755   )
[1] 0.1579755
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.01142696 with absolute error < 0.00000064
> (tvar_95_lnorm_btcgains <- (1/(1-0.95))*0.01142696   )
[1] 0.2285392
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.004821281 with absolute error < 0.00000015
> (tvar_99_lnorm_btcgains <- (1/(1-0.99))*0.004821281   )
[1] 0.4821281
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.003209577 with absolute error < 0.000092
> (tvar_99.5_lnorm_btcgains <- (1/(1-0.995))*0.003209577   )
[1] 0.6419154

 
INVERSE WEIBULL:
Goodness of fit
> btcgains_invweib = fitdist(btcgains, "invweibull", method = "mle")
> summary(btcgains_invweib)
Fitting of the distribution ' invweibull ' by maximum likelihood 
Parameters : 
         estimate Std. Error
shape 0.581181848 0.01176572
scale 0.006221883 0.00035482
Loglikelihood:  2274.865   AIC:  -4545.729   BIC:  -4536.018 
Correlation matrix:
           shape      scale
shape  1.0000000 -0.3255704
scale -0.3255704  1.0000000

> shape.gain_invweib = btcgains_invweib$estimate[1]
> scale.gain_invweib = btcgains_invweib$estimate[2]
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_invweib, fitnames = "Fitting Inverse Weibull to BTC gains")
Goodness-of-fit statistics
                             Fitting Inverse Weibull to BTC gains
Kolmogorov-Smirnov statistic                            0.1685388
Cramer-von Mises statistic                              9.0641284
Anderson-Darling statistic                             54.8973432

Goodness-of-fit criteria
                               Fitting Inverse Weibull to BTC gains
Akaike's Information Criterion                            -4545.729
Bayesian Information Criterion                            -4536.018
Risk measures
> #VaR 70%
  > (var_70_invweib_btcgains <- qinvweibull(0.70, shape.gain_invweib, 1/scale.gain_invweib))
[1] 0.03666873
> #VaR 80%
  > (var_80_invweib_btcgains <- qinvweibull(0.80, shape.gain_invweib, 1/scale.gain_invweib))
[1] 0.08218043
> #VaR 90%
  > (var_90_invweib_btcgains <- qinvweibull(0.90, shape.gain_invweib, 1/scale.gain_invweib))
[1] 0.2989052
> #VaR 95%
  > (var_95_invweib_btcgains <- qinvweibull(0.95, shape.gain_invweib, 1/scale.gain_invweib))
[1] 1.031413
> #VaR 99%
  > (var_99_invweib_btcgains <- qinvweibull(0.99, shape.gain_invweib, 1/scale.gain_invweib))
[1] 17.03859
> #VaR 99.5%
  > (var_95.5_invweib_btcgains <- qinvweibull(0.995, shape.gain_invweib, 1/scale.gain_invweib))
[1] 56.39951
> f = function(x) qinvweibull(x, shape.gain_invweib, 1/scale.gain_invweib)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : 
  the integral is probably divergent
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : 
  the integral is probably divergent
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : 
  the integral is probably divergent
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  the integral is probably divergent
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  the integral is probably divergent
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  the integral is probably divergent


INVERSE PARALOGISTIC:
  Goodness of fit
> btcgains_invpara = fitdist(btcgains, "invparalogis", start = list(shape = 1, rate = 1), method = "mle")
> summary(btcgains_invpara)
Fitting of the distribution ' invparalogis ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape  1.223106  0.0236424
rate  88.442831  4.1031880
Loglikelihood:  2534.781   AIC:  -5065.563   BIC:  -5055.852 
Correlation matrix:
  shape      rate
shape 1.0000000 0.3927739
rate  0.3927739 1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_invpara, fitnames = "Fitting Inverse Paralogistic to BTC gains")
Goodness-of-fit statistics
Fitting Inverse Paralogistic to BTC gains
Kolmogorov-Smirnov statistic                                0.06695767
Cramer-von Mises statistic                                  1.05961779
Anderson-Darling statistic                                  9.68207760

Goodness-of-fit criteria
Fitting Inverse Paralogistic to BTC gains
Akaike's Information Criterion                                 -5065.563
Bayesian Information Criterion                                 -5055.852
Risk measures
> #VaR 70%
> (var_70_invpara_btcgains <- qinvparalogis(0.70, shape.gain_invpara, rate.gain_invpara))
[1] 0.02740787
> #VaR 80%
> (var_80_invpara_btcgains <- qinvparalogis(0.80, shape.gain_invpara, rate.gain_invpara))
[1] 0.04212659
> #VaR 90%
> (var_90_invpara_btcgains <- qinvparalogis(0.90, shape.gain_invpara, rate.gain_invpara))
[1] 0.08100158
> #VaR 95%
> (var_95_invpara_btcgains <- qinvparalogis(0.95, shape.gain_invpara, rate.gain_invpara))
[1] 0.1485998
> #VaR 99%
> (var_99_invpara_btcgains <- qinvparalogis(0.99, shape.gain_invpara, rate.gain_invpara))
[1] 0.5711971
> #VaR 99.5%
> (var_95.5_invpara_btcgains <- qinvparalogis(0.995, shape.gain_invpara, rate.gain_invpara))
[1] 1.010487
> f = function(x) qinvparalogis(x, shape.gain_invpara, rate.gain_invpara)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.05661223 with absolute error < 0.0000015
> (tvar_70_invpara_btcgains <- (1/(1-0.7))*0.05661223)
[1] 0.1887074
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.0532226 with absolute error < 0.00000094
> (tvar_80_invpara_btcgains <- (1/(1-0.8))*0.0532226)
[1] 0.266113
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.04746268 with absolute error < 0.00000041
> (tvar_90_invpara_btcgains <- (1/(1-0.9))*0.04746268)
[1] 0.4746268
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.04207037 with absolute error < 0.00000018
> (tvar_95_invpara_btcgains <- (1/(1-0.95))*0.04207037)
[1] 0.8414074
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.03151254 with absolute error < 0.000000027
> (tvar_99_invpara_btcgains <- (1/(1-0.99))*0.03151254)
[1] 3.151254
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.02778575 with absolute error < 0.000000013
> (tvar_99.5_invpara_btcgains <- (1/(1-0.995))*0.02778575)
[1] 5.55715

 
PARETO:
Goodness of fit
> btcgains_pareto = fitdist(btcgains, "pareto", method = "mle")
> summary(btcgains_pareto)
Fitting of the distribution ' pareto ' by maximum likelihood 
Parameters : 
       estimate Std. Error
shape 47.463147  77.093526
scale  1.087069   1.802655
Loglikelihood:  2614.938   AIC:  -5225.875   BIC:  -5216.165 
Correlation matrix:
          shape     scale
shape 1.0000000 0.9998003
scale 0.9998003 1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_pareto, fitnames = "Fitting Pareto to BTC gains")
Goodness-of-fit statistics
                             Fitting Pareto to BTC gains
Kolmogorov-Smirnov statistic                  0.01377405
Cramer-von Mises statistic                    0.03053002
Anderson-Darling statistic                    0.27301430

Goodness-of-fit criteria
                               Fitting Pareto to BTC gains
Akaike's Information Criterion                   -5225.875
Bayesian Information Criterion                   -5216.165
Risk measures
> #VaR 70%
  > (var_70_pareto_btcgains <- qpareto(0.70, shape.gain_pareto, scale.gain_pareto))
[1] 0.02792783
> #VaR 80%
  > (var_80_pareto_btcgains <- qpareto(0.80, shape.gain_pareto, scale.gain_pareto))
[1] 0.03749375
> #VaR 90%
  > (var_90_pareto_btcgains <- qpareto(0.90, shape.gain_pareto, scale.gain_pareto))
[1] 0.05403727
> #VaR 95%
  > (var_95_pareto_btcgains <- qpareto(0.95, shape.gain_pareto, scale.gain_pareto))
[1] 0.07082415
> #VaR 99%
  > (var_99_pareto_btcgains <- qpareto(0.99, shape.gain_pareto, scale.gain_pareto))
[1] 0.1107607
> #VaR 99.5%
  > (var_95.5_pareto_btcgains <- qpareto(0.995, shape.gain_pareto, scale.gain_pareto))
[1] 0.128382
> f = function(x) qpareto(x, shape.gain_pareto, scale.gain_pareto)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01557758 with absolute error < 0.00000000000000029
> (tvar_70_pareto_btcgains <- (1/(1-0.7))*0.01557758  )
[1] 0.05192527
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01233925 with absolute error < 0.00012
> (tvar_80_pareto_btcgains <- (1/(1-0.8))*0.01233925  )
[1] 0.06169625
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.007859502 with absolute error < 0.00012
> (tvar_90_pareto_btcgains <- (1/(1-0.9))*0.007859502  )
[1] 0.07859502
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.004787079 with absolute error < 0.00012
> (tvar_95_pareto_btcgains <- (1/(1-0.95))*0.004787079 )
[1] 0.09574158
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001365278 with absolute error < 0.000097
> (tvar_99_pareto_btcgains <- (1/(1-0.99))*0.001365278 )
[1] 0.1365278
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.000772641 with absolute error < 0.000049
> (tvar_99.5_pareto_btcgains <- (1/(1-0.995))*0.000772641 )
[1] 0.1545282


BETA:
  Goodness of fit
> btcgains_beta = fitdist(btcgains, "beta", method = "mle")
> summary(btcgains_beta)
Fitting of the distribution ' beta ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape1  0.9429443 0.03791693
shape2 39.3459475 2.04157736
Loglikelihood:  2615.098   AIC:  -5226.196   BIC:  -5216.485 
Correlation matrix:
  shape1    shape2
shape1 1.0000000 0.7651989
shape2 0.7651989 1.0000000

> shape1.gain_beta = btcgains_beta$estimate[1]
> shape2.gain_beta = btcgains_beta$estimate[2]
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_beta, fitnames = "Fitting Beta to BTC gains")
Goodness-of-fit statistics
Fitting Beta to BTC gains
Kolmogorov-Smirnov statistic                 0.0149748
Cramer-von Mises statistic                   0.0386137
Anderson-Darling statistic                   0.2944497

Goodness-of-fit criteria
Fitting Beta to BTC gains
Akaike's Information Criterion                 -5226.196
Bayesian Information Criterion                 -5216.485
Risk measures
> #VaR 70%
> (var_70_beta_btcgains <- qbeta(0.70, shape1.gain_beta, shape2.gain_beta))
[1] 0.02833771
> #VaR 80%
> (var_80_beta_btcgains <- qbeta(0.80, shape1.gain_beta, shape2.gain_beta))
[1] 0.03803419
> #VaR 90%
> (var_90_beta_btcgains <- qbeta(0.90, shape1.gain_beta, shape2.gain_beta))
[1] 0.05448567
> #VaR 95%
> (var_95_beta_btcgains <- qbeta(0.95, shape1.gain_beta, shape2.gain_beta))
[1] 0.07073141
> #VaR 99%
> (var_99_pareto_btcgains <- qbeta(0.99, shape1.gain_beta, shape2.gain_beta))
[1] 0.1075404
> #VaR 99.5%
> (var_95.5_beta_btcgains <- qbeta(0.995, shape1.gain_beta, shape2.gain_beta))
[1] 0.1229817
> f = function(x) qbeta(x, shape1.gain_beta, shape2.gain_beta)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01557927 with absolute error < 0.00000000028
> (tvar_70_beta_btcgains <- (1/(1-0.7))*0.01557927  )
[1] 0.0519309
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01229292 with absolute error < 0.000099
> (tvar_80_beta_btcgains <- (1/(1-0.8))*0.01229292  )
[1] 0.0614646
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007759685 with absolute error < 0.000099
> (tvar_90_beta_btcgains <- (1/(1-0.9))*0.007759685  )
[1] 0.07759685
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004674888 with absolute error < 0.000099
> (tvar_95_beta_btcgains <- (1/(1-0.95))*0.004674888 )
[1] 0.09349776
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001294708 with absolute error < 0.000078
> (tvar_99_beta_btcgains <- (1/(1-0.99))*0.001294708 )
[1] 0.1294708
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0007227623 with absolute error < 0.000039
> (tvar_99.5_beta_btcgains <- (1/(1-0.995))*0.0007227623 )
[1] 0.1445525

 
UNIFORM:
Goodness of fit
> btcgains_uni = fitdist(btcgains, "unif", method = "mle")
> summary(btcgains_uni)
Fitting of the distribution ' unif ' by maximum likelihood 
Parameters : 
         estimate
min 0.00001796359
max 0.17811207165
Loglikelihood:  1637.446   AIC:  -3270.891   BIC:  -3261.18 
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_uni, fitnames = "Fitting Uniform to BTC gains")
Goodness-of-fit statistics
                             Fitting Uniform to BTC gains
Kolmogorov-Smirnov statistic                    0.6135171
Cramer-von Mises statistic                    161.8259049
Anderson-Darling statistic                            Inf

Goodness-of-fit criteria
                               Fitting Uniform to BTC gains
Akaike's Information Criterion                    -3270.891
Bayesian Information Criterion                    -3261.180
Risk measures
> #VaR 70%
  > (var_70_uni_btcgains <- qunif(0.70, min.gain_uni, max.gain_uni))
[1] 0.1246838
> #VaR 80%
  > (var_80_uni_btcgains <- qunif(0.80, min.gain_uni, max.gain_uni))
[1] 0.1424933
> #VaR 90%
  > (var_90_uni_btcgains <- qunif(0.90, min.gain_uni, max.gain_uni))
[1] 0.1603027
> #VaR 95%
  > (var_95_uni_btcgains <- qunif(0.95, min.gain_uni, max.gain_uni))
[1] 0.1692074
> #VaR 99%
  > (var_99_uni_btcgains <- qunif(0.99, min.gain_uni, max.gain_uni))
[1] 0.1763311
> #VaR 99.5%
  > (var_95.5_uni_btcgains <- qunif(0.995, min.gain_uni, max.gain_uni))
[1] 0.1772216
> f = function(x) qunif(x, min.gain_uni, max.gain_uni)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.04541939 with absolute error < 0.0000000000000005
> (tvar_70_uni_btcgains <- (1/(1-0.7))*0.04541939)
[1] 0.151398
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.03206053 with absolute error < 0.00000000000000036
> (tvar_80_uni_btcgains <- (1/(1-0.8))*0.03206053)
[1] 0.1603027
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.01692074 with absolute error < 0.00000000000000019
> (tvar_90_uni_btcgains <- (1/(1-0.9))*0.01692074)
[1] 0.1692074
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.008682986 with absolute error < 0.000000000000000096
> (tvar_95_uni_btcgains <- (1/(1-0.95))*0.008682986)
[1] 0.1736597
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001772216 with absolute error < 0.00000000000000002
> (tvar_99_uni_btcgains <- (1/(1-0.99))*0.001772216)
[1] 0.1772216
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0008883342 with absolute error < 0.0000000000000000099
> (tvar_99.5_uni_btcgains <- (1/(1-0.995))*0.0008883342)
[1] 0.1776668


INVERSE PARETO:
  Goodness of fit
> btcgains_invpare = fitdist(btcgains, "invpareto", start = list(shape = 1, scale = 1), method = "mle")
> summary(btcgains_invpare)
Fitting of the distribution ' invpareto ' by maximum likelihood 
Parameters : 
  estimate   Std. Error
shape 1.40488948 0.0894317236
scale 0.00898784 0.0008412171
Loglikelihood:  2499.647   AIC:  -4995.294   BIC:  -4985.584 
Correlation matrix:
  shape      scale
shape  1.0000000 -0.8602113
scale -0.8602113  1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_invpare, fitnames = "Fitting Inverse Pareto to BTC gains")
Goodness-of-fit statistics
Fitting Inverse Pareto to BTC gains
Kolmogorov-Smirnov statistic                            0.104011
Cramer-von Mises statistic                              2.478237
Anderson-Darling statistic                             18.692313

Goodness-of-fit criteria
Fitting Inverse Pareto to BTC gains
Akaike's Information Criterion                           -4995.294
Bayesian Information Criterion                           -4985.584
Risk measures
> #VaR 70%
> (var_70_invpare_btcgains <- qinvpareto(0.70, shape.gain_invpare, scale.gain_invpare))
[1] 0.03109779
> #VaR 80%
> (var_80_invpare_btcgains <- qinvpareto(0.80, shape.gain_invpare, scale.gain_invpare))
[1] 0.05221154
> #VaR 90%
> (var_90_invpare_btcgains <- qinvpareto(0.90, shape.gain_invpare, scale.gain_invpare))
[1] 0.1154072
> #VaR 95%
> (var_95_invpare_btcgains <- qinvpareto(0.95, shape.gain_invpare, scale.gain_invpare))
[1] 0.2417044
> #VaR 99%
> (var_99_invpare_btcgains <- qinvpareto(0.99, shape.gain_invpare, scale.gain_invpare))
[1] 1.25188
> #VaR 99.5%
> (var_95.5_invpare_btcgains <- qinvpareto(0.995, shape.gain_invpare, scale.gain_invpare))
[1] 2.514574
> f = function(x) qinvpareto(x, shape.gain_invpare, scale.gain_invpare)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
Error in integrate(f, lower = 0.7, upper = 1) : non-finite function value
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
Error in integrate(f, lower = 0.8, upper = 1) : non-finite function value
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
Error in integrate(f, lower = 0.9, upper = 1) : non-finite function value
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
Error in integrate(f, lower = 0.95, upper = 1) : 
  non-finite function value
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
Error in integrate(f, lower = 0.99, upper = 1) : 
  non-finite function value
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
Error in integrate(f, lower = 0.995, upper = 1) : 
  non-finite function value

 
TRANSFORMED GAMMA:
Goodness of fit
> btcgains_trgamma = fitdist(btcgains, "trgamma", start = list(shape1 = 1, shape2 = 1, rate = 1), method = "mle")
> summary(btcgains_trgamma)
Fitting of the distribution ' trgamma ' by maximum likelihood 
Parameters : 
         estimate Std. Error
shape1  0.9629727  0.1595554
shape2  0.9985764  0.1042061
rate   41.1862507  8.6169857
Loglikelihood:  2615.246   AIC:  -5224.492   BIC:  -5209.926 
Correlation matrix:
           shape1     shape2       rate
shape1  1.0000000 -0.9699918  0.9861982
shape2 -0.9699918  1.0000000 -0.9683741
rate    0.9861982 -0.9683741  1.0000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_trgamma, fitnames = "Fitting Transformed Gamma to BTC gains")
Goodness-of-fit statistics
                             Fitting Transformed Gamma to BTC gains
Kolmogorov-Smirnov statistic                             0.01623706
Cramer-von Mises statistic                               0.03043196
Anderson-Darling statistic                               0.23957316

Goodness-of-fit criteria
                               Fitting Transformed Gamma to BTC gains
Akaike's Information Criterion                              -5224.492
Bayesian Information Criterion                              -5209.926
Risk measures
> #VaR 70%
  > (var_70_trgamma_btcgains <- qtrgamma(0.70, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.02807815
> #VaR 80%
  > (var_80_trgamma_btcgains <- qtrgamma(0.80, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.03776662
> #VaR 90%
  > (var_90_trgamma_btcgains <- qtrgamma(0.90, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.05440144
> #VaR 95%
  > (var_95_trgamma_btcgains <- qtrgamma(0.95, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.07109376
> #VaR 99%
  > (var_99_trgamma_btcgains <- qtrgamma(0.99, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.1099798
> #VaR 99.5%
  > (var_95.5_trgamma_btcgains <- qtrgamma(0.995, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma))
[1] 0.1267628
> f = function(x) qtrgamma(x, shape1.gain_trgamma, shape2.gain_trgamma, rate.gain_trgamma)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.01562408 with absolute error < 0.00000000026
> (tvar_70_trgamma_btcgains <- (1/(1-0.7))*0.01562408 )
[1] 0.05208027
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.01236469 with absolute error < 0.00011
> (tvar_80_trgamma_btcgains <- (1/(1-0.8))*0.01236469 )
[1] 0.06182345
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.00785222 with absolute error < 0.00011
> (tvar_90_trgamma_btcgains <- (1/(1-0.9))*0.00785222 )
[1] 0.0785222
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.004762855 with absolute error < 0.00011
> (tvar_95_trgamma_btcgains <- (1/(1-0.95))*0.004762855 )
[1] 0.0952571
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.001341994 with absolute error < 0.000089
> (tvar_99_trgamma_btcgains <- (1/(1-0.99))*0.001341994 )
[1] 0.1341994
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0007550053 with absolute error < 0.000045
> (tvar_99.5_trgamma_btcgains <- (1/(1-0.995))*0.0007550053 )
[1] 0.1510011

TRANSFORMED BETA:
  Goodness of fit
> btcgains_trbeta = fitdist(btcgains, "trbeta", method = "mle")
> summary(btcgains_trbeta)
Fitting of the distribution ' trbeta ' by maximum likelihood 
Parameters : 
  estimate Std. Error
shape1 4.87792970 2.63403551
shape2 1.39008719 0.22325546
shape3 0.64795523 0.13597121
scale  0.09984414 0.04715648
Loglikelihood:  2614.314   AIC:  -5220.628   BIC:  -5201.207 
Correlation matrix:
  shape1     shape2     shape3      scale
shape1  1.0000000 -0.8572064  0.7779482  0.9939635
shape2 -0.8572064  1.0000000 -0.9733024 -0.8342672
shape3  0.7779482 -0.9733024  1.0000000  0.7373616
scale   0.9939635 -0.8342672  0.7373616  1.0000000
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_trbeta, fitnames = "Fitting Transformed Beta to BTC gains")
Goodness-of-fit statistics
Fitting Transformed Beta to BTC gains
Kolmogorov-Smirnov statistic                            0.01591097
Cramer-von Mises statistic                              0.03033984
Anderson-Darling statistic                              0.24189085

Goodness-of-fit criteria
Fitting Transformed Beta to BTC gains
Akaike's Information Criterion                             -5220.628
Bayesian Information Criterion                             -5201.207
Risk measures
> #VaR 70%
> (var_70_trbeta_btcgains <- qtrbeta(0.70, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.02792361
> #VaR 80%
> (var_80_trbeta_btcgains <- qtrbeta(0.80, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.03728399
> #VaR 90%
> (var_90_trbeta_btcgains <- qtrbeta(0.90, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.05359376
> #VaR 95%
> (var_95_trbeta_btcgains <- qtrbeta(0.95, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.07061124
> #VaR 99%
> (var_99_trbeta_btcgains <- qtrbeta(0.99, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.114394
> #VaR 99.5%
> (var_95.5_trbeta_btcgains <- qtrbeta(0.995, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta))
[1] 0.1356629
> f = function(x) qtrbeta(x, shape1.gain_trbeta, shape2.gain_trbeta, shape3.gain_trbeta, 1/scale.gain_trbeta)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
0.01565884 with absolute error < 0.000000029
> (tvar_70_trbeta_btcgains <- (1/(1-0.7))*0.01565884 )
[1] 0.05219613
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
0.01243054 with absolute error < 0.000000019
> (tvar_80_trbeta_btcgains <- (1/(1-0.8))*0.01243054  )
[1] 0.0621527
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
0.007984249 with absolute error < 0.000095
> (tvar_90_trbeta_btcgains <- (1/(1-0.9))*0.007984249  )
[1] 0.07984249
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
0.004931441 with absolute error < 0.000095
> (tvar_95_trbeta_btcgains <- (1/(1-0.95))*0.004931441  )
[1] 0.09862882
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
0.001478613 with absolute error < 0.000078
> (tvar_99_trbeta_btcgains <- (1/(1-0.99))*0.001478613  )
[1] 0.1478613
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
0.0008602544 with absolute error < 0.000078
> (tvar_99.5_trbeta_btcgains <- (1/(1-0.995))*0.0008602544  )
[1] 0.1720509

GENERALIZED BETA:
Goodness of fit
> btcgains_genbeta = fitdist(btcgains, "genbeta", method = "mse")
> summary(btcgains_genbeta)
Parameters : 
           estimate
shape1  4531.354175
shape2 45312.853189
shape3     3.000000
scale      3.734081
Loglikelihood:  -Inf   AIC:  Inf   BIC:  Inf 
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_genbeta, fitnames = "Fitting Generalised Beta to BTC gains")
Goodness-of-fit statistics
                             Fitting Generalised Beta to BTC gains
Kolmogorov-Smirnov statistic                                1.0000
Cramer-von Mises statistic                                316.3333
Anderson-Darling statistic                                     Inf

Goodness-of-fit criteria
                               Fitting Generalised Beta to BTC gains
Akaike's Information Criterion                                   Inf
Bayesian Information Criterion                                   Inf
Risk measures
> #VaR 70%
  > (var_70_genbeta_btcgains <- qgenbeta(0.70, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.683138
> #VaR 80%
  > (var_80_genbeta_btcgains <- qgenbeta(0.80, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.685651
> #VaR 90%
  > (var_90_genbeta_btcgains <- qgenbeta(0.90, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.689135
> #VaR 95%
  > (var_95_genbeta_btcgains <- qgenbeta(0.95, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.692011
> #VaR 99%
  > (var_99_genbeta_btcgains <- qgenbeta(0.99, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.697403
> #VaR 99.5%
  > (var_95.5_genbeta_btcgains <- qgenbeta(0.995, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta))
[1] 1.699376
> f = function(x) qgenbeta(x, shape1.gain_genbeta, shape2.gain_genbeta, shape3.gain_genbeta, 1/scale.gain_genbeta)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.506449 with absolute error < 0.000066
> (tvar_70_genbeta_btcgains <- (1/(1-0.7))*0.506449  )
[1] 1.688163
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.3380141 with absolute error < 0.000089
> (tvar_80_genbeta_btcgains <- (1/(1-0.8))*0.3380141   )
[1] 1.690071
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.1692881 with absolute error < 0.000089
> (tvar_90_genbeta_btcgains <- (1/(1-0.9))*0.1692881   )
[1] 1.692881
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.08476579 with absolute error < 0.000089
> (tvar_95_genbeta_btcgains <- (1/(1-0.95))*0.08476579   )
[1] 1.695316
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.01700081 with absolute error < 0.000017
> (tvar_99_genbeta_btcgains <- (1/(1-0.99))*0.01700081   )
[1] 1.700081
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.00850937 with absolute error < 0.0000084
> (tvar_99.5_genbeta_btcgains <- (1/(1-0.995))*0.00850937  )
[1] 1.701874


INVERSE TRANSFORMED GAMMA:
  Goodness of fit
> btcgains_invtrgamma = fitdist(btcgains, "invtrgamma", method = "mle")
> summary(btcgains_invtrgamma)
Fitting of the distribution ' invtrgamma ' by maximum likelihood 
Parameters : 
  estimate
shape1     0.02490542
shape2  1534.67304752
scale  15269.59201020
Loglikelihood:  -2147483647   AIC:  4294967300   BIC:  4294967315 
> #Goodness of fit tests (excluding NLL)
  > gofstat(btcgains_invtrgamma, fitnames = "Fitting Inverse Transformed Gamma to BTC gains")
Goodness-of-fit statistics
Fitting Inverse Transformed Gamma to BTC gains
Kolmogorov-Smirnov statistic                                        1.0000
Cramer-von Mises statistic                                        316.3333
Anderson-Darling statistic                                             Inf

Goodness-of-fit criteria
Fitting Inverse Transformed Gamma to BTC gains
Akaike's Information Criterion                                  4294967300
Bayesian Information Criterion                                  4294967315
Risk measures
> #VaR 70%
> (var_70_invtrgamma_btcgains <- qinvtrgamma(0.70, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 15763.96
> #VaR 80%
> (var_80_invtrgamma_btcgains <- qinvtrgamma(0.80, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 15932.07
> #VaR 90%
> (var_90_invtrgamma_btcgains <- qinvtrgamma(0.90, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 16223.64
> #VaR 95%
> (var_95_invtrgamma_btcgains <- qinvtrgamma(0.95, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 16520.54
> #VaR 99%
> (var_99_invtrgamma_btcgains <- qinvtrgamma(0.99, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 17231.04
> #VaR 99.5%
> (var_95.5_invtrgamma_btcgains <- qinvtrgamma(0.995, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma))
[1] 17546.37
> f = function(x) qinvtrgamma(x, shape1.gain_invtrgamma, shape2.gain_invtrgamma, 1/scale.gain_invtrgamma)
> #TVaR 70%
> integrate(f, lower = 0.7, upper = 1)
4856.241 with absolute error < 0.0000000000064
> (tvar_70_invtrgamma_btcgains <- (1/(1-0.7))*4856.241 )
[1] 16187.47
> #TVaR 80%
> integrate(f, lower = 0.8, upper = 1)
3272.021 with absolute error < 0.0000000000045
> (tvar_80_invtrgamma_btcgains <- (1/(1-0.8))*3272.021  ) 
[1] 16360.11
> #TVaR 90%
> integrate(f, lower = 0.9, upper = 1)
1665.95 with absolute error < 0.0000000000018
> (tvar_90_invtrgamma_btcgains <- (1/(1-0.9))*1665.95  )
[1] 16659.5
> #TVaR 95%
> integrate(f, lower = 0.95, upper = 1)
848.2188 with absolute error < 0.0000000000048
> (tvar_95_invtrgamma_btcgains <- (1/(1-0.95))*848.2188  )
[1] 16964.38
> #TVaR 99%
> integrate(f, lower = 0.99, upper = 1)
176.9397 with absolute error < 0.000000000002
> (tvar_99_invtrgamma_btcgains <- (1/(1-0.99))*176.9397  )
[1] 17693.97
> #TVaR 99.5%
> integrate(f, lower = 0.995, upper = 1)
90.08886 with absolute error < 0.0000000000023
> (tvar_99.5_invtrgamma_btcgains <- (1/(1-0.995))*90.08886  )
[1] 18017.77

 
LOGLOGISTIC:
Goodness of fit
> btcgains_llogis = fitdist(btcgains, "llogis",start = list(shape=1 ,rate=1), method = "mle")
> summary(btcgains_llogis)
Fitting of the distribution ' llogis ' by maximum likelihood 
Parameters : 
      estimate Std. Error
shape  1.40109 0.03808896
rate  69.23238 2.77703631
Loglikelihood:  2553.822   AIC:  -5103.644   BIC:  -5093.933 
Correlation matrix:
            shape        rate
shape  1.00000000 -0.06214605
rate  -0.06214605  1.00000000
> #Goodness of fit tests (excluding NLL)
> gofstat(btcgains_llogis, fitnames = "Fitting Loglogistic to BTC gains")
Goodness-of-fit statistics
                             Fitting Loglogistic to BTC gains
Kolmogorov-Smirnov statistic                       0.05039974
Cramer-von Mises statistic                         0.67148960
Anderson-Darling statistic                         6.92448839

Goodness-of-fit criteria
                               Fitting Loglogistic to BTC gains
Akaike's Information Criterion                        -5103.644
Bayesian Information Criterion                        -5093.933
Risk measures
> #VaR 70%
  > (var_70_llogis_btcgains <- qllogis(0.70, shape.gain_llogis, rate.gain_llogis))
[1] 0.02644397
> #VaR 80%
  > (var_80_llogis_btcgains <- qllogis(0.80, shape.gain_llogis, rate.gain_llogis))
[1] 0.0388507
> #VaR 90%
  > (var_90_llogis_btcgains <- qllogis(0.90, shape.gain_llogis, rate.gain_llogis))
[1] 0.0693046
> #VaR 95%
  > (var_95_llogis_btcgains <- qllogis(0.95, shape.gain_llogis, rate.gain_llogis))
[1] 0.1181341
> #VaR 99%
  > (var_99_llogis_btcgains <- qparalogis(0.99, shape.gain_llogis, rate.gain_llogis))
[1] 0.146793
> #VaR 99.5%
  > (var_95.5_llogis_btcgains <- qllogis(0.995, shape.gain_llogis, rate.gain_llogis))
[1] 0.6316125
> f = function(x) qllogis(x, shape.gain_llogis, rate.gain_llogis)
> #TVaR 70%
  > integrate(f, lower = 0.7, upper = 1)
0.03399735 with absolute error < 0.00000032
> (tvar_70_llogis_btcgains <- (1/(1-0.7))*0.03399735   )
[1] 0.1133245
> #TVaR 80%
  > integrate(f, lower = 0.8, upper = 1)
0.03080028 with absolute error < 0.00000019
> (tvar_80_llogis_btcgains <- (1/(1-0.8))*0.03080028   )
[1] 0.1540014
> #TVaR 90%
  > integrate(f, lower = 0.9, upper = 1)
0.0256822 with absolute error < 0.000000078
> (tvar_90_llogis_btcgains <- (1/(1-0.9))*0.0256822   )
[1] 0.256822
> #TVaR 95%
  > integrate(f, lower = 0.95, upper = 1)
0.02123202 with absolute error < 0.000000032
> (tvar_95_llogis_btcgains <- (1/(1-0.95))*0.02123202   )
[1] 0.4246404
> #TVaR 99%
  > integrate(f, lower = 0.99, upper = 1)
0.0134798 with absolute error < 0.000000004
> (tvar_99_llogis_btcgains <- (1/(1-0.99))*0.0134798   )
[1] 1.34798
> #TVaR 99.5%
  > integrate(f, lower = 0.995, upper = 1)
0.0110625 with absolute error < 0.0000000017
> (tvar_99.5_llogis_btcgains <- (1/(1-0.995))*0.0110625   )
[1] 2.2125

