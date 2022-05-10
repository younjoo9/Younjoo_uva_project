library(dplyr)
FVX <- read.csv("C:\\Downloads\\FVX.csv", header = TRUE)


#### 1 (A)
## get rid of non market opneing days data.
FVX <- filter(FVX, Open!='null')
#head(FVX)

### make it workable usable instead of strings
FVX[,-1] <- apply(FVX[,-1], 2, function(x){as.numeric(x)})


#### 1 (B)
### added is log diff column with first value NA as no previous data
FVX <- mutate(FVX, logr = c(NA, diff(log(Adj.Close))))
logdiff = FVX$logr[-1]

### converting the date string into Date data type
FVX$Date <- as.Date(FVX$Date, "%Y-%m-%d")

par(mfrow=c(1,2))
plot(FVX$Date[-1],logdiff,type='l', xlab = "Time",ylab="Log difference")
plot(FVX$Date,FVX$Adj.Close,type='l', xlab = "Time", ylab = 'Adjusted Closing Rates')

### ACF manual
ac_f <- rep(0,50)
n = length(logdiff)

for(i in 0:50){
  ac_f[i+1] = cor(logdiff[(i+1):n],logdiff[1:(n-i)]) 
}

par(mfrow=c(1,1))
plot(0:50,ac_f,type='h',col='blue', xlab='k',ylab='Autocorrelation')
abline(h=0, col="black")

##ACF shows an exponential decay therefore we believe that the log return of the time series is stationary
## Furthermore the graph for the log difference looks stable with constant mean around 0 and doesn't appear to
## Have any correlation. 


### c

trainFVX <- logdiff[1:(n-20)]
testFVX <- logdiff[(n-19):n]


X = cbind(rep(1,length(trainFVX)-2),trainFVX[2:(length(trainFVX)-1)],trainFVX[1:(length(trainFVX)-2)])
b_ols = solve(t(X)%*%X,t(X)%*%trainFVX[3:length(trainFVX)])
error <- rep(0,length(trainFVX))

llFVX <- function(para){
  y = trainFVX
  sigmasq = var(y)
  total = 0
  for(i in 3:length(y)){
    error[i] <- y[i]-para[1]-para[2]*y[i-1]-para[3]*y[i-2]
    total = total + (-1/2*log(2*pi)-1/2*log(sigmasq)-(1/(2*sigmasq))*(error[i]^2))
  }
  return(-total)
}

b_mle <- optim(c(0.2,0.5,0.5),llFVX)$par

####
## Results indicate that rt roughly 0 - 0.033r_t-1 - 0.0566 rt-2.
## Results for both MLE and OLS are quite similar. 
cbind(b_ols,b_mle)

####

library(tseries)
model <- arma(trainFVX,order=c(2,0))
b_tseries <- c(model$coef[3],model$coef[1:2])
cbind(b_ols,b_mle,b_tseries)

### similar
par(mfrow=c(1,1))
x = 1:length(trainFVX)
plot(x, trainFVX,type = 'l', col='blue',xlab = 'Time',ylab='log returns') + points(x,model$fitted.values,type='l',col='red')
legend("topleft",legend=c("Fitted Returns", "Actual Returns"), col=c("red", "blue"), lty=1, cex=0.8)

### Quality of fit quite poor
## (e)

normtest <- jarque.bera.test(na.omit(model$residuals))
print(normtest$p.value < 0.05)

# reject null hypothesis therefore at 95% not normal


# (f)
#head(testFVX)
forecast = rep(0,20)
forecast[1] = sum(b_tseries*c(1,trainFVX[length(trainFVX)],trainFVX[length(trainFVX)-1]))
forecast[2] = sum(b_tseries*c(1,forecast[1],trainFVX[length(trainFVX)]))

for(i in 3:20){
    forecast[i] = sum(b_tseries*c(1,forecast[i-1],forecast[i-2]))
}

par(mfrow=c(1,1))
x = 1:20
plot(x, testFVX,type = 'l', col='blue',xlab = 'Time',ylab='log returns') + points(x,forecast,type='l',col='red')
legend("topleft",legend=c("Fitted Returns", "Actual Returns"), col=c("red", "blue"), lty=1, cex=0.8)

######## (4)
## (a)
AEX <- read.csv("C:\\Downloads\\AEX.csv", header = TRUE)
AEX <- filter(AEX, Open!='null')

### make it workable usable instead of strings
AEX[,-1] <- apply(AEX[,-1], 2, function(x){as.numeric(x)})
AEX$Date <- as.Date(AEX$Date, "%Y-%m-%d")


#### (B)
### added is log diff column with first value NA as no previous data
AEX <- mutate(AEX, logr = c(NA, diff(log(Adj.Close))))
logre = AEX$logr[-1]

par(mfrow=c(1,2))
plot(AEX$Date[-1],logre,type='l',xlab = 'Date',ylab = 'log return')
plot(AEX$Date[-1],logre^2,type='l',xlab='Date',ylab = 'square log return')

# Volatility not constant over time

### (C)
n = length(logre)
trainAEX <- logre[1:(n-20)]
testAEX <- logre[(n-19):n]

llog <- function(par){
  total = 0
  y = trainAEX
  N = length(y)
  sig_sq = c(var(y), rep(0,(N-1)))
  sig_sq[2] = par[1] + par[2]*0 + par[3]*sig_sq[1]
  
  for(i in 3:N){
    sig_sq[i] = par[1] + par[2]*(y[i-1]^2)+par[3]*sig_sq[i-1]
    total = total + (-0.5*log(2*pi) -0.5*log(sig_sq[i])-0.5*(y[i]^2/sig_sq[i]))
  }
  return(-total)
}

b_mle <- optim(c(0.1,0.4,0.4),llog,hessian=FALSE)$par
b_mle
## 0.11 of the volatility shock today feeds through to tomorrow and we see that the coefficient
## of error_t-1 and sigma_t-1 sums nearly to 1, it suggests that volatility is persistance. 

### (d)

normtest <- jarque.bera.test(na.omit(trainAEX))
print(normtest$p.value < 0.05)  ## TRUE

# therefore assumption rejected, one possibility could be that errors are t-distributed.

### (e)
library(rugarch)
model = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model=list(model="sGARCH",garchOrder=c(1,1)),distribution.model='norm')
model_fit = ugarchfit(spec=model,data=trainAEX)
b_ug = model_fit@fit$coef[2:4]
cbind(b_ug,b_mle) ## similar

fitcvar = model_fit@fit$var
x = 1:length(fitcvar)
sqr_resid = model_fit@fit$residuals^2

par(mfrow=c(1,1))
plot(x, sqr_resid,type = 'l', col='blue',xlab = 'Time',ylab='square residuals') + points(x, fitcvar,type='l',col='red')
legend("topleft",legend=c("Fitted Conditional Variance", "Squared residuals"), col=c("red", "blue"), lty=1, cex=0.8)

### General fit is good except for squared residuals which are very large


## (f)
forecast = rep(0,20)
forecast[1] = sum(c(1,sqr_resid[length(sqr_resid)],fitcvar[length(fitcvar)])*b_ug)

for(i in 2:20){
  forecast[i] = sum(c(1,testAEX[i-1]^2,forecast[i-1])*b_ug)
}

x <- 1:20
plot(x, testAEX^2,type = 'l', col='blue',xlab = 'Time',ylab='Squared returns') + points(x, forecast,type='l',col='red')
legend("topleft",legend=c("Forecast Conditional Variance", "Squared returns"), col=c("red", "blue"), lty=1, cex=0.8)

#### (g)

BTC <- read.csv("C:\\Downloads\\BTC.csv", header = TRUE)
BTC <- filter(BTC, Open!='null')

### make it workable usable instead of strings
BTC[,-1] <- apply(BTC[,-1], 2, function(x){as.numeric(x)})
BTC$Date <- as.Date(BTC$Date, "%Y-%m-%d")

BTC <- mutate(BTC, logr = c(NA, diff(log(Adj.Close))))
logre = BTC$logr[-1]

par(mfrow=c(1,2))
plot(BTC$Date[-1],logre,type='l',xlab = 'Date',ylab = 'log return')
plot(BTC$Date[-1],logre^2,type='l',xlab='Date',ylab = 'square log return')

# Volatility not constant over time

### (C)
n = length(logre)
trainBTC <- logre[1:(n-20)]
testBTC <- logre[(n-19):n]

llog <- function(par){
  total = 0
  y = trainBTC
  N = length(y)
  sig_sq = c(var(y), rep(0,(N-1)))
  sig_sq[2] = par[1] + par[2]*0 + par[3]*sig_sq[1]
  
  for(i in 3:N){
    sig_sq[i] = par[1] + par[2]*(y[i-1]^2)+par[3]*sig_sq[i-1]
    total = total + (-0.5*log(2*pi) -0.5*log(sig_sq[i])-0.5*(y[i]^2/sig_sq[i]))
  }
  return(-total)
}

b_mle <- optim(c(0.1,0.4,0.4),llog,hessian=FALSE)$par
b_mle
## 0.13 of the volatility shock today feeds through to tomorrow and we see that the coefficient
## of error_t-1 and sigma_t-1 sums nearly to 1, it suggests that volatility might be persistance. 

normtest <- jarque.bera.test(na.omit(trainBTC))
print(normtest$p.value < 0.05)  ## TRUE

# therefore assumption rejected, one possibility could be that errors are t-distributed.

model = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model=list(model="sGARCH",garchOrder=c(1,1)),distribution.model='norm')
model_fit = ugarchfit(spec=model,data=trainBTC)
b_ug = model_fit@fit$coef[2:4]
cbind(b_ug,b_mle) ## similar

fitcvar = model_fit@fit$var
x = 1:length(fitcvar)
sqr_resid = model_fit@fit$residuals^2

par(mfrow=c(1,1))
plot(x, sqr_resid,type = 'l', col='blue',xlab = 'Time',ylab='square residuals') + points(x, fitcvar,type='l',col='red')
legend("topleft",legend=c("Fitted Conditional Variance", "Squared residuals"), col=c("red", "blue"), lty=1, cex=0.8)

### General fit is good except for squared residuals which are very large

forecast = rep(0,20)
forecast[1] = sum(c(1,sqr_resid[length(sqr_resid)],fitcvar[length(fitcvar)])*b_ug)

for(i in 2:20){
  forecast[i] = sum(c(1,testBTC[i-1]^2,forecast[i-1])*b_ug)
}

x <- 1:20
plot(x, testBTC^2,type = 'l', col='blue',xlab = 'Time',ylab='Squared returns') + points(x, forecast,type='l',col='red')
legend("topleft",legend=c("Forecast Conditional Variance", "Squared returns"), col=c("red", "blue"), lty=1, cex=0.8)












































































