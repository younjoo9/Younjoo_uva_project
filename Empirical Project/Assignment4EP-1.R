### Question 4 (a) & (b)

library(tidyverse)
library(svd)
library(gridExtra)

setwd("C:/Downloads")
DeathUK <- read.table("Deaths_1x1.txt",header=TRUE,skip=2)
DeathUK$Age <- as.character(DeathUK$Age)
DeathUK$Age <- ifelse(DeathUK$Age=="110+","110",DeathUK$Age)
DeathUK$Age <- as.numeric(DeathUK$Age)


DeathNL <- read.table("Deaths_1x1_NL.txt",header=TRUE,skip=2)
DeathNL$Age <- as.character(DeathNL$Age)
DeathNL$Age <- ifelse(DeathNL$Age=="110+","110",DeathNL$Age)
DeathNL$Age <- as.numeric(DeathNL$Age)

ExposureUK <- read.table("Exposures_1x1.txt",header=TRUE,skip=2)
ExposureUK$Age <- as.character(ExposureUK$Age)
ExposureUK$Age <- ifelse(ExposureUK$Age=="110+","110",ExposureUK$Age)
ExposureUK$Age <- as.numeric(ExposureUK$Age)

ExposureNL <- read.table("Exposures_1x1_NL.txt",header=TRUE,skip=2)
ExposureNL$Age <- as.character(ExposureNL$Age)
ExposureNL$Age <- ifelse(ExposureNL$Age=="110+","110",ExposureNL$Age)
ExposureNL$Age <- as.numeric(ExposureNL$Age)

UK <- data.frame(Year = DeathUK$Year, Age = DeathUK$Age, DeathM = DeathUK$Male, DeathF = DeathUK$Female, ExposureM = ExposureUK$Male, ExposureF = ExposureUK$Female)
UK <- mutate(UK, MuM = DeathM/ExposureM, MuF = DeathF/ExposureF)
NL <- data.frame(Year = DeathNL$Year, Age = DeathNL$Age, DeathM = DeathNL$Male, DeathF = DeathNL$Female, ExposureM = ExposureNL$Male, ExposureF = ExposureNL$Female)
NL <- mutate(NL, MuM = DeathM/ExposureM, MuF = DeathF/ExposureF)


UK <- filter(UK, Year <= 2008 & Year >= 1970 & Age <= 100 & Age >= 0)
NL <- filter(NL, Year <= 2008 & Year >= 1970 & Age <= 100 & Age >= 0)


UK1970 <- filter(UK, Year==1970 & Age >= 25)
UK2008 <- filter(UK, Year==2008 & Age >= 25)

NL1970 <- filter(NL, Year==1970 & Age >= 25)
NL2008 <- filter(NL, Year==2008 & Age >= 25)


## UK

df <- bind_cols(select(UK1970, Age, MuM1970 = MuM, MuF1970 = MuF),select(UK2008,MuM2008 = MuM, MuF2008 = MuF))

df <- df %>%
  select(Age, Male1970 = MuM1970, Female1970 = MuF1970, Male2008 = MuM2008, Female2008 = MuF2008) %>%
  gather(key = "Variables", value = "value", -Age)

## Log 
UKD <- ggplot(df, aes(x = Age, y = log(value))) + 
  geom_line(aes(color = Variables, linetype = Variables))  + labs(title = "UK", y = "") +
  theme(legend.position=c(0.2,0.8),legend.title = element_text(face="bold"), legend.background = element_rect(fill="lightblue",linetype="solid",size=0.5,colour ="darkblue")) + theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

## Netherlands
df <- bind_cols(select(NL1970, Age, MuM1970 = MuM, MuF1970 = MuF),select(NL2008,MuM2008 = MuM, MuF2008 = MuF))

df <- df %>%
  select(Age, Male1970 = MuM1970, Female1970 = MuF1970, Male2008 = MuM2008, Female2008 = MuF2008) %>%
  gather(key = "Variables", value = "value", -Age)

NLD <- ggplot(df, aes(x = Age, y = log(value))) + 
  geom_line(aes(color = Variables, linetype = Variables)) + labs(title = "NL", y = "") +
  theme(legend.position=c(0.2,0.8),legend.title = element_text(face="bold"), legend.background = element_rect(fill="orange",linetype="solid",size=0.5,colour ="darkred")) + theme(
    panel.background = element_rect(fill = "lightgoldenrod",
                                    colour = "lightgoldenrod",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

grid.arrange(UKD,NLD,ncol=2)

## Question 4c

q1970mNL <- 1-exp(-filter(NL, Year==1970)$MuM)
q1970fNL <- 1-exp(-filter(NL, Year==1970)$MuF)
q2008mNL <- 1-exp(-filter(NL, Year==2008)$MuM)
q2008fNL <- 1-exp(-filter(NL, Year==2008)$MuF)


q1970mUK <- 1-exp(-filter(UK, Year==1970)$MuM)
q1970fUK <- 1-exp(-filter(UK, Year==1970)$MuF)
q2008mUK <- 1-exp(-filter(UK, Year==2008)$MuM)
q2008fUK <- 1-exp(-filter(UK, Year==2008)$MuF)

## Complete life expectancy UK and sexes
sum(cumprod(1-q1970mUK[(65+1):100]))+0.5
sum(cumprod(1-q1970mUK[(0+1):100]))+0.5
sum(cumprod(1-q2008mUK[(65+1):100]))+0.5
sum(cumprod(1-q2008mUK[(0+1):100]))+0.5
sum(cumprod(1-q1970fUK[(65+1):100]))+0.5
sum(cumprod(1-q1970fUK[(0+1):100]))+0.5
sum(cumprod(1-q2008fUK[(65+1):100]))+0.5
sum(cumprod(1-q2008fUK[(0+1):100]))+0.5

## Complete life expectancy NL and sexes
sum(cumprod(1-q1970mNL[(65+1):100]))+0.5
sum(cumprod(1-q1970mNL[(0+1):100]))+0.5
sum(cumprod(1-q2008mNL[(65+1):100]))+0.5
sum(cumprod(1-q2008mNL[(0+1):100]))+0.5
sum(cumprod(1-q1970fNL[(65+1):100]))+0.5
sum(cumprod(1-q1970fNL[(0+1):100]))+0.5
sum(cumprod(1-q2008fNL[(65+1):100]))+0.5
sum(cumprod(1-q2008fNL[(0+1):100]))+0.5

### 5

miniprob <- function(par){
  data <- filter(UK, Year==2008 & Age >= 25 & Age <= 90)$MuF 
  sum( (data- exp(par[1]+par[2]*(25:90)))^2)
}


miniprob2 <- function(par){
  data <- filter(UK, Year==2008 & Age >= 25 & Age <= 90)$MuF 
  sum( (log(data)- (par[1]+par[2]*(25:90)))^2)
}


miniprob3 <- function(par){
  data <- filter(UK, Year==2008 & Age >= 25 & Age <= 90)$MuF
  nd <- filter(UK, Year==2008 & Age >= 25 & Age <= 90)$DeathF
  sum(nd*(data- exp(par[1]+par[2]*(25:90)))^2)
}

result <- optim(par=c(-13,0.1),miniprob)
r1 <- result$par
result <- optim(par=c(-11,0.2),miniprob2)
r2 <- result$par
result <- optim(par=c(-12.6,0.11),miniprob3)
r3 <- result$par

data <- filter(UK, Year==2008 & Age >= 25 & Age <= 90)$MuF
df <- data.frame(Age = 25:90)
df <- mutate(df, Observed = data, Regular = exp(r1[1]+r1[2]*Age), Log = exp(r2[1]+r2[2]*Age), WLS = exp(r3[1]+r3[2]*Age))

df <- df %>%
  gather(key = "Methods", value = "value", -Age)

## no log
UKF <- ggplot(df, aes(x = Age, y = value)) + 
  geom_line(aes(color = Methods, linetype = Methods)) + labs(title = "UK Female", y = "") +
  theme(legend.position = c(0.2,0.8),legend.title = element_text(face="bold"), legend.background = element_rect(fill="pink",linetype="solid",size=0.5,colour ="darkred"))

## log
UKFL <- ggplot(df, aes(x = Age, y = log(value))) + 
  geom_line(aes(color = Methods, linetype = Methods)) + labs(title = "UK Female Log", y = "") + 
  theme(legend.position = c(0.2,0.8),legend.title = element_text(face="bold"), legend.background = element_rect(fill="pink",linetype="solid",size=0.5,colour ="darkred"))

grid.arrange(UKF,UKFL,ncol=2)


# 6 (a)
#UK100 <- filter(UK, Age <= 100)
#NL100 <- filter(NL, Age <= 100)

MuMUK <- matrix(0,101,39)
MuMNL <- matrix(0,101,39)

MuFUK <- matrix(0,101,39)
MuFNL <- matrix(0,101,39)


## progress to make R 101 x 39 matrix 

for(y in 0:38){
  MuMUK[,y%%39+1] <- log(UK[(1:101)+(y*101),]$MuM)
}

for(y in 0:38){
  MuMNL[,y%%39+1] <- log(NL[(1:101)+(y*101),]$MuM)
}

for(y in 0:38){
  MuFUK[,y%%39+1] <- log(UK[(1:101)+(y*101),]$MuF)
}

for(y in 0:38){
  MuFNL[,y%%39+1] <- log(NL[(1:101)+(y*101),]$MuF)
}


alphaMUK <- matrix(apply(MuMUK,1,mean),101,39)
alphaMNL <- matrix(apply(MuMNL,1,mean),101,39)
alphaFUK <- matrix(apply(MuFUK,1,mean),101,39)
alphaFNL <- matrix(apply(MuFNL,1,mean),101,39)

RMUK <- MuMUK - alphaMUK
RFUK <- MuFUK - alphaFUK
RMNL <- MuMNL - alphaMNL
RFNL <- MuFNL - alphaFNL

svdr <- svd(RMUK,1,1)
bMUK <- -svdr$u
ktMUK <- -svdr$v*svdr$d[1]
## check
sum((bMUK)^2); sum(ktMUK) 

svdr <- svd(RFUK,1,1)
bFUK <- -svdr$u
ktFUK <- -svdr$v*svdr$d[1]
## check
sum((bFUK)^2); sum(ktFUK) 


svdr <- svd(RMNL,1,1)
bMNL <- -svdr$u
ktMNL <- -svdr$v*svdr$d[1]
## check
sum((bMNL)^2); sum(ktMNL) 


svdr <- svd(RFNL,1,1)
bFNL <- -svdr$u
ktFNL <- -svdr$v*svdr$d[1]
## check
sum((bFNL)^2); sum(ktFNL) 



### Female Alpha Beta Kt

df <- data.frame(Age = 0:100)
df <- mutate(df, AlphaUK = alphaFUK[,1], AlphaNL = alphaFNL[,1])

df <- df %>%
  gather(key = "Country", value = "value", -Age)

FA <- ggplot(df, aes(x = Age, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Alpha Female") +
  theme(legend.position = c(0.1,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="pink",linetype="solid",size=0.5,colour ="darkred")) + ylab(NULL)


df <- data.frame(Age = 0:100)
df <- mutate(df, BetaUK = bFUK, BetaNL = bFNL)

df <- df %>%
  gather(key = "Country", value = "value", -Age)

FB <- ggplot(df, aes(x = Age, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Beta Female") +
  theme(legend.position = c(0.8,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="pink",linetype="solid",size=0.5,colour ="darkred")) + ylab(NULL)


df <- data.frame(Year = 1970:2008)
df <- mutate(df, KtUK = ktFUK, KtNL = ktFNL)

df <- df %>%
  gather(key = "Country", value = "value", -Year)

FK <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Kt Female") +
  theme(legend.position = c(0.8,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="pink",linetype="solid",size=0.5,colour ="darkred")) + ylab(NULL)


### Male Alpha Beta Kt
df <- data.frame(Age = 0:100)
df <- mutate(df, AlphaUK = alphaMUK[,1], AlphaNL = alphaMNL[,1])

df <- df %>%
  gather(key = "Country", value = "value", -Age)

MA <- ggplot(df, aes(x = Age, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Alpha Male") +
  theme(legend.position = c(0.1,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="lightblue",linetype="solid",size=0.5,colour ="darkblue")) + ylab(NULL)

df <- data.frame(Age = 0:100)
df <- mutate(df, BetaUK = bMUK, BetaNL = bMNL)

df <- df %>%
  gather(key = "Country", value = "value", -Age)

MB <- ggplot(df, aes(x = Age, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Beta Male") +
  theme(legend.position = c(0.8,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="lightblue",linetype="solid",size=0.5,colour ="darkblue")) + ylab(NULL)

df <- data.frame(Year = 1970:2008)
df <- mutate(df, KtUK = ktMUK, KtNL = ktMNL)

df <- df %>%
  gather(key = "Country", value = "value", -Year)

MK <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = Country, linetype = Country)) + labs(title = "Kt Male") +
  theme(legend.position = c(0.8,0.9),legend.title = element_text(face="bold"), legend.background = element_rect(fill="lightblue",linetype="solid",size=0.5,colour ="darkblue")) + ylab(NULL)


grid.arrange(FA,MA,ncol=2)
grid.arrange(FB,MB,ncol=2)
grid.arrange(FK,MK,ncol=2)


## 7 (a)

thetaMUK <- mean(diff(ktMUK))
sigmaMUK <- sd(diff(ktMUK))

thetaFUK <- mean(diff(ktFUK))
sigmaFUK <- sd(diff(ktFUK))

thetaMNL <- mean(diff(ktMNL))
sigmaMNL <- sd(diff(ktMNL))

thetaFNL <- mean(diff(ktFNL))
sigmaFNL <- sd(diff(ktFNL))


##7b.  65 year old Male NL and UK

alphaMUK65 <- alphaMUK[65+1,1]
alphaMNL65 <- alphaMNL[65+1,1]
bMUK65 <- bMUK[65+1]
bMNL65 <- bMNL[65+1]
kt2008MUK <- ktMUK[39]
kt2008MNL <- ktMNL[39]


## simulation
random25 <- lapply(1:25, function(x){set.seed(x);rnorm(50)})

ktMUKforecast <- rep(list(c(kt2008MUK,rep(0,50))),25)
ktMNLforecast <- rep(list(c(kt2008MNL,rep(0,50))),25)


## Recursion to get Kt+1 UK

for(j in 1:25){ ## each simulation
  for(i in 1:50){
    ktMUKforecast[[j]][i+1] <- ktMUKforecast[[j]][i] + thetaMUK + sigmaMUK*random25[[j]][i]
  }
}

## Recursion to get Kt+1 NL

for(j in 1:25){
  for(i in 1:50){
    ktMNLforecast[[j]][i+1] <- ktMNLforecast[[j]][i] + thetaMNL + sigmaMNL*random25[[j]][i]
  }
}

## get mortality rates

q65MUKsims <- rep(list(c(rep(0,51))),25)
q65MNLsims <- rep(list(c(rep(0,51))),25)


## UK
for(j in 1:25){
  q65MUKsims[[j]] <- 1-exp(-exp(alphaMUK65 + bMUK65*ktMUKforecast[[j]]))
}

## NL
for(j in 1:25){
  q65MNLsims[[j]] <- 1-exp(-exp(alphaMNL65 + bMNL65*ktMNLforecast[[j]]))
}


dfMUK <- data.frame(Year=2008:2058)
df <- data.frame(matrix(unlist(q65MUKsims), ncol=length(q65MUKsims)))

dfMUK <- bind_cols(dfMUK,df)

df <- dfMUK %>%
  gather(key = "Simulation", value = "value", -Year)

m25 <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = Simulation, linetype = Simulation)) + 
  labs(title = "UK Male", y = "") + theme(legend.position='none') + theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )


dfMNL <- data.frame(Year=2008:2058)
df <- data.frame(matrix(unlist(q65MNLsims), ncol=length(q65MNLsims)))

dfMNL <- bind_cols(dfMNL,df)

df <- dfMNL %>%
  gather(key = "Simulation", value = "value", -Year)

N25 <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = Simulation, linetype = Simulation)) + 
  labs(title = "NL Male", y = "") + theme(legend.position='none') + theme(
    panel.background = element_rect(fill = "lightgoldenrod",
                                    colour = "lightgoldenrod",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )


grid.arrange(m25, N25,ncol=2)


#### 7 (i)
## max age is 120 therefore formula is \sum_{k=1}^{120-65} kPx (t)
## therefore need 1p65 1p66 ... ... 1p119
## therefore need mu_65,2008 mu_66,2009 .... ... mu_119,2062


H <- function(x){
  return(1/(1+exp(-x)))
}

invH <- function(x){
  return(-log((1/x)-1))
}

w <- function(z,x){
  return(1/11 + (z-85)*(x-85)/110)
}


kt2008FUK <- ktFUK[39] ## Kt at 2008 UK
kt2008FNL <- ktFNL[39] ## kt at 2008 NL
alphaFUK6590 <- alphaFUK[(65+1):(90+1),1] ## alpha 65, alpha 66 .... alpha 90
alphaFNL6590 <- alphaFNL[(65+1):(90+1),1] ## alpha 65, alpha 66 .... alpha 90
bFUK6590 <- bFUK[(65+1):(90+1)]
bFNL6590 <- bFNL[(65+1):(90+1)]


random100054 <- lapply(1:1000, function(x){set.seed(x);rnorm(54)})
ktFUK0862 <- rep(list(c(kt2008FUK,rep(0,54))),1000)
ktFNL0862 <- rep(list(c(kt2008FNL,rep(0,54))),1000)


## Get ktFUK 2008, 2009, ..., 2062

for(j in 1:1000){
  for(i in 1:54){
    ktFUK0862[[j]][i+1] <- ktFUK0862[[j]][i] + thetaFUK + sigmaFUK*random100054[[j]][i]
  }
}

for(j in 1:1000){
  for(i in 1:54){
    ktFNL0862[[j]][i+1] <- ktFNL0862[[j]][i] + thetaFNL + sigmaFNL*random100054[[j]][i]
  }
}

### now ktFUK0862 will have 55 values ktFUK 2008, 2009, ... 2062 ktFUK0892[26] = k2033

## To create mu_65,2008 till mu_90,2033

muFUK0833 <- rep(list(rep(0,26)),1000)
muFNL0833 <- rep(list(rep(0,26)),1000)

## mu65,2008 = exp(alpha65 + b65*k2008)

for(j in 1:1000){
  muFUK0833[[j]] <- exp(alphaFUK6590 + bFUK6590*ktFUK0862[[j]][1:26]) ## mu 65,2008 till 90,2033
}

for(j in 1:1000){
  muFNL0833[[j]] <- exp(alphaFNL6590 + bFNL6590*ktFNL0862[[j]][1:26]) ## mu 65,2008 till 90,2033
}


## mu 80, 2034 till 90, 2034
## mu 80, 2035 till 90, 2035

## The mu matrix of Kannisto used to calculate mu 91,2034 till mu 119, 2062
## matrix looks like mu80,2034 mu81,2034  .........  mu90,2034
##                   mu80,2035 mu81,2035  .........  mu90,2035
##                   .
##                   .
##                   mu80,2062 mu81,2062 ..........  mu90,2062

## Why like this because

## mu91,2034 = H(... mu80,2034 .. mu81,2034 .. mu82,2034 ... mu90,2034)
## mu92,2035 = H(... mu80,2035 .. mu81,2035 .. mu82,2035 ... mu90,2035)

muFUKmatrix <- rep(list(matrix(0,29,11)),1000)
muFNLmatrix <- rep(list(matrix(0,29,11)),1000)

for(j in 1:1000){
  for(i in 1:29){
    muFUKmatrix[[j]][i,] <- exp(alphaFUK6590[16:26] + bFUK6590[16:26]*ktFUK0862[[j]][i+26])
  }
}

for(j in 1:1000){
  for(i in 1:29){
    muFNLmatrix[[j]][i,] <- exp(alphaFNL6590[16:26] + bFNL6590[16:26]*ktFNL0862[[j]][i+26])
  }
}

## to calculate mu 91,2034 till mu 119, 2062

x91119 <- 91:119
muFUK3462 <- rep(list(0,29),1000)
muFNL3462 <- rep(list(0,29),1000)

for(j in 1:1000){
  for(i in 1:29){
    muFUK3462[[j]][i] <- H(sum(w(80:90,x91119[i])*invH(muFUKmatrix[[j]][i,])))
  }
}

for(j in 1:1000){
  for(i in 1:29){
    muFNL3462[[j]][i] <- H(sum(w(80:90,x91119[i])*invH(muFNLmatrix[[j]][i,])))
  }
}



## combine all mu so mu 65,2008 till mu 119, 2062

muFUK0862 <- rep(list(rep(0,55)),1000)
muFNL0862 <- rep(list(rep(0,55)),1000)

for(j in 1:1000){
  muFUK0862[[j]] <- c(muFUK0833[[j]], muFUK3462[[j]]) 
}

for(j in 1:1000){
  muFNL0862[[j]] <- c(muFNL0833[[j]], muFNL3462[[j]]) 
}


p65coh0862FUK <- lapply(muFUK0862, function(x) {exp(-1*x)})
p65coh0862FNL <- lapply(muFNL0862, function(x) {exp(-1*x)})


completeFUK <- lapply(p65coh0862FUK, function(x){sum(cumprod(x)) + 0.5})
completeFNL <- lapply(p65coh0862FNL, function(x){sum(cumprod(x)) + 0.5})

plot_multi_histogram <- function(df, feature, label_column,means,titles) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(xintercept=means, color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "", title = titles)
  plt + guides(fill=guide_legend(title=label_column)) + theme(legend.position=c(0.9,0.9))
}


a <-data.frame(Years=unlist(completeFUK), Country=rep('UK', 1000))
b <-data.frame(Years=unlist(completeFNL), Country=rep('NL', 1000))

many_distros <- do.call('rbind', list(a,b))

plot_multi_histogram(many_distros, 'Years', 'Country', c(mean(a$Years),mean(b$Years)),"Complete Cohort Life Expectancy")


## 7 b (ii)

annuityFUK <- rep(0,1000)
annuityFNL <- rep(0,1000)


for(j in 1:1000){
  px <- p65coh0862FUK[[j]]
  kpx <- c(1,cumprod(px))
  discount <- (1.03)^-(0:(length(kpx)-1))
  annuityFUK[j] <- 1000*sum(discount*kpx)
}

for(j in 1:1000){
  px <- p65coh0862FNL[[j]]
  kpx <- c(1,cumprod(px))
  discount <- (1.03)^-(0:(length(kpx)-1))
  annuityFNL[j] <- 1000*sum(discount*kpx)
}

a <-data.frame(Euros=unlist(annuityFUK), Country=rep('UK', 1000))
b <-data.frame(Euros=unlist(annuityFNL), Country=rep('NL', 1000))

many_distros <- do.call('rbind', list(a,b))

plot_multi_histogram(many_distros, 'Euros', 'Country', c(mean(a$Euros),mean(b$Euros)), "Annuity Value")


## 7b iii

insuranceFUK <- rep(0,1000)
insuranceFNL <- rep(0,1000)

for(j in 1:1000){
  px <- p65coh0862FUK[[j]]
  qx <- 1-p65coh0862FUK[[j]]
  kpx <- c(1,cumprod(px[-length(px)]))
  kqx <- kpx * qx
  discount <- (1.03)^-(1:(length(kqx)))
  insuranceFUK[j] <- 1000*sum(discount*kqx)
}

for(j in 1:1000){
  px <- p65coh0862FNL[[j]]
  qx <- 1-p65coh0862FNL[[j]]
  kpx <- c(1,cumprod(px[-length(px)]))
  kqx <- kpx * qx
  discount <- (1.03)^-(1:(length(kqx)))
  insuranceFNL[j] <- 1000*sum(discount*kqx)
}

a <-data.frame(Euros=unlist(insuranceFUK), Country=rep('UK', 1000))
b <-data.frame(Euros=unlist(insuranceFNL), Country=rep('NL', 1000))

many_distros <- do.call('rbind', list(a,b))

plot_multi_histogram(many_distros, 'Euros', 'Country', c(mean(a$Euros),mean(b$Euros)), "Death Benefit Value")

mean(insuranceFNL)
mean(insuranceFUK)
sd(insuranceFNL)
sd(insuranceFUK)


## a, b 3 methods
r1;r2;r3

c(thetaFUK,sigmaFUK);c(thetaFNL,sigmaFNL);c(thetaMUK,sigmaMUK);c(thetaMNL,sigmaMNL)
