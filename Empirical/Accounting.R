# Code File for computing the accouting exercises
# Data from: China Economic Census and Statistics of US Businesses
# Xin Tang @ Stony Brook University
# Last Updated: July 2014

# Clear memory
rm(list = ls())

# Load in packages
library(foreign)
library(data.table)
library(AER)
library(ggplot2)
library(scales)
library(grid)

# Load SUSB Data
USall <- read.csv("./Data/susb04.csv",header = TRUE)  # Statistics of US Businesses
USall <- as.data.table(USall)
USall <- USall[,list(NAICS, ENTRSIZE, FIRM, ESTB, EMPL, NAICSDSCR, ENTRSIZEDSCR)]
USleft <- c(1,5,10,15,20,25,30,35,40,45,50,75,100,150,200,300,400,500,750,1000,1500,2500)
USright <- c(4,9,14,19,24,29,34,39,44,49,74,99,149,199,299,399,499,749,999,1499,2499,10000)
NUS <- length(USleft)

# Load CNEC Data
# load("DLARGE_R.RData")
# DLARGE <- as.data.table(DLARGE)
# load("DSMALL_R.RData")
# DSMALL <- as.data.table(DSMALL)
# # Combine the LARGE and SMALL from CEC
# ljunk <- DLARGE[,list(industry,nbarworkers,industry_a,status,nbarworkers,product)]
# sjunk <- DSMALL[,list(industry,nbarworkers,industry_a,status,nbarworkers,product)]
# CHNprod <- rbind(ljunk,sjunk)
# # Release memory usage
# rm(DLARGE,DSMALL,ljunk,sjunk)
# save(CHNprod, file = "CHprod_all.RData")
load("./Data/CNEC_avgp.RData")
CHNprod <- CNEC_avgp
# Select only firms with positive employment, product and are operating normally
sel <- which(CHNprod$status == 1 & CHNprod$nbarworkers > 0 & CHNprod$product > 0 )
CHNprod <- CHNprod[sel]
CHleft <- rep(0,NUS)
CHright <- CHleft
# Load NGSPS
load("./Data/KEYFIRM_R.RData")
CHNpol <- KEYFIRM[,list(industry,industry_a,opr_hours,product,cod_e)]
sel <- which(CHNpol$opr_hours > 0 & CHNpol$product > 0 & CHNpol$cod_e > 0)
CHNpol <- CHNpol[sel]
# Check whether the two samples are comparable
# quinprod <- quantile(CHNprod$product, probs=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.99),na.rm=TRUE)
# quinpol <- quantile(CHNpol$product, probs=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.99),na.rm=TRUE)
# hist(log(CHNpol$product), probability=TRUE, xlim=c(0,15),ylim=c(0,0.3))
# lines(density(log(CHNpol$product)))
# hist(log(1.1*CHNprod$product), probability=TRUE, xlim=c(0,15),ylim=c(0,0.3))
# lines(density(log(CHNprod$product)))

# Overall cut-off ranges
quanup <- 0.75
quandown <- 0.25

#########################################################################################
#                             Paper Manufacturing Industry
#########################################################################################
########### Initial Preparations ########################
# Calculate the CNEC cut-off by production scale
sel <- which(CHNprod$industry == 2210 | CHNprod$industry == 2221 | CHNprod$industry == 2222)
# sel <- which(CHNprod$industry_a == 22)
CH <- CHNprod[sel]
# 2-digit sector price deflator, from Brandt etal 2012 JDE
deflator <- 96.30/93.50
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  CHleft[i] <-  quantile(CH$product[sel], probs=quandown, na.rm=TRUE)
  CHright[i] <- quantile(CH$product[sel], probs=quanup, na.rm=TRUE)
}
CHleft07 <- CHleft*deflator
CHright07 <- CHright*deflator

# Calculate the polluting intensity
sel <- which(CHNpol$industry_a == 22)
CHp <- CHNpol[sel]
CHp <- within(CHp,intensity <- cod_e/product)

# Calculate the US/China approximated production share
sel <- which(USall$NAICS == 3221)
US <- USall[sel]
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
US <- within(US,AVGF <- EMPL/FIRM)

distchn <- rep(0,NUS)
distus <- distchn

for (i in 1:(NUS-1)){
  sel <- which(US$AVGF > USleft[i] & US$AVGF <= USright[i])
  distus[i] <- sum(US$EMPL[sel])
#   sel1 <- which(CHNpol$product > CHleft07[i] & CHNpol$product <= CHright07[i])
  sel1 <- which(CHp$product > CHleft07[i] & CHp$product <= CHright07[i])
  distchn[i] <- sum(CHp$product[sel1])
}
# Last category
sel <- which(US$AVGF > USleft[NUS])
distus[NUS] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CHp$product > CHleft[NUS])
distchn[NUS] <- sum(CHp$product[sel1])
distchn <- distchn/sum(distchn)

selp <- rep(0,NUS)
############# Median Intensity ###################
med_int <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product >= CHleft07[i] & CHp$product <= CHright07[i])
  selp[i] <- length(sel)
  med_int[i] <- quantile(CHp$intensity[sel],probs= 0.5)
}
sel <- which(CHp$product >= CHleft07[NUS])
selp[NUS] <- length(sel)
med_int[NUS] <- quantile(CHp$intensity[sel],probs= 0.5)
pchn <- sum(med_int*distchn)
pus <- sum(med_int*distus)
pmed_paper <- pus/pchn

############# Piecewise Linear Estimation ###################
CHleftn <- USleft
CHrightn <- USright
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  i_lm <- lm(log(product) ~ log(nbarworkers), data=CH[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(nbarworkers)","Estimate"]
  CHleftn[i] <- exp(tmpa + tmpb*log(USleft[i]))
  CHrightn[i] <- exp(tmpa + tmpb*log(USright[i]))
}
CHleftn <- CHleftn*deflator
CHrightn <- CHrightn*deflator

# Calculate new distribution for Chinese firms
distchn1 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product > CHleftn[i] & CHp$product <= CHrightn[i])
  distchn1[i] <- sum(CHp$product[sel])
}
# Last category
sel1 <- which(CHp$product > CHleftn[NUS])
distchn1[NUS] <- sum(CHp$product[sel1])
distchn1 <- distchn1/sum(distchn1)

############# Regressing Intensity ###################
reg_int <- rep(0,NUS)
for (i in 1:NUS){
  sel <- which(CHp$product >= CHleftn[i] & CHp$product <= CHrightn[i])
  i_lm <- lm(log(intensity) ~ log(product),data = CHp[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(product)","Estimate"]
  reg_int[i] <- exp(tmpa + tmpb*(log(CHleftn[i])+log(CHrightn[i]))/2)
}
pchn1 <- sum(reg_int*distchn1)
pus1 <- sum(reg_int*distus)
preg_paper <- pus1/pchn1

############# Full Parametricc Analysis ###################
y_lm <- lm(log(product) ~ log(nbarworkers), data = CH)
p_lm <- lm(log(intensity) ~ log(product), data = CHp)
tmpa <- summary(y_lm)$coefficients["(Intercept)","Estimate"]
tmpb <- summary(y_lm)$coefficients["log(nbarworkers)","Estimate"]
tmpc <- summary(p_lm)$coefficients["(Intercept)","Estimate"]
tmpd <- summary(p_lm)$coefficients["log(product)","Estimate"]
mid_us <- (USleft+USright)/2
par_int <- exp(tmpc + tmpd*(tmpa + tmpb*log(mid_us)))

# Calculate new distribution for Chinese firms
distchn2 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CH$nbarworkers > USleft[i] & CH$nbarworkers <= USright[i])
  distchn2[i] <- sum(CH$nbarworkers[sel])
}
# Last category
sel1 <- which(CH$nbarworkers > USleft[NUS])
distchn2[NUS] <- sum(CH$nbarworkers[sel1])
distchn2 <- distchn2/sum(distchn2)
pus <- sum(par_int*distus)
pch <- sum(par_int*distchn2)
ppar_paper <- pus/pch

#########################################################################################
#                             Agricultural Food Industry
#########################################################################################
########### Initial Preparations ########################
# Calculate the CNEC cut-off by production scale
# sel <- which(CHNprod$industry == 2210 | CHNprod$industry == 2221 | CHNprod$industry == 2222)
sel <- which(CHNprod$industry_a == 13)
CH <- CHNprod[sel]
# 2-digit sector price deflator, from Brandt etal 2012 JDE
deflator <- 108.80/99.90
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  CHleft[i] <-  quantile(CH$product[sel], probs=quandown, na.rm=TRUE)
  CHright[i] <- quantile(CH$product[sel], probs=quanup, na.rm=TRUE)
}
CHleft07 <- CHleft*deflator
CHright07 <- CHright*deflator

# Calculate the polluting intensity
sel <- which(CHNpol$industry_a == 13)
CHp <- CHNpol[sel]
CHp <- within(CHp,intensity <- cod_e/product)

# Calculate the US/China approximated production share
sel <- which(USall$NAICS == 311)
US <- USall[sel]
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
US <- within(US,AVGF <- EMPL/FIRM)

distchn <- rep(0,NUS)
distus <- distchn

for (i in 1:(NUS-1)){
  sel <- which(US$AVGF > USleft[i] & US$AVGF <= USright[i])
  distus[i] <- sum(US$EMPL[sel])
  #   sel1 <- which(CHNpol$product > CHleft07[i] & CHNpol$product <= CHright07[i])
  sel1 <- which(CHp$product > CHleft07[i] & CHp$product <= CHright07[i])
  distchn[i] <- sum(CHp$product[sel1])
}
# Last category
sel <- which(US$AVGF > USleft[NUS])
distus[NUS] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CHp$product > CHleft[NUS])
distchn[NUS] <- sum(CHp$product[sel1])
distchn <- distchn/sum(distchn)

selp <- rep(0,NUS)
############# Median Intensity ###################
med_int <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product >= CHleft07[i] & CHp$product <= CHright07[i])
  selp[i] <- length(sel)
  med_int[i] <- quantile(CHp$intensity[sel],probs= 0.5)
}
sel <- which(CHp$product >= CHleft07[NUS])
selp[NUS] <- length(sel)
med_int[NUS] <- quantile(CHp$intensity[sel],probs= 0.5)
pchn <- sum(med_int*distchn)
pus <- sum(med_int*distus)
pmed_agri <- pus/pchn

############# Piecewise Linear Estimation ###################
CHleftn <- USleft
CHrightn <- USright
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  i_lm <- lm(log(product) ~ log(nbarworkers), data=CH[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(nbarworkers)","Estimate"]
  CHleftn[i] <- exp(tmpa + tmpb*log(USleft[i]))
  CHrightn[i] <- exp(tmpa + tmpb*log(USright[i]))
}
CHleftn <- CHleftn*deflator
CHrightn <- CHrightn*deflator

# Calculate new distribution for Chinese firms
distchn1 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product > CHleftn[i] & CHp$product <= CHrightn[i])
  distchn1[i] <- sum(CHp$product[sel])
}
# Last category
sel1 <- which(CHp$product > CHleftn[NUS])
distchn1[NUS] <- sum(CHp$product[sel1])
distchn1 <- distchn1/sum(distchn1)

############# Regressing Intensity ###################
reg_int <- rep(0,NUS)
for (i in 1:NUS){
  sel <- which(CHp$product >= CHleftn[i] & CHp$product <= CHrightn[i])
  i_lm <- lm(log(intensity) ~ log(product),data = CHp[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(product)","Estimate"]
  reg_int[i] <- exp(tmpa + tmpb*(log(CHleftn[i])+log(CHrightn[i]))/2)
}
pchn1 <- sum(reg_int*distchn1)
pus1 <- sum(reg_int*distus)
preg_agri <- pus1/pchn1

############# Full Parametricc Analysis ###################
y_lm <- lm(log(product) ~ log(nbarworkers), data = CH)
p_lm <- lm(log(intensity) ~ log(product), data = CHp)
tmpa <- summary(y_lm)$coefficients["(Intercept)","Estimate"]
tmpb <- summary(y_lm)$coefficients["log(nbarworkers)","Estimate"]
tmpc <- summary(p_lm)$coefficients["(Intercept)","Estimate"]
tmpd <- summary(p_lm)$coefficients["log(product)","Estimate"]
mid_us <- (USleft+USright)/2
par_int <- exp(tmpc + tmpd*(tmpa + tmpb*log(mid_us)))

# Calculate new distribution for Chinese firms
distchn2 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CH$nbarworkers > USleft[i] & CH$nbarworkers <= USright[i])
  distchn2[i] <- sum(CH$nbarworkers[sel])
}
# Last category
sel1 <- which(CH$nbarworkers > USleft[NUS])
distchn2[NUS] <- sum(CH$nbarworkers[sel1])
distchn2 <- distchn2/sum(distchn2)
pus <- sum(par_int*distus)
pch <- sum(par_int*distchn2)
ppar_agri <- pus/pch

#########################################################################################
#                             Textile Industry
#########################################################################################
########### Initial Preparations ########################
# Calculate the CNEC cut-off by production scale
# sel <- which(CHNprod$industry == 2210 | CHNprod$industry == 2221 | CHNprod$industry == 2222)
sel <- which(CHNprod$industry_a == 17)
CH <- CHNprod[sel]
# 2-digit sector price deflator, from Brandt etal 2012 JDE
deflator <- 103.80/100.60
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  CHleft[i] <-  quantile(CH$product[sel], probs=quandown, na.rm=TRUE)
  CHright[i] <- quantile(CH$product[sel], probs=quanup, na.rm=TRUE)
}
CHleft07 <- CHleft*deflator
CHright07 <- CHright*deflator

# Calculate the polluting intensity
sel <- which(CHNpol$industry_a == 17)
CHp <- CHNpol[sel]
CHp <- within(CHp,intensity <- cod_e/product)

# Calculate the US/China approximated production share
sel <- which(USall$NAICS == 313)
US <- USall[sel]
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
US <- within(US,AVGF <- EMPL/FIRM)

distchn <- rep(0,NUS)
distus <- distchn

for (i in 1:(NUS-1)){
  sel <- which(US$AVGF > USleft[i] & US$AVGF <= USright[i])
  distus[i] <- sum(US$EMPL[sel])
  #   sel1 <- which(CHNpol$product > CHleft07[i] & CHNpol$product <= CHright07[i])
  sel1 <- which(CHp$product > CHleft07[i] & CHp$product <= CHright07[i])
  distchn[i] <- sum(CHp$product[sel1])
}
# Last category
sel <- which(US$AVGF > USleft[NUS])
distus[NUS] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CHp$product > CHleft[NUS])
distchn[NUS] <- sum(CHp$product[sel1])
distchn <- distchn/sum(distchn)

selp <- rep(0,NUS)
############# Median Intensity ###################
med_int <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product >= CHleft07[i] & CHp$product <= CHright07[i])
  selp[i] <- length(sel)
  med_int[i] <- quantile(CHp$intensity[sel],probs= 0.5)
}
sel <- which(CHp$product >= CHleft07[NUS])
selp[NUS] <- length(sel)
med_int[NUS] <- quantile(CHp$intensity[sel],probs= 0.5)
pchn <- sum(med_int*distchn)
pus <- sum(med_int*distus)
pmed_text <- pus/pchn

############# Piecewise Linear Estimation ###################
CHleftn <- USleft
CHrightn <- USright
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  i_lm <- lm(log(product) ~ log(nbarworkers), data=CH[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(nbarworkers)","Estimate"]
  CHleftn[i] <- exp(tmpa + tmpb*log(USleft[i]))
  CHrightn[i] <- exp(tmpa + tmpb*log(USright[i]))
}
CHleftn <- CHleftn*deflator
CHrightn <- CHrightn*deflator

# Calculate new distribution for Chinese firms
distchn1 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product > CHleftn[i] & CHp$product <= CHrightn[i])
  distchn1[i] <- sum(CHp$product[sel])
}
# Last category
sel1 <- which(CHp$product > CHleftn[NUS])
distchn1[NUS] <- sum(CHp$product[sel1])
distchn1 <- distchn1/sum(distchn1)

############# Regressing Intensity ###################
reg_int <- rep(0,NUS)
for (i in 1:NUS){
  sel <- which(CHp$product >= CHleftn[i] & CHp$product <= CHrightn[i])
  i_lm <- lm(log(intensity) ~ log(product),data = CHp[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(product)","Estimate"]
  reg_int[i] <- exp(tmpa + tmpb*(log(CHleftn[i])+log(CHrightn[i]))/2)
}
pchn1 <- sum(reg_int*distchn1)
pus1 <- sum(reg_int*distus)
preg_text <- pus1/pchn1

############# Full Parametricc Analysis ###################
y_lm <- lm(log(product) ~ log(nbarworkers), data = CH)
p_lm <- lm(log(intensity) ~ log(product), data = CHp)
tmpa <- summary(y_lm)$coefficients["(Intercept)","Estimate"]
tmpb <- summary(y_lm)$coefficients["log(nbarworkers)","Estimate"]
tmpc <- summary(p_lm)$coefficients["(Intercept)","Estimate"]
tmpd <- summary(p_lm)$coefficients["log(product)","Estimate"]
mid_us <- (USleft+USright)/2
par_int <- exp(tmpc + tmpd*(tmpa + tmpb*log(mid_us)))

# Calculate new distribution for Chinese firms
distchn2 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CH$nbarworkers > USleft[i] & CH$nbarworkers <= USright[i])
  distchn2[i] <- sum(CH$nbarworkers[sel])
}
# Last category
sel1 <- which(CH$nbarworkers > USleft[NUS])
distchn2[NUS] <- sum(CH$nbarworkers[sel1])
distchn2 <- distchn2/sum(distchn2)
pus <- sum(par_int*distus)
pch <- sum(par_int*distchn2)
ppar_text <- pus/pch

#########################################################################################
#                             ChemMat Industry
#########################################################################################
########### Initial Preparations ########################
# Calculate the CNEC cut-off by production scale
# sel <- which(CHNprod$industry == 2210 | CHNprod$industry == 2221 | CHNprod$industry == 2222)
sel <- which(CHNprod$industry_a == 26)
CH <- CHNprod[sel]
# 2-digit sector price deflator, from Brandt etal 2012 JDE
deflator <- 108.80/99.90
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  CHleft[i] <-  quantile(CH$product[sel], probs=quandown, na.rm=TRUE)
  CHright[i] <- quantile(CH$product[sel], probs=quanup, na.rm=TRUE)
}
CHleft07 <- CHleft*deflator
CHright07 <- CHright*deflator

# Calculate the polluting intensity
sel <- which(CHNpol$industry_a == 26)
CHp <- CHNpol[sel]
CHp <- within(CHp,intensity <- cod_e/product)

# Calculate the US/China approximated production share
sel <- which(USall$NAICS == 3251 | USall$NAICS == 3252 | USall$NAICS == 3253 | USall$NAICS == 3255 | USall$NAICS == 3256 | USall$NAICS == 3259)
US <- USall[sel]
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
US <- US[, list(FIRM=sum(FIRM, na.rm = TRUE), ESTB=sum(ESTB, na.rm = TRUE), EMPL=sum(EMPL, na.rm = TRUE)), by=list(ENTRSIZE)]
US <- within(US,AVGF <- EMPL/FIRM)

distchn <- rep(0,NUS)
distus <- distchn

for (i in 1:(NUS-1)){
  sel <- which(US$AVGF > USleft[i] & US$AVGF <= USright[i])
  distus[i] <- sum(US$EMPL[sel])
  #   sel1 <- which(CHNpol$product > CHleft07[i] & CHNpol$product <= CHright07[i])
  sel1 <- which(CHp$product > CHleft07[i] & CHp$product <= CHright07[i])
  distchn[i] <- sum(CHp$product[sel1])
}
# Last category
sel <- which(US$AVGF > USleft[NUS])
distus[NUS] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CHp$product > CHleft[NUS])
distchn[NUS] <- sum(CHp$product[sel1])
distchn <- distchn/sum(distchn)

selp <- rep(0,NUS)
############# Median Intensity ###################
med_int <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product >= CHleft07[i] & CHp$product <= CHright07[i])
  selp[i] <- length(sel)
  med_int[i] <- quantile(CHp$intensity[sel],probs= 0.5)
}
sel <- which(CHp$product >= CHleft07[NUS])
selp[NUS] <- length(sel)
med_int[NUS] <- quantile(CHp$intensity[sel],probs= 0.5)
pchn <- sum(med_int*distchn)
pus <- sum(med_int*distus)
pmed_chem <- pus/pchn

############# Piecewise Linear Estimation ###################
CHleftn <- USleft
CHrightn <- USright
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  i_lm <- lm(log(product) ~ log(nbarworkers), data=CH[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(nbarworkers)","Estimate"]
  CHleftn[i] <- exp(tmpa + tmpb*log(USleft[i]))
  CHrightn[i] <- exp(tmpa + tmpb*log(USright[i]))
}
CHleftn <- CHleftn*deflator
CHrightn <- CHrightn*deflator

# Calculate new distribution for Chinese firms
distchn1 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product > CHleftn[i] & CHp$product <= CHrightn[i])
  distchn1[i] <- sum(CHp$product[sel])
}
# Last category
sel1 <- which(CHp$product > CHleftn[NUS])
distchn1[NUS] <- sum(CHp$product[sel1])
distchn1 <- distchn1/sum(distchn1)

############# Regressing Intensity ###################
reg_int <- rep(0,NUS)
for (i in 1:NUS){
  sel <- which(CHp$product >= CHleftn[i] & CHp$product <= CHrightn[i])
  i_lm <- lm(log(intensity) ~ log(product),data = CHp[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(product)","Estimate"]
  reg_int[i] <- exp(tmpa + tmpb*(log(CHleftn[i])+log(CHrightn[i]))/2)
}
pchn1 <- sum(reg_int*distchn1)
pus1 <- sum(reg_int*distus)
preg_chem <- pus1/pchn1

############# Full Parametricc Analysis ###################
y_lm <- lm(log(product) ~ log(nbarworkers), data = CH)
p_lm <- lm(log(intensity) ~ log(product), data = CHp)
tmpa <- summary(y_lm)$coefficients["(Intercept)","Estimate"]
tmpb <- summary(y_lm)$coefficients["log(nbarworkers)","Estimate"]
tmpc <- summary(p_lm)$coefficients["(Intercept)","Estimate"]
tmpd <- summary(p_lm)$coefficients["log(product)","Estimate"]
mid_us <- (USleft+USright)/2
par_int <- exp(tmpc + tmpd*(tmpa + tmpb*log(mid_us)))

# Calculate new distribution for Chinese firms
distchn2 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CH$nbarworkers > USleft[i] & CH$nbarworkers <= USright[i])
  distchn2[i] <- sum(CH$nbarworkers[sel])
}
# Last category
sel1 <- which(CH$nbarworkers > USleft[NUS])
distchn2[NUS] <- sum(CH$nbarworkers[sel1])
distchn2 <- distchn2/sum(distchn2)
pus <- sum(par_int*distus)
pch <- sum(par_int*distchn2)
ppar_chem <- pus/pch

#########################################################################################
#                             Beverage Industry
#########################################################################################
########### Initial Preparations ########################
# Calculate the CNEC cut-off by production scale
# sel <- which(CHNprod$industry == 2210 | CHNprod$industry == 2221 | CHNprod$industry == 2222)
sel <- which(CHNprod$industry_a == 15)
CH <- CHNprod[sel]
# 2-digit sector price deflator, from Brandt etal 2012 JDE
deflator <- 103.80/100.60
for (i in 1:NUS){
  sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
  CHleft[i] <-  quantile(CH$product[sel], probs=quandown, na.rm=TRUE)
  CHright[i] <- quantile(CH$product[sel], probs=quanup, na.rm=TRUE)
}
CHleft07 <- CHleft*deflator
CHright07 <- CHright*deflator

# Calculate the polluting intensity
sel <- which(CHNpol$industry_a == 15)
CHp <- CHNpol[sel]
CHp <- within(CHp,intensity <- cod_e/product)

# Calculate the US/China approximated production share
sel <- which(USall$NAICS == 3121)
US <- USall[sel]
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
US <- within(US,AVGF <- EMPL/FIRM)

distchn <- rep(0,NUS)
distus <- distchn

for (i in 1:(NUS-1)){
  sel <- which(US$AVGF > USleft[i] & US$AVGF <= USright[i])
  distus[i] <- sum(US$EMPL[sel])
  #   sel1 <- which(CHNpol$product > CHleft07[i] & CHNpol$product <= CHright07[i])
  sel1 <- which(CHp$product > CHleft07[i] & CHp$product <= CHright07[i])
  distchn[i] <- sum(CHp$product[sel1])
}
# Last category
sel <- which(US$AVGF > USleft[NUS])
distus[NUS] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CHp$product > CHleft[NUS])
distchn[NUS] <- sum(CHp$product[sel1])
distchn <- distchn/sum(distchn)

selp <- rep(0,NUS)
############# Median Intensity ###################
med_int <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CHp$product >= CHleft07[i] & CHp$product <= CHright07[i])
  selp[i] <- length(sel)
  med_int[i] <- quantile(CHp$intensity[sel],probs= 0.5)
}
sel <- which(CHp$product >= CHleft07[NUS])
selp[NUS] <- length(sel)
med_int[NUS] <- quantile(CHp$intensity[sel],probs= 0.5)
pchn <- sum(med_int*distchn)
pus <- sum(med_int*distus)
pmed_bever <- pus/pchn

# ############# Piecewise Linear Estimation ###################
# CHleftn <- USleft
# CHrightn <- USright
# for (i in 1:NUS){
#   sel <- which(CH$nbarworkers >= USleft[i] & CH$nbarworkers <= USright[i])
#   i_lm <- lm(log(product) ~ log(nbarworkers), data=CH[sel])
#   tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
#   tmpb <- summary(i_lm)$coefficients["log(nbarworkers)","Estimate"]
#   CHleftn[i] <- exp(tmpa + tmpb*log(USleft[i]))
#   CHrightn[i] <- exp(tmpa + tmpb*log(USright[i]))
# }
# CHleftn <- CHleftn*deflator
# CHrightn <- CHrightn*deflator
# 
# # Calculate new distribution for Chinese firms
# distchn1 <- rep(0,NUS)
# for (i in 1:(NUS-1)){
#   sel <- which(CHp$product > CHleftn[i] & CHp$product <= CHrightn[i])
#   distchn1[i] <- sum(CHp$product[sel])
# }
# # Last category
# sel1 <- which(CHp$product > CHleftn[NUS])
# distchn1[NUS] <- sum(CHp$product[sel1])
# distchn1 <- distchn1/sum(distchn1)

############# Regressing Intensity ###################
reg_int <- rep(0,NUS)
for (i in 1:NUS){
  sel <- which(CHp$product >= CHleftn[i] & CHp$product <= CHrightn[i])
  i_lm <- lm(log(intensity) ~ log(product),data = CHp[sel])
  tmpa <- summary(i_lm)$coefficients["(Intercept)","Estimate"]
  tmpb <- summary(i_lm)$coefficients["log(product)","Estimate"]
  reg_int[i] <- exp(tmpa + tmpb*(log(CHleftn[i])+log(CHrightn[i]))/2)
}
pchn1 <- sum(reg_int*distchn1)
pus1 <- sum(reg_int*distus)
preg_bever <- pus1/pchn1

############# Full Parametricc Analysis ###################
y_lm <- lm(log(product) ~ log(nbarworkers), data = CH)
p_lm <- lm(log(intensity) ~ log(product), data = CHp)
tmpa <- summary(y_lm)$coefficients["(Intercept)","Estimate"]
tmpb <- summary(y_lm)$coefficients["log(nbarworkers)","Estimate"]
tmpc <- summary(p_lm)$coefficients["(Intercept)","Estimate"]
tmpd <- summary(p_lm)$coefficients["log(product)","Estimate"]
mid_us <- (USleft+USright)/2
par_int <- exp(tmpc + tmpd*(tmpa + tmpb*log(mid_us)))

# Calculate new distribution for Chinese firms
distchn2 <- rep(0,NUS)
for (i in 1:(NUS-1)){
  sel <- which(CH$nbarworkers > USleft[i] & CH$nbarworkers <= USright[i])
  distchn2[i] <- sum(CH$nbarworkers[sel])
}
# Last category
sel1 <- which(CH$nbarworkers > USleft[NUS])
distchn2[NUS] <- sum(CH$nbarworkers[sel1])
distchn2 <- distchn2/sum(distchn2)
pus <- sum(par_int*distus)
pch <- sum(par_int*distchn2)
ppar_bever <- pus/pch

pmed <- cat(pmed_paper,pmed_agri,pmed_text,pmed_chem,pmed_bever)
preg <- cat(preg_paper,preg_agri,preg_text,preg_chem,-1)
ppar <- cat(ppar_paper,ppar_agri,ppar_text,ppar_chem,ppar_bever)
