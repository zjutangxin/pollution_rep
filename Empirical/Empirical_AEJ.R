# --------------------------------------------------------------------------
#                       Program Description
# --------------------------------------------------------------------------
#    
# Purpose:
#     - Produce most of the empirical results in the main text.
#     - 1. Table 2
#     - 2. Figures: 1, 2, 3
#     - 3. Regressions: 1, 2, 3, Section I.B
#     - 4. Calibration Targets: Size distribution, phi1, clean share
#             ke/Y
#     - The Size Distribution of Firms and Industrial Water Pollution: A
#       Quantitative Analysis of China
#     - Prepared for AEJ: Macro
# 
# Author:
#     - Xin Tang @ International Monetary Fund
# 
# Record of Revisions:
#          Date:                 Description of Changes
#    ============        =================================
#       04/06/2019                 Original Version
#       09/16/2019                Improved Annotation
# ==========================================================================
# Clear memory
rm(list = ls())

# Load packages
library(foreign)
library(data.table)
library(AER)
library(scales)
library(grid)

# ==========================================================================
#                   1. Intensity and Firm Size
# ==========================================================================
load("./Data/KEYFIRM_R.RData")
# -------------------- Data Processing --------------------------
# Aggregate ownership rights type
# 0: missing, 1: State/collective, 3: private, 4: HMT, 5: foreign
KEYFIRM$type_a <- 0
# State and collective
sel <- which(KEYFIRM$type == 110 | KEYFIRM$type == 141
    | KEYFIRM$type == 151 | KEYFIRM$type == 120 | KEYFIRM$type == 130
    | KEYFIRM$type == 140 | KEYFIRM$type == 150 | KEYFIRM$type == 142
    | KEYFIRM$type == 143 | KEYFIRM$type == 149 | KEYFIRM$type == 159
    | KEYFIRM$type == 160 | KEYFIRM$type == 100 )
KEYFIRM$type_a[sel] <- 1
# Private
sel <- which(KEYFIRM$type == 170 | KEYFIRM$type == 171
    | KEYFIRM$type == 172 | KEYFIRM$type == 173
    | KEYFIRM$type == 174 | KEYFIRM$type == 190)
KEYFIRM$type_a[sel] <- 3
# Hong Kong, Macau and Taiwan
sel <- which(KEYFIRM$type == 200 | KEYFIRM$type == 210
    | KEYFIRM$type == 220 | KEYFIRM$type == 230 | KEYFIRM$type == 240)
KEYFIRM$type_a[sel] <- 4
# Foreign
sel <- which(KEYFIRM$type == 300 | KEYFIRM$type == 310
    | KEYFIRM$type == 320 | KEYFIRM$type == 330 | KEYFIRM$type == 340)
KEYFIRM$type_a[sel] <- 5

# Aggregate treatment technology type
KEYFIRM$dm1_code_a <- 0
# Physical
sel <- which(KEYFIRM$dm1_code >= 1000 & KEYFIRM$dm1_code < 2000)
KEYFIRM$dm1_code_a[sel] <- 1
# Chemical
sel <- which(KEYFIRM$dm1_code >= 2000 & KEYFIRM$dm1_code < 3000)
KEYFIRM$dm1_code_a[sel] <- 2
# Physiochemical
sel <- which(KEYFIRM$dm1_code >= 3000 & KEYFIRM$dm1_code < 4000)
KEYFIRM$dm1_code_a[sel] <- 3
# Biological
sel <- which(KEYFIRM$dm1_code >= 4000 & KEYFIRM$dm1_code < 5000)
KEYFIRM$dm1_code_a[sel] <- 4
# Combination
sel <- which(KEYFIRM$dm1_code >= 5000 & KEYFIRM$dm1_code < 6000)
KEYFIRM$dm1_code_a[sel] <- 5
# dm1_code_a == 0 means the equipment is unclassified

KEYFIRM$province <- factor(KEYFIRM$province)        
KEYFIRM$industry_a <- factor(KEYFIRM$industry_a)    
KEYFIRM$Census_Type <- factor(KEYFIRM$Census_Type)  
KEYFIRM$dm1_code_a <- factor(KEYFIRM$dm1_code_a)    
KEYFIRM$type_a <- factor(KEYFIRM$type_a)            

POL5 <- KEYFIRM[industry_a == 22 | industry_a == 13 | industry_a == 15 
    | industry_a == 17 | industry_a == 26]
POL5 <- POL5[product > 0 & cod_e > 0 & type_a != 0]
POL5$intensity <- with(POL5, intensity <- cod_e/product)

# ===================== Regression 1 =======================================
lm_pol5_all <- lm(log(cod_e) ~ log(product) + province + type_a 
                  + industry_a, data = POL5)
summary(lm_pol5_all)

# ===================== Figure 1 ===========================================
# Residual intensity versus production
lm_pol5_aux1 <- lm(log(intensity) ~ 
      province + type_a + industry_a, data = POL5)
lm_pol5_aux2 <- lm(log(product) ~ 
      province + type_a + industry_a, data = POL5)
POL5$res_intensity <- residuals(lm_pol5_aux1)
POL5$res_product <- residuals(lm_pol5_aux2)

pdf("./Results/Figure1.pdf",height=5,width=5)
plot((POL5$res_intensity)~(POL5$res_product),
     cex=0.5,mgp=c(1.75, 0.75, 0), 
     xlab="Log Production",ylab="Log Intensity",
     main="Pooled Polluting",cex.main=1.75,cex.lab=1.5)
pol5_residual <- lm(res_intensity ~ res_product, data = POL5)
abline(pol5_residual,col="red",lwd=4)
dev.off()

# ==========================================================================
#                   2. Firm Size and Technology
# ==========================================================================
# ===================== Clean Share ========================================
# Used in calibration
dmtb <- table(POL5$dm1_code_a)
clean_share <- (sum(dmtb[5:6]))/sum(dmtb)

# ===================== Table 2 ============================================
# Table 2 Column 2
PAPER <- KEYFIRM[industry_a == 22]
dmtb <- table(PAPER$dm1_code_a)
phyrate <- dmtb[2]/sum(dmtb[2:6])
chemrate <- dmtb[3]/sum(dmtb[2:6])
biorate <- sum(dmtb[5:6])/sum(dmtb[2:6])
phyrate
chemrate
biorate

# Table 2 Column 1
PAPER <- within(PAPER,{
    cod_eg <- cod_e/cod_g
    dm1_unit <- dm1_quant/dm1_inv
    dm1_prod <- dm1_inv/product
    dm1_prod2 <- (dm1_inv + dm1_oprcost)/product
    dm1_prod3 <- dm1_oprcost/product
    int <- cod_e/product
})
phyeffc <- 1 -
  mean(PAPER$cod_eg[PAPER$dm1_code_a == 1 & PAPER$cod_eg <= 1],na.rm=TRUE)
chemeffc <- 1 - 
  mean(PAPER$cod_eg[PAPER$dm1_code_a == 2 & PAPER$cod_eg <= 1],na.rm = TRUE)
bioeffc <- 1 - 
  mean(PAPER$cod_eg[(PAPER$dm1_code_a == 4 | PAPER$dm1_code_a == 5) 
                    & PAPER$cod_eg <= 1],na.rm=TRUE)
phyeffc
chemeffc
bioeffc

# Table 2 Column 3
median(PAPER$dm1_inv[PAPER$dm1_code_a == 1 & PAPER$cod_eg <= 1],na.rm=TRUE)
median(PAPER$dm1_inv[PAPER$dm1_code_a == 2 & PAPER$cod_eg <= 1],na.rm=TRUE)
median(PAPER$dm1_inv[PAPER$dm1_code_a == 4 | PAPER$dm1_code_a == 5 
                     & PAPER$cod_eg <= 1],na.rm=TRUE)

# Table 2 Column 4
median(PAPER$product[PAPER$dm1_code_a == 1 & PAPER$cod_eg <= 1],na.rm=TRUE)
median(PAPER$product[PAPER$dm1_code_a == 2 & PAPER$cod_eg <= 1],na.rm=TRUE)
median(PAPER$product[PAPER$dm1_code_a == 4 | PAPER$dm1_code_a == 5 
                     & PAPER$cod_eg <= 1],na.rm=TRUE)

# =========== Unumbered Regression in Section I.B ==========================
# Linear Probability Model of Technology Adoption
POL5$clean <- 0
sel <- which(POL5$dm1_code_a == 4 | POL5$dm1_code_a == 5)
POL5$clean[sel] <- 1

lm_clean <- lm(clean ~ log(product) + industry_a 
      + province + type_a, data = POL5)
summary(lm_clean)

# ======================= Regressions 2 and 3 ==============================
# Regression 2
lm1 <- lm(log(cod_e) ~ log(product) + province + industry_a + type_a, 
        data = POL5[(dm1_code_a == 4 | dm1_code_a == 5) & intensity > 0])
summary(lm1)
# Regression 3
lm_pool <- lm(log(cod_e) ~ log(product) + province 
    + industry_a + type_a + dm1_code_a, 
    data = POL5[dm1_code_a == 2 | dm1_code_a == 1 & intensity > 0])
summary(lm_pool)

# =========== Unumbered 3 Regressions in Appendix C.1 ======================
# The first one
lm_phy <- lm(log(cod_e) ~ log(product) + province 
             + industry_a + type_a, 
             data = POL5[dm1_code_a == 1 & intensity > 0])
summary(lm_phy)
# The second one
lm_chem <- lm(log(cod_e) ~ log(product) + province 
             + industry_a + type_a, 
             data = POL5[dm1_code_a == 2 & intensity > 0])
summary(lm_chem)

# ============ Fixed Cost/Output Ratio of Clean Firms ======================
# Used in calibration
sel <- which(POL5$dm1_code_a == 4 | POL5$dm1_code_a == 5)
sum(POL5$dm1_inv[sel], na.rm = TRUE)/sum(POL5$product[sel], na.rm = TRUE)

# ==========================================================================
#                   3. Firm Size Distribution
# ==========================================================================
# Clear memory
rm(list = ls())

load("./Data/CNEC_avgp.RData")
CHNall <- CNEC_avgp
rm(CNEC_avgp)
USall <- read.csv("./Data/susb04.csv",header = TRUE)  
USall <- as.data.table(USall)
USall <- USall[,list(NAICS, ENTRSIZE, FIRM, ESTB, 
        EMPL, NAICSDSCR, ENTRSIZEDSCR)]
qup <- quantile(CHNall$nbarworkers, probs=c(.995),na.rm= TRUE)
qdown <- quantile(CHNall$nbarworkers, probs=c(.01),na.rm= TRUE)
sel <- which(CHNall$nbarworkers > 0 & CHNall$nbarworkers < qup
             & CHNall$nbarworkers > qdown)
CHNall <- CHNall[sel]

# --------------------------------------------------------------------------
#                   Pooled Polluting
# --------------------------------------------------------------------------
sel <- which(CHNall$industry_a == 22 | CHNall$industry_a == 13
      | CHNall$industry_a == 15 | CHNall$industry_a == 17
      | CHNall$industry_a == 26)
CH <- CHNall[sel]

sel <- which(USall$NAICS == 3221 | USall$NAICS == 311 
    | USall$NAICS == 313 | USall$NAICS == 3251 
    | USall$NAICS == 3252 | USall$NAICS == 3253 
    | USall$NAICS == 3255 | USall$NAICS == 3256 
    | USall$NAICS == 3259 | USall$NAICS == 3121)
US <- USall[sel]

# Process the U.S. Data
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
USSUM <- US[, list(FIRM=sum(FIRM, na.rm = TRUE), 
        ESTB=sum(ESTB, na.rm = TRUE), EMPL=sum(EMPL, na.rm = TRUE)), 
        by=list(ENTRSIZE)]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,149,199,299,
        399,499,749,999,1499,2499,5000)
USSUM <- within(USSUM,
                {AVGF <- EMPL/FIRM   # Average firm size
                AVGE <- EMPL/ESTB   # Average plant size
                CF <- cf
                })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
          & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last group
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# =================== Figure 2 =============================================
pdf("./Results/Figure2_Left.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
    ylim=c(0,1.0),xlab="Firm Size",main="Pooled Polluting", 
    cex.main = 2.50, cex.lab = 1.75,
    names.arg=c("1-19","20-99","100-399","400+"), 
    cex.names=1.75)
title(ylab = "Employment Share", line = 2.3, cex.lab = 1.5)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",cex=1.5)
dev.off()

# Size Distribution by groups used in computation
cutoff <- c(1,19,49,99,399)
n1 <- length(cutoff)
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
                & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)
# Employment distribution for polluting industries
# NOT used in calibration
distchn

# --------------------------------------------------------------------------
#                   All Manufacturing
# --------------------------------------------------------------------------
CH <- CHNall
sel <- which(USall$NAICS == "31-33")
US <- USall[sel]

# Process the U.S. Data
sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
USSUM <- US[, list(FIRM=sum(FIRM, na.rm = TRUE), 
    ESTB=sum(ESTB, na.rm = TRUE), EMPL=sum(EMPL, na.rm = TRUE)), 
    by=list(ENTRSIZE)]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,149,199,299,
        399,499,749,999,1499,2499,5000)
USSUM <- within(USSUM,
                {AVGF <- EMPL/FIRM   # Average firm size
                AVGE <- EMPL/ESTB   # Average plant size
                CF <- cf
                })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
          & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# ==================== Figure 2 ============================================
pdf("./Results/Figure2_Right.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
    ylim=c(0,1.0),xlab="Firm Size",main="All Manufacturing", 
    cex.main = 2.50, cex.lab = 1.75,
    names.arg=c("1-19","20-99","100-399","400+"), 
    cex.names=1.75)
title(ylab = "Employment Share", line = 2.3, cex.lab = 1.5)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",cex=1.5)
dev.off()

# ----------------------------------------------------------------
# Compute the calibration target of employment distribution
# ----------------------------------------------------------------
cutoff <- c(1,19,49,99,399)
n1 <- length(cutoff)
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
          & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)
# =================== Calibration Target ===================================
# Employment share used in quantitative part
distchn

# ----------------------------------------------------------------
# Compute the calibration target of firm size distribution
# ----------------------------------------------------------------
cutoff <- c(1,19,49,99,399)
n1 <- length(cutoff)
distchn <- rep(0,n1)
# distus <- distchn

for (i in 2:n1){
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
          & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- length(sel1)
}
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- length(sel1)
distchn <- distchn/sum(distchn)
# =================== Calibration Target ===================================
# Firm size distribution used in quantitative part
distchn

# ==========================================================================
#                       4. Distortions
# ==========================================================================
# ----------------------------------------------------------------
# Compute phi1 following Equation (8)
# ----------------------------------------------------------------
rm(list = ls())

load("./Data/CNEC_avgp.RData")
CNEC <- CNEC_avgp
rm(CNEC_avgp)

# Drop irregular samples
CNEC <- CNEC[status == 1]
CNEC <- CNEC[product > 0]
CNEC <- CNEC[totcapital > 0]
CNEC <- CNEC[nbarworkers > 0]
CNEC <- CNEC[wage + nonwage > 0]

# Construct categorical variables
CNEC$type_a <- factor(CNEC$type_a)
CNEC$province <- factor(CNEC$province)
CNEC$industry <- factor(CNEC$industry)
CNEC$industry_a <- factor(CNEC$industry_a)

# Calculate new variables
CNEC <- within(CNEC,
      {lcomp <- wage + nonwage
      age <- 2005 - founding_y
      })
# Calculate average factor products
alpha = 0.5376
gamma = 0.93

CNEC <- within(CNEC,
      {phik   <- product/totcapital
       phil   <- product/lcomp
       phil_1 <- product/nbarworkers
       phi    <- (phik^alpha)*(phil^(1-alpha))
       kappa  <- totcapital/lcomp
       z      <- (product/(totcapital^alpha*
            lcomp^(1-alpha))^gamma)^(1/(1-gamma))
               })

philz <- CNEC
# Five Polluting Industries
sel <- which(philz$industry_a == 22 | philz$industry_a == 13 
      | philz$industry_a == 15 | philz$industry_a == 17 
      | philz$industry_a == 26)
philz_pol <- philz[sel]

cutup = 0.90
cutdown = 0.10
phiup <- quantile(philz_pol$phil, probs=c(cutup), na.rm = TRUE)
phidown <- quantile(philz_pol$phil, probs=c(cutdown), na.rm = TRUE)
zup <- quantile(philz_pol$z, probs=c(cutup), na.rm = TRUE)
zdown <- quantile(philz_pol$z, probs=c(cutdown), na.rm = TRUE)
sel <- which(philz_pol$z > zup)
zupnew <- mean(philz_pol$z[sel])
sel <- which(philz_pol$z < zdown)
zdownnew <- mean(philz_pol$z[sel])
sel <- which(philz_pol$phil > phiup)
phiupnew <- mean(philz_pol$phil[sel])
sel <- which(philz_pol$phil < phidown)
phidownnew <- mean(philz_pol$phil[sel])
phi_quant <- (log(phidownnew/phiupnew))/(log(zupnew/zdownnew))
# Five polluting industries only
phi_quant

# All Manufacturing Industries
cutup = 0.90
cutdown = 0.10
phiup <- quantile(CNEC$phil, probs=c(cutup), na.rm = TRUE)
phidown <- quantile(CNEC$phil, probs=c(cutdown), na.rm = TRUE)
zup <- quantile(CNEC$z, probs=c(cutup), na.rm = TRUE)
zdown <- quantile(CNEC$z, probs=c(cutdown), na.rm = TRUE)
sel <- which(CNEC$z > zup)
zupnew <- mean(CNEC$z[sel])
sel <- which(CNEC$z < zdown)
zdownnew <- mean(CNEC$z[sel])
sel <- which(CNEC$phil > phiup)
phiupnew <- mean(CNEC$phil[sel])
sel <- which(CNEC$phil < phidown)
phidownnew <- mean(CNEC$phil[sel])
phi_quant <- (log(phidownnew/phiupnew))/(log(zupnew/zdownnew))

# =================== phi1 in calibration ==================================
# All manufacturing
phi_quant

# ----------------------------------------------------------------
# Plot Figure 3
# ----------------------------------------------------------------
rm(list = ls())
load("./Data/CNEC_avgp.RData")
CNEC <- CNEC_avgp
rm(CNEC_avgp)

# Drop irregular samples
CNEC <- CNEC[status == 1]
CNEC <- CNEC[product > 0]
CNEC <- CNEC[totcapital > 0]
CNEC <- CNEC[nbarworkers > 0]
CNEC <- CNEC[wage + nonwage > 0]

# Construct categorical variables
CNEC$type_a <- factor(CNEC$type_a)
CNEC$province <- factor(CNEC$province)
CNEC$industry <- factor(CNEC$industry)
CNEC$industry_a <- factor(CNEC$industry_a)

# Calculate new variables
CNEC <- within(CNEC,
               {lcomp <- wage + nonwage
               age <- 2005 - founding_y
               })
# Calculate average factor products
alpha = 0.5376
gamma = 0.93

CNEC <- within(CNEC,
               {phik   <- product/totcapital
               phil   <- product/lcomp
               phil_1 <- product/nbarworkers
               phi    <- (phik^alpha)*(phil^(1-alpha))
               kappa  <- totcapital/lcomp
               z      <- (product/(totcapital^alpha*
                                     lcomp^(1-alpha))^gamma)^(1/(1-gamma))
               })
CNEC_RSV = CNEC
sel <- which(CNEC$industry_a == 22 | CNEC$industry_a == 13 
             | CNEC$industry_a == 15 | CNEC$industry_a == 17 
             | CNEC$industry_a == 26)
CNEC <- CNEC[sel]
CNEC_NPOL <- CNEC_RSV[-sel]

# now CNEC is polluting industries
qup <- quantile(CNEC$phi, probs=c(.975),na.rm=TRUE)
qdown <- quantile(CNEC$phi, probs=c(0.025),na.rm=TRUE)
sel <- which(CNEC$phi>qdown & CNEC$phi<qup)
CNEC_TRIM <- CNEC[sel]

qup <- quantile(CNEC_TRIM$z, probs=c(.975),na.rm=TRUE)
qdown <- quantile(CNEC_TRIM$z, probs=c(0.025),na.rm=TRUE)
sel <- which(CNEC_TRIM$z>qdown & CNEC_TRIM$z<qup)
CNEC_TRIM <- CNEC_TRIM[sel]

CNEC_TRIM <- within(CNEC_TRIM,
                    {logzratio <- log(z)/(mean(log(z),na.rm = TRUE))
                    logphiratio <- log(phi)/(mean(log(phi),na.rm = TRUE))
                    })

# Calculate quintiles
cutoff <- seq(from = 0.01, to = 0.99, by = 0.04)
qcut <- quantile(CNEC_TRIM$logzratio, probs = cutoff, na.rm = TRUE)

n1 <- length(cutoff)
zplot <- rep(0,n1-1)
zplotraw <- zplot
phiplot <- zplot
phiplotraw <- zplot

for (i in 2:n1){
  sel <- which(CNEC_TRIM$logzratio > qcut[i-1] 
               & CNEC_TRIM$logzratio <= qcut[i])
  zplot[i-1] <- 
    sum(CNEC_TRIM$logzratio[sel]*CNEC_TRIM$product[sel])/
    sum(CNEC_TRIM$product[sel])
  zplotraw[i-1] <- mean(CNEC_TRIM$logzratio[sel])
  phiplot[i-1] <- 
    sum(CNEC_TRIM$logphiratio[sel]*CNEC_TRIM$product[sel])/
    sum(CNEC_TRIM$product[sel])
  phiplotraw[i-1] <- mean(CNEC_TRIM$logphiratio[sel])
}

# ==================== Figure 3 ============================================
pdf("./Results/Figure3_Left.pdf",height=5,width=5)
plot(phiplot~zplot,cex=0.5,mgp=c(1.75, 0.75, 0), 
     xlab="Log Productivity",ylab="Log AFP",
     main="Average Factor Product: \n Polluting Sector",
     cex.main=1.5,cex.lab=1.5,
     type = "l",lwd=4,col="deepskyblue4")
dev.off()

# now CNEC is non-polluting industries
CNEC <- CNEC_NPOL
qup <- quantile(CNEC$phi, probs=c(.975),na.rm=TRUE)
qdown <- quantile(CNEC$phi, probs=c(0.025),na.rm=TRUE)
sel <- which(CNEC$phi>qdown & CNEC$phi<qup)
CNEC_TRIM <- CNEC[sel]

qup <- quantile(CNEC_TRIM$z, probs=c(.975),na.rm=TRUE)
qdown <- quantile(CNEC_TRIM$z, probs=c(0.025),na.rm=TRUE)
sel <- which(CNEC_TRIM$z>qdown & CNEC_TRIM$z<qup)
CNEC_TRIM <- CNEC_TRIM[sel]

CNEC_TRIM <- within(CNEC_TRIM,
                    {logzratio <- log(z)/(mean(log(z),na.rm = TRUE))
                    logphiratio <- log(phi)/(mean(log(phi),na.rm = TRUE))
                    })

# Calculate quintiles
cutoff <- seq(from = 0.01, to = 0.99, by = 0.04)
qcut <- quantile(CNEC_TRIM$logzratio, probs = cutoff, na.rm = TRUE)

n1 <- length(cutoff)
zplot <- rep(0,n1-1)
zplotraw <- zplot
phiplot <- zplot
phiplotraw <- zplot

for (i in 2:n1){
  sel <- which(CNEC_TRIM$logzratio > qcut[i-1] 
               & CNEC_TRIM$logzratio <= qcut[i])
  zplot[i-1] <- 
    sum(CNEC_TRIM$logzratio[sel]*CNEC_TRIM$product[sel])/
    sum(CNEC_TRIM$product[sel])
  zplotraw[i-1] <- mean(CNEC_TRIM$logzratio[sel])
  phiplot[i-1] <- 
    sum(CNEC_TRIM$logphiratio[sel]*CNEC_TRIM$product[sel])/
    sum(CNEC_TRIM$product[sel])
  phiplotraw[i-1] <- mean(CNEC_TRIM$logphiratio[sel])
}

# ==================== Figure 3 ============================================
pdf("./Results/Figure3_Right.pdf",height=5,width=5)
plot(phiplot~zplot,cex=0.5,mgp=c(1.75, 0.75, 0), 
     xlab="Log Productivity",ylab="Log AFP",
     main="Average Factor Product: \n Non-Polluting Sector",
     cex.main=1.5,cex.lab=1.5,
     type = "l",lwd=4,col="deepskyblue4")
dev.off()