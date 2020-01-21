# --------------------------------------------------------------------------
#                       Program Description
# --------------------------------------------------------------------------
#    
# Purpose:
#     - For replicating the U.S. firm size distributions in 
#         Figures 2 and E.1.
#     - These are the results reproducible with the published data.
#     - Code snippet taken directly from Empirical_AEJ.R (Section 3) and
#         Empirical_Appendix.R (Section 5).
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
#     01/13/2020                  Original Version
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
#           U.S. Distribution in Figure 2 (blue bar)
# ==========================================================================
USall <- read.csv("./Data/susb04.csv",header = TRUE)  
USall <- as.data.table(USall)
USall <- USall[,list(NAICS, ENTRSIZE, FIRM, ESTB, 
                     EMPL, NAICSDSCR, ENTRSIZEDSCR)]

# ------------------------ Pooled Polluting ---------------------------
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
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
}
# Last group
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)

# =================== Figure 2 Left ========================================
pdf("./Results/Figure2_US_Left.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",main="Pooled Polluting", 
        cex.main = 2.50, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), 
        cex.names=1.75)
title(ylab = "Employment Share", line = 2.3, cex.lab = 1.5)
legend("topleft", "US",fill="blue",
       bty="o",cex=1.5)
dev.off()

# ------------------------ All Manufacturing -------------------------------
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
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
}
# Last category
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)

# ==================== Figure 2 Right ======================================
pdf("./Results/Figure2_US_Right.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",main="All Manufacturing", 
        cex.main = 2.50, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), 
        cex.names=1.75)
title(ylab = "Employment Share", line = 2.3, cex.lab = 1.5)
legend("topleft", "US",fill="blue",
       bty="o",cex=1.5)
dev.off()

# ==========================================================================
#           U.S. Distributions in Figure E.1 (blue bar)
# ==========================================================================
# ------------------------ Paper Industry ---------------------------
sel <- which(USall$NAICS == 3221)
US <- USall[sel]

sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,
        149,199,299,399,499,749,999,1499,2499,5000)
US <- within(US,
             {AVGF <- EMPL/FIRM   # Average firm size
             AVGE <- EMPL/ESTB   # Average plant size
             CF <- cf
             })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)

# ==================== Figure E1 Top Left ==================================
pdf("./Results/FigureE1_US_TopLeft.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share", 
        main="Paper", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", "US",fill="blue",
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# ------------------ Food Industry ------------------------------
sel <- which(USall$NAICS == 311)
US <- USall[sel]

sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,
        149,199,299,399,499,749,999,1499,2499,5000)
US <- within(US,
             {AVGF <- EMPL/FIRM   # Average firm size
             AVGE <- EMPL/ESTB   # Average plant size
             CF <- cf
             })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)

# ======================= Figure E1 Top Right ==============================
pdf("./Results/FigureE1_US_TopRight.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Agricultural Food", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", "US",fill="blue",
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# ---------------- Textile Industry -----------------------------
sel <- which(USall$NAICS == 313)
US <- USall[sel]

sel <- which(US$ENTRSIZE != 1 & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,
        149,199,299,399,499,749,999,1499,2499,5000)
US <- within(US,
             {AVGF <- EMPL/FIRM   # Average firm size
             AVGE <- EMPL/ESTB   # Average plant size
             CF <- cf
             })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)

# ====================== Figure E1 Mid Left ================================
pdf("./Results/FigureE1_US_MidLeft.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Textile", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", "US",fill="blue",
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# --------------- Chemistry Industry ----------------------------
sel <- which(USall$NAICS == 3251 | USall$NAICS == 3252 
             | USall$NAICS == 3253 | USall$NAICS == 3255 
             | USall$NAICS == 3256 | USall$NAICS == 3259)
US <- USall[sel]

sel <- which(US$ENTRSIZE != 1 
             & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
USSUM <- US[, list(FIRM=sum(FIRM, na.rm = TRUE), 
                   ESTB=sum(ESTB, na.rm = TRUE), 
                   EMPL=sum(EMPL, na.rm = TRUE)), 
            by=list(ENTRSIZE)]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,
        149,199,299,399,499,749,999,1499,2499,5000)
USSUM <- within(USSUM,
                {AVGF <- EMPL/FIRM   # Average firm size
                AVGE <- EMPL/ESTB   # Average plant size
                CF <- cf
                })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(USSUM$AVGF > cutoff[i-1] & USSUM$AVGF <= cutoff[i])
  distus[i-1] <- sum(USSUM$EMPL[sel])
}
# Last category
sel <- which(USSUM$AVGF > cutoff[n1])
distus[n1] <- sum(USSUM$EMPL[sel])
distus <- distus/sum(distus)

# ====================== Figure E1 Mid Right ===============================
pdf("./Results/FigureE1_US_MidRight.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Chemical Materials", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", "US",fill="blue",
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# --------------- Beverage Industry ------------------------
sel <- which(USall$NAICS == 3121)
US <- USall[sel]

sel <- which(US$ENTRSIZE != 1 
             & US$ENTRSIZE != 6 &  US$ENTRSIZE != 9)
US <- US[sel]
cf <- c(0,4,9,14,19,24,29,34,39,44,49,74,99,
        149,199,299,399,499,749,999,1499,2499,5000)
US <- within(US,
             {AVGF <- EMPL/FIRM   # Average firm size
             AVGE <- EMPL/ESTB   # Average plant size
             CF <- cf
             })

# Calculate the employment share
cutoff <- c(1,19,99,399)
n1 <- length(cutoff)
distus <- rep(0,n1)

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)

# ======================== Figure E1 Bottom Left ===========================
pdf("./Results/FigureE1_US_BotLeft.pdf",height=6,width=7.5)
barplot(distus,beside=TRUE,col="blue",
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Beverage", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", "US",fill="blue",
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()