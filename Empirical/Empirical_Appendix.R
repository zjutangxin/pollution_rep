# -------------------------------------------------------------------------
#                       Program Description
# -------------------------------------------------------------------------
#    
# Purpose:
#     - Produce all the empirical results in the online appendix.
#     - 1. log(COD)~log(Sales) and Figure 1
#     - 2. Inv~log(Sales)
#     - 3. log(COD)~log(Sales) for Dirty and Clean
#     - 4. Figure 2
#     - 5. Distortions
#     - The Size Distribution of Firms and Industrial Water Pollution: A
#       Quantitative Analysis of China
#     - Prepared for AEJM R&R
# 
# Author:
#     - Xin Tang @ International Monetary Fund
# 
# Record of Revisions:
#          Date:                 Description of Changes
#    ============        =================================
#       04/06/2019                 Original Version
# =========================================================================
# Clear memory
rm(list = ls())

# Load in packages
library(foreign)
library(data.table)
library(AER)
library(scales)
library(grid)

# Set working directory
PathIn <- paste("C:/Users/zjuta/Dropbox/Daily Printing/PollutionPaper",
      "AEJ_Submission/R&R/Empirical", sep="/")
setwd(PathIn)

# =====================================================================
#                       Figures
# =====================================================================
# Distribution of output for NGSPS (Key/All), CNEC (All/Large)
# Read the full sample of CNEC 
load("./Data/CHprod_all.RData")
# Drop missing values
sel <- which(CHNprod$product > 0)
CHNprod <- CHNprod[sel]
# CNEC records output in 2004 CNY 1000, but NGSPS records output 
#   in 2007 CNY 10,000.
deflator <- 96.30/93.50
CHNprod$rproduct <- (CHNprod$product/10.0)*deflator
tmpden <- density(log(CHNprod$rproduct),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
pdf("./Figures/cnec_den_all_appen.pdf",height=5,width=5)
plot(tmpden,xlab="Log Output",ylab="Density",main="CNEC All",
     xlim = c(0, 12), ylim=c(0,0.5), cex.lab = 1.25, 
     lwd = 2.0, col = 4)
dev.off()

# Read the large firms of CNEC
load("./Data/DLARGE_R.RData")
# Drop missing values
sel <- which(DLARGE$product > 0)
DLARGE <- DLARGE[sel]
# Adjusting for inflation and the fact that CNEC records 
#     output in CNY 1000.
deflator <- 96.30/93.50
DLARGE$rproduct <- (DLARGE$product/10.0)*deflator
tmpden <- density(log(DLARGE$rproduct),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
pdf("./Figures/cnec_den_large_appen.pdf",height=5,width=5)
plot(tmpden,xlab="Log Output",ylab="Density",main="CNEC Large",
     xlim = c(0, 12), ylim=c(0,0.5), cex.lab = 1.25,
     lwd = 2.0, col = 4)
dev.off()

# Read the key firms of NGSPS
load("./Data/KEYFIRM_R.RData")
# Drop missing values
sel <- which(KEYFIRM$product > 0)
KEYFIRM <- KEYFIRM[sel]
tmpden <- density(log(KEYFIRM$product),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
pdf("./Figures/ngsps_den_key_appen.pdf",height=5,width=5)
plot(tmpden,xlab="Log Output",ylab="Density",main="NGSPS Key",
     xlim = c(0, 12), ylim=c(0,0.5), cex.lab = 1.25, 
     lwd = 2.0, col = 4)
dev.off()

# Read all firms of NGSPS
load("./Data/ALLFIRM_R.RData")
# Drop missing values
sel <- which(ALLFIRM$product > 0)
ALLFIRM <- ALLFIRM[sel]
tmpden <- density(log(ALLFIRM$product),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
pdf("./Figures/ngsps_den_all_appen.pdf",height=5,width=5)
plot(tmpden,xlab="Log Output",ylab="Density",main="NGSPS All",
     xlim = c(0, 12), ylim=c(0,0.5), 
     cex.lab = 1.25, lwd = 2.0, col = 4)
dev.off()

# Technical Features of the Treatment Technologies
# Clear memory
rm(list = ls())

# Set working directory
PathIn <- paste("C:/Users/zjuta/Dropbox/Daily Printing/PollutionPaper",
                "AEJ_Submission/R&R/Empirical", sep="/")
setwd(PathIn)

load("./Data/KEYFIRM_NEW.RData")

GB <- KEYFIRM[industry_a == 22]    # Paper Manufacturing Industry
GB <- GB[product > 0]          # Keep firms with positive production
GB <- GB[cod_e >= 0 & is.na(cod_e)==FALSE]
GB <- GB[cod_g > 0]            # Keep firms with positive COD generation
GB <- GB[dm1_inv > 0]          # Keep firms with positive abatement investment

GB <- within(GB,{
  cod_eg <- cod_e/cod_g
  dm1_unit <- dm1_quant/dm1_inv
  dm1_prod <- dm1_inv/product
  dm1_prod2 <- (dm1_inv + dm1_oprcost)/product
  dm1_prod3 <- dm1_oprcost/product
  int <- cod_e/product
})
table(GB$dm1_code_a)

# Plot Densities
pdf("./Figures/abate_inv.pdf",height=5,width=5)
tmpden <- density(log(GB$dm1_inv[GB$dm1_code_a == 1]),
        kernel = "gaussian", bw =0.50, na.rm = TRUE)
plot(tmpden,xlab="Costs",ylab="Density",main="Costs",
     ylim=c(0,0.35),cex.main=1.50,cex.lab=1.5,lwd = 2.0)
lines(density(log(GB$dm1_inv[GB$dm1_code_a == 2]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),
      col=2, lwd = 2.0)
lines(density(log(GB$dm1_inv[GB$dm1_code_a == 5 | GB$dm1_code_a == 4]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),
      col=4, kernel = "gaussian", bw =0.50, na.rm = TRUE,lwd = 2.0)
legend("topleft",c("Phy","Chem","Bio"),
       lty=c(1,1,1), col=c(1,2,4),lwd = 2.0)
dev.off()

pdf("./Figures/abate_cap.pdf",height=5,width=5)
tmpden <- density(log(GB$dm1_quant[GB$dm1_code_a == 1]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
plot(tmpden,xlab="Capacity",ylab="Density",main="Processing Capacity",
      ylim=c(0,0.25),cex.main=1.50,cex.lab=1.5,lwd = 2.0)
lines(density(log(GB$dm1_quant[GB$dm1_code_a == 2]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=2,lwd = 2.0)
lines(density(log(GB$dm1_quant[GB$dm1_code_a == 4 | GB$dm1_code_a == 5]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=4,lwd = 2.0)
legend("topleft",c("Phy","Chem","Bio"),
      lty=c(1,1,1), col=c(1,2,4),lwd =2.0)
dev.off()

pdf("./Figures/abate_unit.pdf",height=5,width=5)
tmpden <- density(log(GB$dm1_unit[GB$dm1_code_a == 1]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
plot(tmpden,xlab="Unit-cost",ylab="Density",main="Unit Capacity Cost",
      ylim=c(0,0.40),cex.main=1.50,cex.lab=1.5,lwd = 2.0)
lines(density(log(GB$dm1_unit[GB$dm1_code_a == 2]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=2,lwd = 2.0)
lines(density(log(GB$dm1_unit[GB$dm1_code_a == 4 | GB$dm1_code_a == 5]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=4,lwd = 2.0)
legend("topleft",c("Phy","Chem","Bio"),
      lty=c(1,1,1), col=c(1,2,4),lwd = 2.0)
dev.off()

pdf("./Figures/abate_prod.pdf",height=5,width=5)
tmpden <- density(log(GB$product[GB$dm1_code_a == 1]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
plot(tmpden,xlab="Production",ylab="Density",main="Production Scale",
     ylim=c(0,0.40),cex.main=1.50,cex.lab=1.5,lwd= 2.0)
lines(density(log(GB$product[GB$dm1_code_a == 2]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=2,lwd= 2.0)
lines(density(log(GB$product[GB$dm1_code_a == 4 | GB$dm1_code_a == 5]),
      kernel = "gaussian", bw =0.50, na.rm = TRUE),col=4,lwd= 2.0)
legend("topleft",c("Phy","Chem","Bio"),
      lty=c(1,1,1), col=c(1,2,4),lwd= 2.0)
dev.off()

# =====================================================================
#           Industry-level Regression Results
# =====================================================================
# Clear memory
rm(list = ls())

# Set working directory
PathIn <- paste("C:/Users/zjuta/Dropbox/Daily Printing/PollutionPaper",
                "AEJ_Submission/R&R/Empirical", sep="/")
setwd(PathIn)

load("./Data/KEYFIRM_NEW.RData")
KEYFIRM <- KEYFIRM[product > 0 & cod_e > 0]
KEYFIRM$intensity <- with(KEYFIRM, intensity <- cod_e/product)
# -------------------- PAPER -------------------------
sel <- which(KEYFIRM$industry_a == 22)
PAPER <- KEYFIRM[sel]
# All firms with 4-digit
lm_paper_all <- 
  lm(log(cod_e) ~ log(product) + province + type_a, data = PAPER)
summary(lm_paper_all)

# -------------------- FOOD -------------------------
sel <- which(KEYFIRM$industry_a == 13)
AGRI <- KEYFIRM[sel]
lm_agri_all <- 
  lm(log(cod_e) ~ log(product) + province + type_a, data = AGRI)
summary(lm_agri_all)

# -------------------- TEXTILE -------------------------
sel <- which(KEYFIRM$industry_a == 17)
TEXT <- KEYFIRM[sel]
lm_text_all <- 
  lm(log(cod_e) ~ log(product) + province + type_a, data = TEXT)
summary(lm_text_all)

# -------------------- CHEMISTRY -------------------------
sel <- which(KEYFIRM$industry_a == 26)
CHEM <- KEYFIRM[sel]
lm_chem_all <- 
  lm(log(cod_e) ~ log(product) + province + type_a, data = CHEM)
summary(lm_chem_all)

# -------------------- BEVERAGE -------------------------
sel <- which(KEYFIRM$industry_a == 15)
BEVER <- KEYFIRM[sel]
lm_bever_all <- 
  lm(log(cod_e) ~ log(product) + province + type_a, data = BEVER)
summary(lm_bever_all)

# --------- Update Figure 2 in the Appendix -------------------------------
# -------------------- PAPER -------------------------
lm_paper_aux1 <- lm(log(intensity) ~ 
    province + type_a, data = PAPER)
lm_paper_aux2 <- lm(log(product) ~ 
    province + type_a, data = PAPER)
PAPER$res_intensity <- residuals(lm_paper_aux1)
PAPER$res_product <- residuals(lm_paper_aux2)
pdf("./Figures/paper_ez.pdf",height=5,width=5)
plot(PAPER$res_intensity~PAPER$res_product,cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",main="Paper",
     cex.main=1.75,cex.lab=1.5,ylim=c(-12,7))
paper_residual <- lm(res_intensity ~ res_product, data = PAPER)
abline(paper_residual,col="red",lwd=4)
dev.off()

# -------------------- FOOD -------------------------
lm_agri_aux1 <- lm(log(intensity) ~ 
      province + type_a, data = AGRI)
lm_agri_aux2 <- lm(log(product) ~ 
      province + type_a, data = AGRI)
AGRI$res_intensity <- residuals(lm_agri_aux1)
AGRI$res_product <- residuals(lm_agri_aux2)
pdf("./Figures/agri_ez.pdf",height=5,width=5)
plot(AGRI$res_intensity~AGRI$res_product,cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",main="Agricultural Food",
     cex.main=1.75,cex.lab=1.5,ylim=c(-12,7))
agri_residual <- lm(res_intensity ~ res_product, data = AGRI)
abline(agri_residual,col="red",lwd=4)
dev.off()

# -------------------- TEXTILE -------------------------
lm_text_aux1 <- lm(log(intensity) ~ 
      province + type_a, data = TEXT)
lm_text_aux2 <- lm(log(product) ~ 
      province + type_a, data = TEXT)
TEXT$res_intensity <- residuals(lm_text_aux1)
TEXT$res_product <- residuals(lm_text_aux2)
pdf("./Figures/text_ez.pdf",height=5,width=5)
plot(TEXT$res_intensity~TEXT$res_product,cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",main="Textile",
     cex.main=1.75,cex.lab=1.5)
text_residual <- lm(res_intensity ~ res_product, data = TEXT)
abline(text_residual,col="red",lwd=4)
dev.off()

# -------------------- CHEMISTRY -------------------------
lm_chem_aux1 <- lm(log(intensity) ~ 
      province + type_a, data = CHEM)
lm_chem_aux2 <- lm(log(product) ~ 
      province + type_a, data = CHEM)
CHEM$res_intensity <- residuals(lm_chem_aux1)
CHEM$res_product <- residuals(lm_chem_aux2)
pdf("./Figures/chem_ez.pdf",height=5,width=5)
plot(CHEM$res_intensity~CHEM$res_product,cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Chemical Materials",cex.main=1.75,cex.lab=1.5)
chem_residual <- lm(res_intensity ~ res_product, data = CHEM)
abline(chem_residual,col="red",lwd=4)
dev.off()

# -------------------- BEVERAGE -------------------------
lm_bever_aux1 <- lm(log(intensity) ~ 
      province + type_a, data = BEVER)
lm_bever_aux2 <- lm(log(product) ~ 
      province + type_a, data = BEVER)
BEVER$res_intensity <- residuals(lm_bever_aux1)
BEVER$res_product <- residuals(lm_bever_aux2)
pdf("./Figures/bever_ez.pdf",height=5,width=5)
plot(BEVER$res_intensity~BEVER$res_product,cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Beverage",cex.main=1.75,cex.lab=1.5)
bever_residual <- lm(res_intensity ~ res_product, data = BEVER)
abline(bever_residual,col="red",lwd=4)
dev.off()

# Cluster the standard error at provincial level
# Inline Function for Calculating Clustered Standard Errors
cl <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

sel <- which(KEYFIRM$industry_a == 15 | KEYFIRM$industry_a == 17 
    | KEYFIRM$industry_a == 22 | KEYFIRM$industry_a == 26 
    | KEYFIRM$industry_a == 13)
ALL <- KEYFIRM[sel]
lm_all <- lm(log(cod_e) ~ log(product) 
      + province + industry_a + type_a, data = ALL)
summary(lm_all)
cl(ALL, lm_all, ALL$province)

# =====================================================================
#           Additional Results for Treatment Technology
# =====================================================================
# Clear memory
rm(list = ls())

# Set working directory
PathIn <- paste("C:/Users/zjuta/Dropbox/Daily Printing/PollutionPaper",
                "AEJ_Submission/R&R/Empirical", sep="/")
setwd(PathIn)

load("./Data/KEYFIRM_NEW.RData")
KEYFIRM <- KEYFIRM[product > 0 & cod_e > 0]

sel <- which(KEYFIRM$industry_a == 15 | KEYFIRM$industry_a == 17 
    | KEYFIRM$industry_a == 22 | KEYFIRM$industry_a == 26 
    | KEYFIRM$industry_a == 13)
POL5 <- KEYFIRM[sel]

sel <- which(POL5$dm1_inv > 0)
POL5 <- POL5[sel]
POL5$dm_y <- with(POL5, dm_y <- dm1_inv/product)
qdown <- quantile(POL5$dm_y, probs=c(0.01), na.rm = TRUE)
qup <- quantile(POL5$dm_y, probs=c(0.99), na.rm = TRUE)
sel <- which(POL5$dm_y > qdown & POL5$dm_y < qup)
POL5 <- POL5[sel]

qdown <- quantile(POL5$product, probs=c(0.80), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.99), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
summary(POL5$dm_y[sel])

lm_dmy <- lm(log(dm_y) ~ log(product) + province + type_a 
             + industry_a, data = POL5)
summary(lm_dmy)

lm_dm <- lm(log(dm1_inv) ~ log(product) + province + type_a 
            + industry_a, data = POL5)
summary(lm_dm)
