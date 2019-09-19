# --------------------------------------------------------------------------
#                       Program Description
# --------------------------------------------------------------------------
#    
# Purpose:
#     - Produce most of the empirical results in the Online Appendices.
#     - 1. Tables: C.1, D.1
#     - 2. Figures: B.1, C.1, C.2, D.1, E.1
#     - 3. Regressions: Appendix C.1, D.2
#     - 4. Miscellaneous Info: Appendix D.2
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
#       09/17/2019                Improved Annotation
# ==========================================================================
# Clear memory
rm(list = ls())

# Load in packages
library(foreign)
library(data.table)
library(AER)
library(scales)
library(grid)

# ==========================================================================
#         1. Firm Size Distribution in Different Datasets
# ==========================================================================
# ====================== Figure B.1 ========================================
# Distribution of output for NGSPS (Key/All), CNEC (All/Large)
# Read the full sample of CNEC 
load("./Data/CNEC_avgp.RData")
CHNprod <- CNEC_avgp
rm(CNEC_avgp)
# Drop missing values
sel <- which(CHNprod$product > 0)
CHNprod <- CHNprod[sel]
# CNEC records output in 2004 CNY 1000, but NGSPS records output 
#   in 2007 CNY 10,000.
deflator <- 96.30/93.50
CHNprod$rproduct <- (CHNprod$product/10.0)*deflator
tmpden <- density(log(CHNprod$rproduct),
      kernel = "gaussian", bw =0.50, na.rm = TRUE)
pdf("./Results/FigureB1_BotRight.pdf",height=5,width=5)
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
pdf("./Results/FigureB1_BotLeft.pdf",height=5,width=5)
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
pdf("./Results/FigureB1_TopLeft.pdf",height=5,width=5)
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
pdf("./Results/FigureB1_TopRight.pdf",height=5,width=5)
plot(tmpden,xlab="Log Output",ylab="Density",main="NGSPS All",
     xlim = c(0, 12), ylim=c(0,0.5), 
     cex.lab = 1.25, lwd = 2.0, col = 4)
dev.off()

# ==========================================================================
#           2. Industry-level Regression Results
# ==========================================================================
# ---------------------- Individual Industry --------------------------
# Clear memory
rm(list = ls())

load("./Data/KEYFIRM_R.RData")
# ------------ Aggregate ownership rights type ------------------
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
             | KEYFIRM$type == 220 | KEYFIRM$type == 230 
             | KEYFIRM$type == 240)
KEYFIRM$type_a[sel] <- 4
# Foreign
sel <- which(KEYFIRM$type == 300 | KEYFIRM$type == 310
             | KEYFIRM$type == 320 | KEYFIRM$type == 330 
             | KEYFIRM$type == 340)
KEYFIRM$type_a[sel] <- 5

# ------------ Aggregate treatment technology type ------------------
# For disposal equipment type
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

# --------------- Declare dummies -----------------
KEYFIRM$province <- factor(KEYFIRM$province)        # Province
KEYFIRM$industry_a <- factor(KEYFIRM$industry_a)    # 2-digit industry
KEYFIRM$Census_Type <- factor(KEYFIRM$Census_Type)  # key 1, regular 2
KEYFIRM$dm1_code_a <- factor(KEYFIRM$dm1_code_a)    # treatment
KEYFIRM$type_a <- factor(KEYFIRM$type_a)            # ownership rights

KEYFIRM <- KEYFIRM[product > 0 & cod_e > 0]
KEYFIRM$intensity <- with(KEYFIRM, intensity <- cod_e/product)

# ====================== Table C1 ==========================================
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

# ====================== Figure C1 =========================================
# -------------------- PAPER -------------------------
lm_paper_aux1 <- lm(log(intensity) ~ 
    province + type_a, data = PAPER)
lm_paper_aux2 <- lm(log(product) ~ 
    province + type_a, data = PAPER)
PAPER$res_intensity <- residuals(lm_paper_aux1)
PAPER$res_product <- residuals(lm_paper_aux2)
pdf("./Results/FigureC1_TopLeft.pdf",height=5,width=5)
plot(PAPER$res_intensity~PAPER$res_product,cex=0.5,
     mgp=c(1.75, 0.75, 0),
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
pdf("./Results/FigureC1_TopRight.pdf",height=5,width=5)
plot(AGRI$res_intensity~AGRI$res_product,
     cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Agricultural Food",
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
pdf("./Results/FigureC1_MidLeft.pdf",height=5,width=5)
plot(TEXT$res_intensity~TEXT$res_product,
     cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Textile",
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
pdf("./Results/FigureC1_MidRight.pdf",height=5,width=5)
plot(CHEM$res_intensity~CHEM$res_product,
     cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Chemical Materials",
     cex.main=1.75,cex.lab=1.5)
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
pdf("./Results/FigureC1_BotLeft.pdf",height=5,width=5)
plot(BEVER$res_intensity~BEVER$res_product,
     cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity",
     main="Beverage",
     cex.main=1.75,cex.lab=1.5)
bever_residual <- lm(res_intensity ~ res_product, data = BEVER)
abline(bever_residual,col="red",lwd=4)
dev.off()

# ---------------------- All Manufacturing -----------------------
rm(list = ls())
load("./Data/ALLFIRM_R.RData")
# ------------ Aggregate ownership rights type ------------------
# 0: missing, 1: state/collective, 3: private, 4: HMT, 5: foreign
ALLFIRM$type_a <- 0
# State and collective
sel <- which(ALLFIRM$type == 110 | ALLFIRM$type == 141
    | ALLFIRM$type == 151 | ALLFIRM$type == 120 | ALLFIRM$type == 130
    | ALLFIRM$type == 140 | ALLFIRM$type == 150 | ALLFIRM$type == 142
    | ALLFIRM$type == 143 | ALLFIRM$type == 149 | ALLFIRM$type == 159
    | ALLFIRM$type == 160 | ALLFIRM$type == 100 )
ALLFIRM$type_a[sel] <- 1
# Private
sel <- which(ALLFIRM$type == 170 | ALLFIRM$type == 171
    | ALLFIRM$type == 172 | ALLFIRM$type == 173
    | ALLFIRM$type == 174 | ALLFIRM$type == 190)
ALLFIRM$type_a[sel] <- 3
# Hong Kong, Macau and Taiwan
sel <- which(ALLFIRM$type == 200 | ALLFIRM$type == 210
    | ALLFIRM$type == 220 | ALLFIRM$type == 230 
    | ALLFIRM$type == 240)
ALLFIRM$type_a[sel] <- 4
# Foreign
sel <- which(ALLFIRM$type == 300 | ALLFIRM$type == 310
    | ALLFIRM$type == 320 | ALLFIRM$type == 330 
    | ALLFIRM$type == 340)
ALLFIRM$type_a[sel] <- 5

# ------------ Aggregate treatment technology type ------------------
# For disposal equipment type
ALLFIRM$dm1_code_a <- 0
# Physical
sel <- which(ALLFIRM$dm1_code >= 1000 & ALLFIRM$dm1_code < 2000)
ALLFIRM$dm1_code_a[sel] <- 1
# Chemical
sel <- which(ALLFIRM$dm1_code >= 2000 & ALLFIRM$dm1_code < 3000)
ALLFIRM$dm1_code_a[sel] <- 2
# Physiochemical
sel <- which(ALLFIRM$dm1_code >= 3000 & ALLFIRM$dm1_code < 4000)
ALLFIRM$dm1_code_a[sel] <- 3
# Biological
sel <- which(ALLFIRM$dm1_code >= 4000 & ALLFIRM$dm1_code < 5000)
ALLFIRM$dm1_code_a[sel] <- 4
# Combination
sel <- which(ALLFIRM$dm1_code >= 5000 & ALLFIRM$dm1_code < 6000)
ALLFIRM$dm1_code_a[sel] <- 5

# --------------- Declare dummies -----------------
ALLFIRM$province <- factor(ALLFIRM$province)        
ALLFIRM$industry_a <- factor(ALLFIRM$industry_a)    
ALLFIRM$Census_Type <- factor(ALLFIRM$Census_Type)  
ALLFIRM$dm1_code_a <- factor(ALLFIRM$dm1_code_a)    
ALLFIRM$type_a <- factor(ALLFIRM$type_a)            

POLLUTEALL <- ALLFIRM

qup <- quantile(POLLUTEALL$product, probs=c(.99),na.rm= TRUE)
qdown <- quantile(POLLUTEALL$product, probs=c(.01),na.rm= TRUE)
sel <- which(POLLUTEALL$product > 0 & POLLUTEALL$product < qup
             & POLLUTEALL$product > qdown)
POLLUTEALL <- POLLUTEALL[sel]

# ================== Regression (!#) Page 9 Upper ==========================
CODR <- POLLUTEALL[cod_e > 0]
lm_all <- lm(log(cod_e)~log(product)+industry_a+province+type_a,
             data=CODR)
summary(lm_all)

# ================== Figure C2 =============================================
CODR$intensity <- with(CODR, intensity <- cod_e/product)
lm_codr_aux1 <- lm(log(intensity) ~ 
                     province + type_a + industry_a, data = CODR)
lm_codr_aux2 <- lm(log(product) ~ 
                     province + type_a + industry_a, data = CODR)
CODR$res_intensity <- residuals(lm_codr_aux1)
CODR$res_product <- residuals(lm_codr_aux2)
pdf("./Results/FigureC2.pdf",height=5,width=5)
plot((CODR$res_intensity)~(CODR$res_product),
     cex=0.5,mgp=c(1.75, 0.75, 0),
     xlab="Log Production",ylab="Log Intensity Residual",
     main="All Manufacturing",cex.main=1.75,cex.lab=1.5)
codr_residual <- lm(res_intensity ~ res_product, data = CODR)
abline(codr_residual,col="red",lwd=4)
dev.off()

# ================== Regression (!#) Page 9 Lower ==========================
CODR$clean <- 0
sel <- which(CODR$dm1_code_a == 4 | CODR$dm1_code_a == 5)
CODR$clean[sel] <- 1
lm_clean_all <- lm(clean ~ log(product) + industry_a 
                   + province + type_a, data = CODR)
summary(lm_clean_all)

# ----------------- Cluster Standard Errors ----------------------------
# Cluster the standard error at provincial level
# Inline Function for Calculating Clustered Standard Errors
rm(list = ls())
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

load("./Data/KEYFIRM_R.RData")
# ------------ Aggregate ownership rights type ------------------
# 0: missing, 1: state/collective, 3: private, 4: HMT, 5: foreign
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

# ------------ Aggregate treatment technology type ------------------
# For disposal equipment type
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

# --------------- Declare dummies -----------------
KEYFIRM$province <- factor(KEYFIRM$province)        # Province
KEYFIRM$industry_a <- factor(KEYFIRM$industry_a)    # 2-digit industry
KEYFIRM$Census_Type <- factor(KEYFIRM$Census_Type)  # key 1, regular 2
KEYFIRM$dm1_code_a <- factor(KEYFIRM$dm1_code_a)    # treatment
KEYFIRM$type_a <- factor(KEYFIRM$type_a)            # ownership rights
sel <- which(KEYFIRM$industry_a == 15 | KEYFIRM$industry_a == 17 
    | KEYFIRM$industry_a == 22 | KEYFIRM$industry_a == 26 
    | KEYFIRM$industry_a == 13)
ALL <- KEYFIRM[sel]
ALL <- ALL[product > 0 & cod_e > 0]
# ================== Regression (!#) Page 10 ===============================
lm_all <- lm(log(cod_e) ~ log(product) 
      + province + industry_a + type_a, data = ALL)
summary(lm_all)
cl(ALL, lm_all, ALL$province)

# ==========================================================================
#         3. Additional Information of Treatment Technologies
# ==========================================================================
# Clear memory
rm(list = ls())
# ====================== Figure D.3 ========================================
load("./Data/KEYFIRM_R.RData")
# ------------ Aggregate ownership rights type ------------------
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
             | KEYFIRM$type == 220 | KEYFIRM$type == 230 
             | KEYFIRM$type == 240)
KEYFIRM$type_a[sel] <- 4
# Foreign
sel <- which(KEYFIRM$type == 300 | KEYFIRM$type == 310
             | KEYFIRM$type == 320 | KEYFIRM$type == 330 
             | KEYFIRM$type == 340)
KEYFIRM$type_a[sel] <- 5

# ------------ Aggregate treatment technology type ------------------
# For disposal equipment type
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

# --------------- Declare dummies -----------------
KEYFIRM$province <- factor(KEYFIRM$province)        # Province
KEYFIRM$industry_a <- factor(KEYFIRM$industry_a)    # 2-digit industry
KEYFIRM$Census_Type <- factor(KEYFIRM$Census_Type)  # key 1, regular 2
KEYFIRM$dm1_code_a <- factor(KEYFIRM$dm1_code_a)    # treatment
KEYFIRM$type_a <- factor(KEYFIRM$type_a)            # ownership rights

GB <- KEYFIRM[industry_a == 22]    
GB <- GB[product > 0]          
GB <- GB[cod_e >= 0 & is.na(cod_e)==FALSE]
GB <- GB[cod_g > 0]            
GB <- GB[dm1_inv > 0]          

GB <- within(GB,{
  cod_eg <- cod_e/cod_g
  dm1_unit <- dm1_quant/dm1_inv
  dm1_prod <- dm1_inv/product
  dm1_prod2 <- (dm1_inv + dm1_oprcost)/product
  dm1_prod3 <- dm1_oprcost/product
  int <- cod_e/product
})

# Plot Densities
pdf("./Results/FigureD3_TopRight.pdf",height=5,width=5)
tmpden <- density(log(GB$dm1_inv[GB$dm1_code_a == 1]),
                  kernel = "gaussian", bw =0.50, na.rm = TRUE)
plot(tmpden,xlab="Costs",ylab="Density",main="Costs",
     ylim=c(0,0.35),cex.main=1.50,cex.lab=1.5,lwd = 2.0)
lines(density(log(GB$dm1_inv[GB$dm1_code_a == 2]),
              kernel = "gaussian", bw =0.50, na.rm = TRUE),
      col=2, lwd = 2.0)
lines(density(log(GB$dm1_inv[GB$dm1_code_a == 5 | GB$dm1_code_a == 4]),
              kernel = "gaussian", bw =0.50, na.rm = TRUE),
      col=4, lwd = 2.0)
legend("topleft",c("Phy","Chem","Bio"),
       lty=c(1,1,1), col=c(1,2,4),lwd = 2.0)
dev.off()

pdf("./Results/FigureD3_TopLeft.pdf",height=5,width=5)
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

pdf("./Results/FigureD3_BotLeft.pdf",height=5,width=5)
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

pdf("./Results/FigureD3_BotRight.pdf",height=5,width=5)
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

# ===================== Results in Appendix D.2 ============================
# Clear memory
rm(list = ls())

load("./Data/KEYFIRM_R.RData")
# ------------ Aggregate ownership rights type ------------------
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

# ------------ Aggregate treatment technology type ------------------
# For disposal equipment type
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

# --------------- Declare dummies -----------------
KEYFIRM$province <- factor(KEYFIRM$province)        
KEYFIRM$industry_a <- factor(KEYFIRM$industry_a)    
KEYFIRM$Census_Type <- factor(KEYFIRM$Census_Type)  
KEYFIRM$dm1_code_a <- factor(KEYFIRM$dm1_code_a)    
KEYFIRM$type_a <- factor(KEYFIRM$type_a)            
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

# ====================== Ratios on page 14 =================================
qdown <- quantile(POL5$product, probs=c(0.001), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.20), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
quint1 <- mean(POL5$dm_y[sel])

qdown <- quantile(POL5$product, probs=c(0.20), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.40), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
quint2 <- mean(POL5$dm_y[sel])

qdown <- quantile(POL5$product, probs=c(0.40), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.60), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
quint3 <- mean(POL5$dm_y[sel])

qdown <- quantile(POL5$product, probs=c(0.60), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.80), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
quint4 <- mean(POL5$dm_y[sel])

qdown <- quantile(POL5$product, probs=c(0.80), na.rm = TRUE)
qup <- quantile(POL5$product, probs=c(0.99), na.rm = TRUE)
sel <- which(POL5$product > qdown & POL5$product < qup)
quint5 <- mean(POL5$dm_y[sel])

quint <- c(quint1,quint2,quint3,quint4,quint5)
quint

# ================== Regression on Page 14 =================================
lm_dmy <- lm(log(dm_y) ~ log(product) + province + type_a 
             + industry_a, data = POL5)
summary(lm_dmy)

# =========================== Table D1 =====================================
phy <- POL5[POL5$dm1_code_a == 1]
qdown <- quantile(phy$dm1_inv, probs=c(0.01), na.rm = TRUE)
qup <- quantile(phy$dm1_inv, probs=c(0.99), na.rm = TRUE)
sel <- which(phy$dm1_inv > qdown & phy$product < qup)
phy <- phy[sel]
summary(phy$dm1_inv)

bio <- POL5[POL5$dm1_code_a == 4 | POL5$dm1_code_a == 5]
qdown <- quantile(bio$dm1_inv, probs=c(0.01), na.rm = TRUE)
qup <- quantile(bio$dm1_inv, probs=c(0.99), na.rm = TRUE)
sel <- which(bio$dm1_inv > qdown & bio$product < qup)
bio <- bio[sel]
summary(bio$dm1_inv)

# ==========================================================================
#                 4. Correlated Distortions
# ==========================================================================
rm(list = ls())
# ====================== Figure D1 =========================================
load("./Data/CNEC_avgp.RData")
CNEC <- CNEC_avgp
rm(CNEC_avgp)
# General sample selection: 
#   regular producing firms with positive production, capital and labor
CNEC <- CNEC[status == 1]
CNEC <- CNEC[product > 0]
CNEC <- CNEC[totcapital > 0]
CNEC <- CNEC[nbarworkers > 0]
CNEC <- CNEC[wage + nonwage > 0]

PAPER <- CNEC[industry_a == 22]

PAPER <- within(PAPER,
                {lcomp <- wage + nonwage
                age <- 2005 - founding_y
                })
alpha = 0.5376
gamma = 0.93

# Calculate average factor products
PAPER <- within(PAPER,
          {phik <- product/totcapital
           phil <- product/lcomp
           phil_1 <- product/nbarworkers
           phi <- (phik^alpha)*(phil^(1-alpha))
           phi1 <- product/totcapital*lcomp
           kappa <- totcapital/lcomp
           z <- product/(totcapital^alpha * lcomp^(1-alpha))
           z2 <- (product/(totcapital^alpha * 
                             lcomp^(1-alpha))^gamma)^(1/(1-gamma))})

qup <- quantile(PAPER$phik, probs=c(.97),na.rm=TRUE)
qdown <- quantile(PAPER$phik, probs=c(0.03),na.rm=TRUE)
sel <- which(PAPER$phik>qdown & PAPER$phik<qup)
phikz <- PAPER[sel]

qup <- quantile(phikz$z2, probs=c(.97),na.rm=TRUE)
qdown <- quantile(phikz$z2, probs=c(0.03),na.rm=TRUE)
sel <- which(phikz$z2>qdown & phikz$z2<qup)
phikz <- phikz[sel]

qup <- quantile(PAPER$phil, probs=c(.97),na.rm=TRUE)
qdown <- quantile(PAPER$phil, probs=c(0.03),na.rm=TRUE)
sel <- which(PAPER$phil>qdown & PAPER$phil<qup)
philz <- PAPER[sel]

qup <- quantile(philz$z2, probs=c(.97),na.rm=TRUE)
qdown <- quantile(philz$z2, probs=c(0.03),na.rm=TRUE)
sel <- which(philz$z2>qdown & philz$z2<qup)
philz <- philz[sel]

qup <- quantile(PAPER$kappa, probs=c(.97),na.rm=TRUE)
qdown <- quantile(PAPER$kappa, probs=c(0.03),na.rm=TRUE)
sel <- which(PAPER$kappa>qdown & PAPER$kappa<qup)
kappaz <- PAPER[sel]

qup <- quantile(kappaz$z2, probs=c(.97),na.rm=TRUE)
qdown <- quantile(kappaz$z2, probs=c(0.03),na.rm=TRUE)
sel <- which(kappaz$z2>qdown & kappaz$z2<qup)
kappaz <- kappaz[sel]

# =================== Figure D1 ============================================
# Plot ARPK
pdf("./Results/FigureD1_TopLeft.pdf",height=5,width=5)
plot(log(phikz$phik)~log(phikz$z2),cex=0.5,mgp=c(1.75, 0.75, 0), 
     xlab="Productivity",ylab="ARK",
     main="Average Product of Capital",cex.main=1.5,cex.lab=1.5)
p_lm <- lm(log(phik)~log(z2), data = phikz)
abline(p_lm,col="red",lwd=4)
dev.off()

# Plot ARPL
pdf("./Results/FigureD1_TopRight.pdf",height=5,width=5)
plot(log(philz$phil)~log(philz$z2),cex=0.5,mgp=c(1.75, 0.75, 0), 
     xlab="Productivity",ylab="ARL",
     main="Average Product of Labor",cex.main=1.50,cex.lab=1.50)
p_lm <- lm(log(phil)~log(z2), data = philz)
abline(p_lm,col="red",lwd=4)
dev.off()

# Plot k/l ratio
pdf("./Results/FigureD1_Bot.pdf",height=5,width=5)
plot(log(kappaz$kappa)~log(kappaz$z2),cex=0.5,
     mgp=c(1.75, 0.75, 0), xlab="Productivity",ylab="K/L Ratio",
     main="Capital-labor Ratio",cex.main=1.35,cex.lab=1.35)
p_lm <- lm(log(kappa)~log(z2), data = kappaz)
abline(p_lm,col="red",lwd=4)
dev.off()

# ==========================================================================
#         5. Employment Distributions by Industry
# ==========================================================================
rm(list = ls())

# Load in packages
library(foreign)
library(data.table)
library(AER)
library(ggplot2)
library(scales)
library(grid)

load("./Data/CNEC_avgp.RData")
CHNall <- CNEC_avgp
USall <- read.csv("./Data/susb04.csv",header = TRUE)
USall <- as.data.table(USall)
USall <- USall[,list(NAICS, ENTRSIZE, FIRM, 
                     ESTB, EMPL, NAICSDSCR, ENTRSIZEDSCR)]

# ------------------------ Paper Industry ---------------------------
sel <- which(CHNall$industry_a == 22)
CH <- CHNall[sel]
qch <- quantile(CH$nbarworkers, probs=c(.99),na.rm=TRUE)
sel <- which(CH$nbarworkers < qch)
CH <- CH[sel]
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
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
                & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# =========================== Figure E1 ====================================
pdf("./Results/FigureE1_TopLeft.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share", 
        main="Paper", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# ---------------- Textile Industry -----------------------------
sel <- which(CHNall$industry_a == 17)
CH <- CHNall[sel]
qch <- quantile(CH$nbarworkers, probs=c(.99),na.rm=TRUE)
sel <- which(CH$nbarworkers < qch)
CH <- CH[sel]

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
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
                & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# =========================== Figure E1 ====================================
pdf("./Results/FigureE1_MidLeft.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Textile", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# ------------------ Food Industry ------------------------------
sel <- which(CHNall$industry_a == 13)
CH <- CHNall[sel]
qch <- quantile(CH$nbarworkers, probs=c(.99),na.rm=TRUE)
sel <- which(CH$nbarworkers < qch)
CH <- CH[sel]

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
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
                & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# =========================== Figure E1 ====================================
pdf("./Results/FigureE1_TopRight.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Agricultural Food", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# --------------- Chemistry Industry ----------------------------
sel <- which(CHNall$industry_a == 26)
CH <- CHNall[sel]
qch <- quantile(CH$nbarworkers, probs=c(.99),na.rm=TRUE)
sel <- which(CH$nbarworkers < qch)
CH <- CH[sel]

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

# =========================== Figure E1 ====================================
pdf("./Results/FigureE3_MidRight.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Chemical Materials", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()

# --------------- Beverage Industry ------------------------
sel <- which(CHNall$industry_a == 15)
CH <- CHNall[sel]
qch <- quantile(CH$nbarworkers, probs=c(.99),na.rm=TRUE)
sel <- which(CH$nbarworkers < qch)
CH <- CH[sel]
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
distchn <- rep(0,n1)
distus <- distchn

for (i in 2:n1){
  sel <- which(US$AVGF > cutoff[i-1] & US$AVGF <= cutoff[i])
  distus[i-1] <- sum(US$EMPL[sel])
  sel1 <- which(CH$nbarworkers > cutoff[i-1] 
                & CH$nbarworkers <= cutoff[i])
  distchn[i-1] <- sum(CH$nbarworkers[sel1])
}
# Last category
sel <- which(US$AVGF > cutoff[n1])
distus[n1] <- sum(US$EMPL[sel])
distus <- distus/sum(distus)
sel1 <- which(CH$nbarworkers > cutoff[n1])
distchn[n1] <- sum(CH$nbarworkers[sel1])
distchn <- distchn/sum(distchn)

# =========================== Figure E1 ====================================
pdf("./Results/FigureE3_BotLeft.pdf",height=6,width=7.5)
barplot(rbind(distchn,distus),beside=TRUE,col=c("red","blue"),
        ylim=c(0,1.0),xlab="Firm Size",ylab="Employment Share",
        main="Beverage", cex.main = 2.5, cex.lab = 1.75,
        names.arg=c("1-19","20-99","100-399","400+"), cex.names=1.75)
legend("topleft", c("China","US"),fill=c("red","blue"),
       bty="o",y.intersp=0.7,x.intersp=0.3,text.width=0.8)
dev.off()