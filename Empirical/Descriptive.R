# --------------------------------------------------------------------------
#                       Program Description
# --------------------------------------------------------------------------
#    
# Purpose:
#     - Compute summary statistics of the NGSPS
#     - Table 1 of main text
#     - Table A.1 of Appendix A.1
# 
# Author:
#     - Xin Tang @ Stony Brook, May 2014
# 
# Record of Revisions:
#       Date:                 Description of Changes
#    ============        =================================
#     05/25/2014                 Original Version
#     09/16/2019                Improved Annotation
# ==========================================================================

# Clear memory
rm(list = ls())

# Load in packages
library(foreign)
library(AER)
library(data.table)

########################### Key Firms ##################################
load("./Data/KEYFIRM_R.RData")
setkey(KEYFIRM,industry_a)
KEYSUM_E <- KEYFIRM[, list(wastewater=sum(wastewater_e, na.rm = TRUE), 
      cod=sum(cod_e, na.rm = TRUE), nh=sum(nh_e, na.rm = TRUE), 
      pet=sum(pet_e, na.rm = TRUE), phe=sum(phe_e, na.rm = TRUE), 
      bod=sum(bod_e, na.rm = TRUE), cyn=sum(cyn_e, na.rm = TRUE), 
      as=sum(as_e, na.rm = TRUE), chr=sum(chr_e, na.rm = TRUE), 
      chr6=sum(chr6_e, na.rm = TRUE)), by=list(industry_a)]

# Calculate the percentage
KEYPER_E <- KEYSUM_E[, list(wastewater = 100*wastewater/sum(wastewater), 
      cod = 100*cod/sum(cod), nh = 100*nh/sum(nh), 
      pet = 100*pet/sum(pet), phe = 100*phe/sum(phe), 
      bod = 100*bod/sum(bod), cyn = 100*cyn/sum(cyn), 
      as = 100*as/sum(as), chr = 100*chr/sum(chr), 
      chr6 = 100*chr6/sum(chr6), industry_a = industry_a)]

# Chemical Oxygen Demand
# Table 1, Row 1
s <- order(-KEYPER_E$cod)
KEYPER_E <- KEYPER_E[s]
head(KEYPER_E$cod,5)

rm(list = ls())

########################### All Firms ##################################

load("./Data/ALLFIRM_R.RData")
setkey(ALLFIRM,industry_a)
ALLSUM_E <- ALLFIRM[, list(wastewater=sum(wastewater_e, na.rm = TRUE), 
        cod=sum(cod_e, na.rm = TRUE), nh=sum(nh_e, na.rm = TRUE), 
        pet=sum(pet_e, na.rm = TRUE), phe=sum(phe_e, na.rm = TRUE), 
        bod=sum(bod_e, na.rm = TRUE), cyn=sum(cyn_e, na.rm = TRUE), 
        as=sum(as_e, na.rm = TRUE)), by=list(industry_a)]

## Compare the relative contribution of key and regular firms
ALLCOMP_E <- ALLFIRM[, list(wastewater=sum(wastewater_e, na.rm = TRUE), 
        cod=sum(cod_e, na.rm = TRUE), nh=sum(nh_e, na.rm = TRUE), 
        pet=sum(pet_e, na.rm = TRUE), phe=sum(phe_e, na.rm = TRUE), 
        bod=sum(bod_e, na.rm = TRUE), cyn=sum(cyn_e, na.rm = TRUE), 
        as=sum(as_e, na.rm = TRUE), product = sum(product, na.rm = TRUE)), 
        by=list(industry_a, Census_Type)]
AC_PRES <- ALLCOMP_E
# Calculate the relative percentage
ALLAUX_E <- ALLFIRM[, list(wastewater=sum(wastewater_e, na.rm = TRUE), 
        cod=sum(cod_e, na.rm = TRUE), nh=sum(nh_e, na.rm = TRUE), 
        pet=sum(pet_e, na.rm = TRUE), phe=sum(phe_e, na.rm = TRUE), 
        bod=sum(bod_e, na.rm = TRUE), cyn=sum(cyn_e, na.rm = TRUE), 
        as=sum(as_e, na.rm = TRUE), product = sum(product, na.rm = TRUE)), 
        by=list(industry_a)]

# For safety consideration, sort the two datasets again
# Sort ALLCOMP_E
s <- order(ALLCOMP_E$industry_a, ALLCOMP_E$Census_Type)
ALLCOMP_E <- ALLCOMP_E[s]
# Sort ALLAUX_E
s <- order(ALLAUX_E$industry_a)
ALLAUX_E <- ALLAUX_E[s]

foo1 <- seq(from = 1, to = 79, by = 2)
for (i in foo1){
  ALLCOMP_E$wastewater[i] <- 
    100*ALLCOMP_E$wastewater[i]/ALLAUX_E$wastewater[(i+1)/2]
  ALLCOMP_E$wastewater[i+1] <- 100 - ALLCOMP_E$wastewater[i]
  ALLCOMP_E$cod[i] <- 100*ALLCOMP_E$cod[i]/ALLAUX_E$cod[(i+1)/2]
  ALLCOMP_E$cod[i+1] <- 100 - ALLCOMP_E$cod[i]
  ALLCOMP_E$nh[i] <- 100*ALLCOMP_E$nh[i]/ALLAUX_E$nh[(i+1)/2]
  ALLCOMP_E$nh[i+1] <- 100 - ALLCOMP_E$nh[i]
  ALLCOMP_E$pet[i] <- 100*ALLCOMP_E$pet[i]/ALLAUX_E$pet[(i+1)/2]
  ALLCOMP_E$pet[i+1] <- 100 - ALLCOMP_E$pet[i]
  ALLCOMP_E$phe[i] <- 100*ALLCOMP_E$phe[i]/ALLAUX_E$phe[(i+1)/2]
  ALLCOMP_E$phe[i+1] <- 100 - ALLCOMP_E$phe[i]
  ALLCOMP_E$bod[i] <- 100*ALLCOMP_E$bod[i]/ALLAUX_E$bod[(i+1)/2]
  ALLCOMP_E$bod[i+1] <- 100 - ALLCOMP_E$bod[i]
  ALLCOMP_E$cyn[i] <- 100*ALLCOMP_E$cyn[i]/ALLAUX_E$cyn[(i+1)/2]
  ALLCOMP_E$cyn[i+1] <- 100 - ALLCOMP_E$cyn[i]
  ALLCOMP_E$as[i] <- 100*ALLCOMP_E$as[i]/ALLAUX_E$as[(i+1)/2]
  ALLCOMP_E$as[i+1] <- 100 - ALLCOMP_E$as[i]
  ALLCOMP_E$product[i] <- 
    100*ALLCOMP_E$product[i]/ALLAUX_E$product[(i+1)/2]
  ALLCOMP_E$product[i+1] <- 100 - ALLCOMP_E$product[i]
}

# Table 1, Rows 2 and 3
ALLCOMP_E[which(ALLCOMP_E$industry_a == 22), list(cod, product)]
ALLCOMP_E[which(ALLCOMP_E$industry_a == 13), list(cod, product)]
ALLCOMP_E[which(ALLCOMP_E$industry_a == 17), list(cod, product)]
ALLCOMP_E[which(ALLCOMP_E$industry_a == 26), list(cod, product)]
ALLCOMP_E[which(ALLCOMP_E$industry_a == 15), list(cod, product)]

# Table A.1, Row 1
# number of firms
sel <- which(ALLFIRM$Census_Type == 1)
numkey <- length(sel)
# wastewater
sel <- which(ALLFIRM$wastewater_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# COD
sel <- which(ALLFIRM$cod_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# Petro
sel <- which(ALLFIRM$pet_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# NH4
sel <- which(ALLFIRM$nh_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# BOD
sel <- which(ALLFIRM$bod_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# CN
sel <- which(ALLFIRM$cyn_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# Cr6
sel <- which(ALLFIRM$chr6_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# Phenol
sel <- which(ALLFIRM$phe_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# As
sel <- which(ALLFIRM$as_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# Cr
sel <- which(ALLFIRM$chr_e>0 & ALLFIRM$Census_Type == 1)
num1 <- length(sel)
num1/numkey

# Table A.1, Row 2
# number of firms
sel <- which(ALLFIRM$Census_Type == 2)
numkey <- length(sel)
# wastewater
sel <- which(ALLFIRM$wastewater_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# COD
sel <- which(ALLFIRM$cod_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# Petro
sel <- which(ALLFIRM$pet_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# NH4
sel <- which(ALLFIRM$nh_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# BOD
sel <- which(ALLFIRM$bod_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# CN
sel <- which(ALLFIRM$cyn_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# Cr6
sel <- which(ALLFIRM$chr6_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# Phenol
sel <- which(ALLFIRM$phe_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# As
sel <- which(ALLFIRM$as_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey

# Cr
sel <- which(ALLFIRM$chr_e>0 & ALLFIRM$Census_Type == 2)
num1 <- length(sel)
num1/numkey