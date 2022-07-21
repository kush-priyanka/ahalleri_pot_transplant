# Load libraries
library(readr)
library(dplyr)
library(udunits2) #for converting
library(tidyr)

# Load required libraries for LMM 
library(lmerTest)
library(performance)
library(MuMIn)
library(nlme)

setwd("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Data")

# Import data files
bac <- read.table("Data/ahalleri_clonal_16S_Richness_02162022.txt", header = T, row.names = 1)
fungi <- read.table("Data/ahalleri_clonal_ITS_Richness_02162022.txt", header = T, row.names = 1)

# Rename columns
colnames(bac)[12] <- "Bac_Richness"
bac$Bac_Richness <- as.numeric(bac$Bac_Richness)
colnames(bac)[13] <- "Bac_Shannon"
colnames(bac)[14] <- "Bac_NMDS1"
colnames(bac)[15] <- "Bac_NMDS2"

colnames(fungi)[12] <- "Fungi_Richness"
fungi$Fungi_Richness <- as.numeric(fungi$Fungi_Richness)
colnames(fungi)[13]  <- "Fungi_Shannon"
colnames(fungi)[14] <- "Fungi_NMDS1"
colnames(fungi)[15]  <- "Fungi_NMDS2"

# Combine data sets
## This applies when all the samples rows are in the same order in the two datasets
dat <- bac
dat$Fungi_Richness <- fungi$Fungi_Richness
dat$Fungi_Shannon <- fungi$Fungi_Shannon
dat$Fungi_NMDS1 <- fungi$Fungi_NMDS2
dat$Fungi_NMDS2 <- fungi$Fungi_NMDS2

# Subset for only plant samples
plant <- subset(dat, dat$Population != "Blank")

# Linear mixed-effects model. Fixed effect: Population (Site); Random effect: Site_type (M vs NM);
### Plants & Blank ####

# Alpha diversity
anova(lmer(Bac_Richness ~ Population + (1|Site_type), data = dat))
anova(lmer(Bac_Shannon ~ Population + (1|Site_type), data = dat))

anova(lmer(Fungi_Richness ~ Population + (1|Site_type), data = dat))
anova(lmer(Fungi_Shannon ~ Population + (1|Site_type), data = dat))

## Fungal richness doesn't fit

# Model R2
# Alpha diversity
r2(lmer(Bac_Richness ~ Population + (1|Site_type), data = dat))
r2(lmer(Bac_Shannon ~ Population + (1|Site_type), data = dat))

r2(lmer(Fungi_Richness ~ Population + (1|Site_type), data = dat))
r2(lmer(Fungi_Shannon ~ Population + (1|Site_type), data = dat))

## Fungal richness doesn't fit

### Only Plants ####

# Alpha diversity
anova(lmer(Bac_Richness ~ Population + (1|Site_type), data = plant))
anova(lmer(Bac_Shannon ~ + Population + (1|Site_type), data = plant))

anova(lmer(Fungi_Richness ~ Population + (1|Site_type), data = plant))
anova(lmer(Fungi_Shannon ~ Population + (1|Site_type), data = plant))

## Fungal richness doesn't fit

# Model R2
# Alpha diversity
r2(lmer(Bac_Richness ~ Population + (1|Site_type), data = plant))
r2(lmer(Bac_Shannon ~ Population + (1|Site_type), data = plant))

r2(lmer(Fungi_Richness ~ Population + (1|Site_type), data = plant))
r2(lmer(Fungi_Shannon ~ Population + (1|Site_type), data = plant))

## Fungal richness doesn't fit

# Get slope values
lmer(Bac_Richness ~ Population + (1|Site_type), data = plant)
lmer(Bac_Shannon ~ Population + (1|Site_type), data = plant)

lmer(Fungi_Richness ~ Population + (1|Site_type), data = plant)
lmer(Fungi_Shannon ~ Population + (1|Site_type), data = plant)


r2(lmer(Fungi_Richness ~ Soil_type + (1|Site_type), data = plant))
r2(lmer(Fungi_Shannon ~ Soil_type + (1|Site_type), data = plant))

