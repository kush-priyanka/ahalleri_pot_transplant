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
bac <- read.table("ahalleri_clonal_16S_Richness_02162022.txt", header = T, row.names = 1)
fungi <- read.table("ahalleri_clonal_ITS_Richness_02162022.txt", header = T, row.names = 1)

# Rename columns
colnames(bac)[11] <- "Bac_Richness"
bac$Bac_Richness <- as.numeric(bac$Bac_Richness)
colnames(bac)[12] <- "Bac_Shannon"
colnames(bac)[13] <- "Bac_NMDS1"
colnames(bac)[14] <- "Bac_NMDS2"

colnames(fungi)[11] <- "Fungi_Richness"
fungi$Fungi_Richness <- as.numeric(fungi$Fungi_Richness)
colnames(fungi)[12]  <- "Fungi_Shannon"
colnames(fungi)[13] <- "Fungi_NMDS1"
colnames(fungi)[14]  <- "Fungi_NMDS2"

# Combine data sets
dat <- bac
dat$Fungi_Richness <- fungi$Fungi_Richness
dat$Fungi_Shannon <- fungi$Fungi_Shannon
dat$Fungi_NMDS1 <- fungi$Fungi_NMDS2
dat$Fungi_NMDS2 <- fungi$Fungi_NMDS2

# Subset for only plant samples
plant <- subset(dat, dat$Population != "Blank")

# Linear mixed-effects model. Fixed effect: soil type; Random effect: ecotype;
### Plants & Blank ####

# Alpha diversity
anova(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = dat))
anova(lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = dat))

anova(lmer(Fungi_Richness ~ Soil_type+ (1|Ecotype), data = dat))
anova(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = dat))

# Model R2
# Alpha diversity
r2(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = dat))
r2(lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = dat))

r2(lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = dat))
r2(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = dat))

### Only Plants ####

# Alpha diversity
anova(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = plant))
anova(lmer(Bac_Shannon ~ + Soil_type + (1|Ecotype), data = plant))

anova(lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = plant))
anova(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = plant))

# Model R2
# Alpha diversity
r2(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = plant))
r2(lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = plant))

r2(lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = plant))
r2(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = plant))

# Get slope values
lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = plant)
lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = plant)

lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = plant)
lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = plant)

#### PL14 plants ####
pl14 <- subset(plant, plant$Population_type == "NM_PL14_P")

# Alpha diversity
anova(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = pl14))
anova(lmer(Bac_Shannon ~ + Soil_type + (1|Ecotype), data = pl14))

anova(lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = pl14))
anova(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = pl14))

# Model R2
# Alpha diversity
r2(lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = plant))
r2(lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = plant))

r2(lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = plant))
r2(lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = plant))

# Get slope values
lmer(Bac_Richness ~ Soil_type + (1|Ecotype), data = plant)
lmer(Bac_Shannon ~ Soil_type + (1|Ecotype), data = plant)

lmer(Fungi_Richness ~ Soil_type + (1|Ecotype), data = plant)
lmer(Fungi_Shannon ~ Soil_type + (1|Ecotype), data = plant)