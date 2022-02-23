# Load Libraries
library(car) # for leveneTest

# Import Raw Data
chem <- read.table("Data/Ahalleri_soil_plant_02172022.txt", 
                   header=T, row.names=1, sep="\t")

chem <- chem[c(1:120),c(1:20,23:48)]
dim(chem) #120 46

##### Homegenity tests on raw Soil data ####
#####  Bartlett test for soil data #####
bart.soil <- matrix (data = NA, ncol = 14, nrow = 1)
colnames (bart.soil) <- colnames(chem[,7:20])
rownames(bart.soil) <- "bar.soil.prenorm"

for (i in 7:20) 
{
  b <- bartlett.test(chem[, i] ~ Ecotype, data = chem)
  bart.soil[1,i-6] <- as.numeric(b$p.value)
}

#### Levene's test for soil data ####
lev.soil <- matrix (data = NA, ncol = 14, nrow = 1)
colnames (lev.soil) <- colnames(chem[,7:20])
rownames(lev.soil) <- "lev.soil.prenorm"

for (i in 7:20) 
{
  l <- leveneTest(chem[, i] ~ Ecotype, data = chem)
  lev.soil[1,i-6] <- as.numeric(l[[3]][1])
}

soil.hom <- rbind(bart.soil , lev.soil)

# Subset plant data to remove pots wo plants
plant <- subset(chem, chem$Population != "Blank")
dim(plant)

# Number of days between T0, T1, T2 (total= 178)
d1 <- 129
d2 <- 49
d3 <- d1 + d2

# RGR between T0 & T1
rel_growth_rate1 <- function(ela0, ela1, d) {
  rgr1 <- ((log(ela1)-log(ela0))/d)
  return(rgr1)
}

# RGR between T1 & T2
rel_growth_rate2 <- function(ela1, ela2, d) {
  rgr2 <- ((log(ela2)-log(ela1))/d)
  return(rgr2)
}

# RGR between T0 & T2
rel_growth_rate3 <- function(ela0, ela2, d) {
  rgr3 <- ((log(ela2)-log(ela0))/d)
  return(rgr3)
}

# Calculate RGRs
plant$RGR1 <- rel_growth_rate1(ela0 = plant$ELA_T0, 
                               ela1 = plant$ELA_T1, 
                               d = d1)

plant$RGR2 <- rel_growth_rate2(ela1 = plant$ELA_T1, 
                               ela2 = plant$ELA_T2, 
                               d = d2)

plant$RGR3 <- rel_growth_rate3(ela0 = plant$ELA_T0, 
                               ela2 = plant$ELA_T2, 
                               d = d3)

# Rearrange columns
plant <- plant[, c(1:22,27:28, 24:26,47:49,41:46,29:40)]

#### Homegenity tests on raw Plant data ####
#####  Bartlett test for plant data #####
bart.plant <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (bart.plant ) <- colnames(plant[,21:36])
rownames(bart.plant) <- "bar.plant .prenorm"

for (i in 21:36) 
{
  bp <- bartlett.test(plant[, i] ~ Ecotype, data = plant)
  bart.plant [1,i-20] <- as.numeric(bp$p.value)
}

#### Levene's test for plant data ####
lev.plant <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (lev.plant ) <- colnames(plant[,21:36])
rownames(lev.plant) <- "lev.plant .prenorm"

for (i in 21:36) 
{
  lp <- leveneTest(plant [, i] ~ Ecotype, data = plant)
  lev.plant [1,i-20] <- as.numeric(lp[[3]][1])
}

plant.hom <- rbind(bart.plant  , lev.plant)

# Import normalized files
soil.norm <- read.table("Data/Soil_bestNormalize_02232022.txt", 
                   header=T, row.names=1, sep="\t")
dim(soil.norm) #120 20

# Rearrange soil.norm columns to match raw data
soil.norm <- soil.norm[,c(1:6, 8:19, 7, 20)]

plant.norm <- read.table("Data/Plant_bestNormalize_02232022.txt", 
                        header=T, row.names=1, sep="\t")
dim(plant.norm) #80 32

##### Homegenity tests on normalized Soil data ####
#####  Bartlett test for norm soil data #####
bart.soil.nm <- matrix (data = NA, ncol = 14, nrow = 1)
colnames (bart.soil.nm) <- colnames(soil.norm[,7:20])
rownames(bart.soil.nm) <- "bar.soil.norm"

for (i in 7:20) 
{
  b <- bartlett.test(soil.norm[, i] ~ Ecotype, data = soil.norm)
  bart.soil.nm[1,i-6] <- as.numeric(b$p.value)
}

#### Levene's test for norm soil data ####
lev.soil.nm <- matrix (data = NA, ncol = 14, nrow = 1)
colnames (lev.soil.nm) <- colnames(soil.norm[,7:20])
rownames(lev.soil.nm) <- "lev.soil.norm"

for (i in 7:20) 
{
  l <- leveneTest(soil.norm[, i] ~ Ecotype, data = soil.norm)
  lev.soil.nm[1,i-6] <- as.numeric(l[[3]][1])
}

soil.hom <- rbind(soil.hom, bart.soil.nm)
soil.hom <- rbind(soil.hom, lev.soil.nm)

soil.hom.final <- t(soil.hom[c(1,3,2,4),])

# Save the results
write.table(soil.hom.final, file = "Results/Soil_Plant/Soil_Homogenity_Variance_02232022.txt", 
            sep="\t", quote=F, row.names=T, col.names=NA)

##### Homegenity tests on normalized Plant data ####
#####  Bartlett test for norm plant data #####
bart.plant.nm <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (bart.plant.nm) <- colnames(plant.norm[,7:22])
rownames(bart.plant.nm) <- "bar.plant.norm"

for (i in 7:22) 
{
  b <- bartlett.test(plant.norm[, i] ~ Ecotype, data = plant.norm)
  bart.plant.nm[1,i-6] <- as.numeric(b$p.value)
}

#### Levene's test for norm plant data ####
lev.plant.nm <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (lev.plant.nm) <- colnames(plant.norm[, 7:22])
rownames(lev.plant.nm) <- "lev.plant.norm"

for (i in 7:22) 
{
  l <- leveneTest(plant.norm[, i] ~ Ecotype, data = plant.norm)
  lev.plant.nm[1,i-6] <- as.numeric(l[[3]][1])
}

plant.hom <- rbind(plant.hom, bart.plant.nm)
plant.hom <- rbind(plant.hom, lev.plant.nm)

plant.hom.final <- t(plant.hom[c(1,3,2,4),])

# Save the results
write.table(plant.hom.final, file = "Results/Soil_Plant/Plant_Homogenity_Variance_02232022.txt", 
            sep="\t", quote=F, row.names=T, col.names=NA)