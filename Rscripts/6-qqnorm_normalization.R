library(ggplot2)
library(agricolae)
library(gridExtra) #multiple panels in 1 figure
library(rcompanion) #TEST FOR NORMALITY + HISTOGRAMS
library(bestNormalize) #best transformation

# Import Data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header=T, row.names=1, sep="\t")

dim(chem) #120 49

#remove soil data for blanks
plant <- subset(chem, chem$Population != "Blank")
dim(plant) #80 49

#### Function for RGR ####
# Calculate number of days between t0, t1, t2
survey <- data.frame(date=c("2018/09/28", "2018/11/16"),
                     tx_start=c("2018/05/22","2018/09/28"))

survey$date_diff <- as.Date(as.character(survey$date), format="%Y/%m/%d")-
  as.Date(as.character(survey$tx_start), format="%Y/%m/%d")

survey
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


#### qqnorm and qqline Soil ####
soil <- chem[,c(1:20)]
dim(soil)

pdf("Plots/SoilChem/Soil_normalization/Soil_qqnorm_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
colnames_qqnorm <- c("Acid Phosphatase","Alkaline Phosphatase","B_glucosidase","Arylsulfatase",
                     "Cadmium_soil", "Lead_soil", "Zinc_soil","pH","Ammonium", "Nitrate", "WHC", "Basal_respiration", "C_mic")
for (i in 8:20) 
{
  qqnorm(soil[,i],main = colnames_qqnorm[i-7])
  qqline(soil[,i])
}
dev.off()

#### Test for normality and histograms soil #####
shapi.p <- matrix (data = NA, ncol = 13, nrow = 1)
colnames (shapi.p) <- colnames(soil[,8:20])

for (i in 8:20) 
{
  shapi.p[1,i-7] <- as.numeric(shapiro.test(soil[,i])$p.value)
}
shapi.p.round <- round(shapi.p, digits = 4)

pdf("Plots/SoilChem/Soil_histograms_shapi_p_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 8:20) 
{
  plotNormalHistogram(soil[,i], 
                      main = paste(colnames_qqnorm[i-7], 
                                   shapi.p.round[i-7]))     
}
dev.off()

#### Subset wo Arylsulfatase & Basal respiration as it is normal ###
soil_non_norm <- soil[, c(1:10, 12:18, 20)]

#### Choose Best transformation Soil ####
soil_trans <- matrix (data = NA, ncol = ncol(soil_non_norm[,8:18]), nrow = nrow(soil_non_norm))
colnames (soil_trans) <- colnames(soil_non_norm[,8:18])

for (i in 8:18) 
{
  x <- soil_non_norm[,i]
  (BNobject <- bestNormalize(x))
  soil_trans[,i-7]<-predict(BNobject)
}

shapi.p_trans <- matrix (data = NA, ncol = 11, nrow = 1)
colnames (shapi.p_trans) <- colnames(soil_non_norm[,8:18])

for (i in 1:11) 
{
  shapi.p_trans[1,i] <- as.numeric(shapiro.test(soil_trans[,i])$p.value)
}
shapi.p.round_trans <- round(shapi.p_trans, digits = 3)

soil_trans_final <- cbind(soil[,1:6], soil_trans, soil[,c(11,19)])
soil_trans_final <- soil_trans_final[, c(1:9,18,10:17,19)]
write.table(soil_trans_final, file = "Data/Soil_bestNormalize_clean_data_04062022.txt", 
            sep="\t", quote=F, row.names=T, col.names=NA)

colnames_soil_trans <- c("Acid Phosphatase","Alkaline Phosphatase","B_glucosidase",
                   "Cadmium_soil", "Lead_soil","Zinc_soil","pH","Ammonium", "Nitrate", "WHC","C mic")

pdf("Plots/SoilChem/Soil_normalization/Soil_bestNormilize_histograms_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 1:11) 
{
  plotNormalHistogram(soil_trans[,i], 
                      main = paste(colnames_soil_trans[i], 
                                                   shapi.p.round_trans[i]))     
}
dev.off()

#### Plot normalized qqnorm and qqline Soil #####
pdf("Plots/SoilChem/Soil_normalization/Soil_BestNormalize_qqnorm_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 1:11) 
{
  qqnorm(soil_trans[,i],main = colnames_soil_trans[i])
  qqline(soil_trans[,i])
}
dev.off()

#### qqnorm and qqline Plant ####

pdf("Plots/PlantChem/plant_normalization/Plant_qqnorm_clean_data_04062022.pdf", 
    width = 15, height = 10)
par(mfrow=c(2,7))
colnames_qqnorm <- c("Plant fresh Biomass" ,"Plant dry Biomass",
                     "Plant Cadmium","Plant Zinc", "Easy Leaf Area T0","Easy Leaf Area T1","Easy Leaf Area T2",
                     "Relative Growth Rate 1","Relative Growth Rate 2", 
                     "Relative Growth Rate 3","Fv_Fm_T0","Fv_Fm_T1", "Fv_Fm_T2", 
                     "PI_abs_T0", "PI_abs_T1", "PI_abs_T2")

for (i in 21:36) 
{
  qqnorm(plant[,i],main = colnames_qqnorm[i-20])
  qqline(plant[,i])
}
dev.off()

### Test for normality and histograms Plant ####
shapi.p <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (shapi.p) <- colnames(plant[,21:36])

for (i in 21:36) 
{
  shapi.p[1,i-20] <- as.numeric(shapiro.test(plant[,i])$p.value)
}
shapi.p.round <- round(shapi.p, digits = 4)

pdf("Plots/PlantChem/plant_normalization/Plant_histograms_clean_data_04062022.pdf", 
    width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 21:36) 
{
  plotNormalHistogram(plant[,i], main = paste(colnames_qqnorm[i-20], 
                                              shapi.p.round[i-20]))     
}
dev.off()

#### Choose the Best Transformation Plant ####
plant_trans <- matrix (data = NA, ncol = ncol(plant[,21:36]), 
                       nrow = nrow(plant))
colnames (plant_trans) <- colnames(plant[,21:36])

for (i in 21:36) 
{
  x <- plant[,i]
  (BNobject <- bestNormalize(x))
  plant_trans[,i-20]<-predict(BNobject)
}

shapi.p_trans <- matrix (data = NA, ncol = 16, nrow = 1)
colnames (shapi.p_trans) <- colnames(plant[,21:36])

for (i in 1:16) 
{
  shapi.p_trans[1,i] <- as.numeric(shapiro.test(plant_trans[,i])$p.value)
}
shapi.p.round_trans <- round(shapi.p_trans, digits = 3)

plant_trans_final <- cbind(plant[,1:6], plant_trans, plant[, 37:46])
write.table(plant_trans_final, file = "Data/Plant_bestNormalize_clean_data_04062022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

colnames_plant_trans <- c("Plant fresh Biomass" ,"Plant dry Biomass",
                          "Plant Cadmium","Plant Zinc", "Easy Leaf Area T0","Easy Leaf Area T1","Easy Leaf Area T2",
                          "Relative Growth Rate 1","Relative Growth Rate 2", 
                          "Relative Growth Rate 3","Fv_Fm_T0","Fv_Fm_T1", "Fv_Fm_T2", 
                          "PI_abs_T0", "PI_abs_T1", "PI_abs_T2")


pdf("Plots/PlantChem/plant_normalization/Plant_bestNormilize_histograms_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 1:16) 
{
  plotNormalHistogram(plant_trans[,i], 
                      main = paste(colnames_plant_trans[i], 
                                                    shapi.p.round_trans[i]))     
}
dev.off()

#### Plot normalized qqnorm and qqline Plant #####
pdf("Plots/PlantChem/plant_normalization/Plant_BestNormalize_qqnorm_clean_data_04062022.pdf", width = 15, height = 10)
par(mfrow = c(2,7))
for (i in 1:16) 
{
  qqnorm(plant_trans[,i],main = colnames_plant_trans[i])
  qqline(plant_trans[,i])
}
dev.off()