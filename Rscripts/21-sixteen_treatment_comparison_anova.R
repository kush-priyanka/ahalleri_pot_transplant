# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)
library(multcompView)

## Import raw soil + plant data 
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header=T, row.names=1, sep="\t")

#### Subset raw soil data ####
soil <- chem[, c(1:21, 50)]
soil_woblanks <- subset(soil, soil$Population != "Blank")

soil_woblanks$Soil_type <-factor(soil_woblanks$Soil_type,
                                 levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

soil_woblanks$Population_type <-factor(soil_woblanks$Population_type,
                                       levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))

## Import Normalized Data
soil_norm <- read.table("Data/Soil_bestNormalize_clean_data_04062022.txt", 
                        header = T, row.names = 1, sep = "\t")
soil_norm <- soil_norm[, c(1:17,19,18)]

soil_norm_woblanks <- subset(soil_norm, soil_norm$Population != "Blank")
soil_norm_woblanks <- cbind(soil_norm_woblanks, soil_woblanks$Group)

colnames(soil_norm_woblanks)[20] <- 'Group'

## Add new column with order of Group
soil_norm_woblanks$Level <- with(soil_norm_woblanks, ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL14_P", 'L1', 
                                                            ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL35_P", 'L2',
                                                                   ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL22_P", 'L3',
                                                                          ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                                 ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                                        ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                                               ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                                      ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                                             ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                                    ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                                           ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                                  ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                                         ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                                ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                                       ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))


#### Matrix for sig letters Soil ####
anova.matrix <- matrix(data = NA, nrow = 16, ncol = 13)
rownames(anova.matrix) = c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(anova.matrix) = colnames(soil_norm_woblanks[, 7:19])

for(i in 7:19)
{
  a1 <- aov(soil_norm_woblanks[,i]~ Level, data = soil_norm_woblanks)
  tukey <- TukeyHSD(a1)
  Cld <- multcompLetters4(a1, tukey)
  cld <- as.data.frame.list(Cld$Level)
  cld.ordered <- cld[order(as.character(row.names(cld))),]
  cld.ordered <- cld.ordered[c(1,9:16,2:8),]
  cld.let <- cld.ordered$Letters
  anova.matrix[, i - 6] <- cld.let
}

#### Assign max value for sig letters position ####
names <- c("Soil_type","Population_type", "Acid_Phosphatase_yloc","Alkaline_Phosphatase_yloc",
           "B_glucosidase_yloc" ,"Arylsulfatase_yloc",
           "Cd_soil_yloc", "Pb_soil_yloc",  "Zn_soil_yloc" ,  "pH_soil_yloc",               
           "Ammonium_soil_yloc" , "Nitrate_soil_yloc", "WHC_Soil_yloc", "Basal_respiration_Soil_yloc", 
           "C_mic_Soil_yloc",
           "Acid_Phosphatase_sig","Alkaline_Phosphatasesig","B_glucosidase_sig" ,"Arylsulfatase_sig",
           "Cd_soil_sig", "Pb_soil_sig",  "Zn_soil_sig" ,  "pH_soil_sig",               
           "Ammonium_soil_sig" , "Nitrate_soil_sig", "WHC_Soil_sig", "Basal_respiration_Soil_sig", 
           "C_mic_Soil_sig")

sig <- data.frame(matrix(nrow = 16, ncol = 28))
rownames(sig) <- c(1:16)
colnames(sig) <- names

sig$Population_type <- as.character(c("NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P", "M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P"))

sig$Soil_type <- as.character(c("NM_PL14_S","NM_PL14_S", "NM_PL14_S","NM_PL14_S",
                                "NM_PL35_S", "NM_PL35_S", "NM_PL35_S","NM_PL35_S",
                                "M_PL22_S", "M_PL22_S","M_PL22_S","M_PL22_S",
                                "M_PL27_S","M_PL27_S", "M_PL27_S","M_PL27_S"))

for(i in 9:21){
  sig[, i - 6] <- (max(soil_woblanks[,i] +(max(soil_woblanks[,i]))*0.03))
  sig[, i + 7] <- anova.matrix[,i-8]
}

### Plot soil variables using ggplot 
colnames_soil <- names(soil_woblanks)[9:21]
soil_plots <- list()
soil_plots <- lapply(9:21, function(i){
  ggplot(soil_woblanks, aes(x = Soil_type,
                            y = soil_woblanks[,i], 
                            fill = Population_type)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom ="point", shape = 22, size = 3, color = "black",
                 position = position_dodge2(width = 0.75,   
                                            preserve = "single")) +
    ylab(colnames_soil[i-8]) + 
    scale_fill_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          legend.position="bottom") +
    geom_text(data = sig, aes(y = sig[,i-6], label = sig[,i+7]), 
              position = position_dodge(width = .75))
})

pdf("Plots/SoilChem/Soil_ANOVA1_07132022.pdf", width = 16, height = 8)
grid.arrange(soil_plots[[1]],soil_plots[[2]],soil_plots[[3]],
             soil_plots[[4]],soil_plots[[5]],ncol = 2)
dev.off()


pdf("Plots/SoilChem/Soil_ANOVA2_07132022.pdf", width = 16, height = 8)
grid.arrange(soil_plots[[6]],soil_plots[[7]],soil_plots[[8]],
             soil_plots[[9]],soil_plots[[10]], ncol = 2)
dev.off()

pdf("Plots/SoilChem/Soil_ANOVA3_07132022.pdf", width = 16, height = 8)
grid.arrange(soil_plots[[11]],soil_plots[[12]],soil_plots[[13]], ncol = 2)
dev.off()

#### Import plant dataset ####
## Subset raw soil data
plant <- subset(chem, chem$Population != "Blank")
plant <- plant[, c(1:7,22:37,50)]

plant$Soil_type <-factor(plant$Soil_type,
                         levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

plant$Population_type <-factor(plant$Population_type,
                               levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))


## Import Normalized Data
plant_norm <- read.table("Data/Plant_bestNormalize_clean_data_04062022.txt", 
                         header = T, row.names = 1, sep = "\t")
plant_norm <- plant_norm[, c(1:22)]

plant_norm <- cbind(plant_norm, plant$Group)
colnames(plant_norm)[23] <- 'Group'

## Add new column with order of Group
plant_norm$Level <- with(plant_norm, ifelse(plant_norm$Group == "NM_PL14_S_PL14_P", 'L1', 
                                            ifelse(plant_norm$Group == "NM_PL14_S_PL35_P", 'L2',
                                                   ifelse(plant_norm$Group == "NM_PL14_S_PL22_P", 'L3',
                                                          ifelse(plant_norm$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                 ifelse(plant_norm$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                        ifelse(plant_norm$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                               ifelse(plant_norm$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                      ifelse(plant_norm$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                             ifelse(plant_norm$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                    ifelse(plant_norm$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                           ifelse(plant_norm$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                  ifelse(plant_norm$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                         ifelse(plant_norm$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                ifelse(plant_norm$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                       ifelse(plant_norm$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))

#### Matrix for sig letters Plant ####
anova.matrix2 <- matrix(data = NA, nrow = 16, ncol = 16)
rownames(anova.matrix2) = c('L1', 'L2', 'L3','L4',
                         'L5','L6', 'L7', 'L8', 
                         'L9', 'L10', 'L11', 'L12', 
                         'L13', 'L14','L15', 'L16')
colnames(anova.matrix2) = colnames(plant_norm[,7:22])

for(i in 7:22)
{
  a2 <- aov(plant_norm[,i]~ Level, data = plant_norm)
  tukey2 <- TukeyHSD(a2)
  Cld2 <- multcompLetters4(a2, tukey2)
  cld2 <- as.data.frame.list(Cld2$Level)
  cld.ordered2 <- cld2[order(as.character(row.names(cld2))),]
  cld.ordered2 <- cld.ordered2[c(1,9:16,2:8),]
  cld.let2 <- cld.ordered2$Letters
  anova.matrix2[, i - 6] <- cld.let2
}


### Assign max value for sig letters position ###
names1 <- c("Soil_type","Population_type", "fresh_weight_plant_yloc",  "dry_weight_plant_yloc ",   
            "Cd_plant_yloc", "Zn_plant_yloc", "ELA_T0_yloc", "ELA_T1_yloc", "ELA_T2_yloc",             
            "RGR1_yloc",  "RGR2_yloc",  "RGR3_yloc",  "Fv_Fm_T0_yloc"  ,"Fv_Fm_T1_yloc", "Fv_Fm_T2_yloc",          
            "PI_abs_T0_yloc", "PI_abs_T1_yloc", "PI_abs_T2_yloc",
            "fresh_weight_plant_sig",  "dry_weight_plant_sig",  "Cd_plant_sig",  "Zn_plant_sig", 
            "ELA_T0_sig", "ELA_T1_sig", "ELA_T2_sig","RGR1_sig",               
            "RGR2_sig ", "RGR3_sig","Fv_Fm_T0_sig", "Fv_Fm_T1_sig", "Fv_Fm_T2_sig" ,          
            "PI_abs_T0_sig" , "PI_abs_T1_sig", "PI_abs_T2_sig")

sig1 <- data.frame(matrix(nrow = 16, ncol = 34))
rownames(sig1) <- c(1:16)
colnames(sig1) <- names1

sig1$Population_type <- as.character(c("NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                       "NM_PL14_P","NM_PL35_P", "M_PL22_P","M_PL27_P",
                                       "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                       "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P"))

sig1$Soil_type <- as.character(c("NM_PL14_S","NM_PL14_S", "NM_PL14_S","NM_PL14_S",
                                 "NM_PL35_S", "NM_PL35_S", "NM_PL35_S","NM_PL35_S",
                                 "M_PL22_S", "M_PL22_S","M_PL22_S","M_PL22_S",
                                 "M_PL27_S","M_PL27_S", "M_PL27_S","M_PL27_S"))

for(i in 8:23){
  sig1[, i - 5] <- (max(plant[,i] +(max(plant[,i]))*0.03))
  sig1[, i + 11] <- anova.matrix2[,i-7]
}

### Plot plant variables using ggplot 
colnames_plant <- names(plant)[8:23]
plant_plots <- list()
plant_plots <- lapply(8:23, function(i){
  ggplot(plant, aes(x = Soil_type,
                    y = plant[,i],
                    fill = Population_type)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom ="point", shape = 22, size = 3, color = "black",
                 position = position_dodge2(width = 0.75,   
                                            preserve = "single")) +
    ylab(colnames_plant[i-7]) + 
    scale_fill_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          legend.position = "bottom") +
    geom_text(data = sig1, aes(y = sig1[,i-5], label = sig1[,i+11]), 
              position = position_dodge(width = .75))
})

pdf("Plots/PlantChem/Plant_ANOVA1_07132022.pdf", width = 16, height = 10)
grid.arrange(plant_plots[[1]],plant_plots[[2]],plant_plots[[3]],plant_plots[[4]], ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_ANOVA2_07132022.pdf", width = 16, height = 10)
grid.arrange(plant_plots[[5]], plant_plots[[6]], plant_plots[[7]], 
             plant_plots[[8]],  ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_ANOVA3_07132022.pdf", width = 16, height = 10)
grid.arrange(plant_plots[[9]],plant_plots[[10]],plant_plots[[11]],
             plant_plots[[12]],ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_ANOVA4_07132022.pdf", width = 16, height = 10)
grid.arrange(plant_plots[[13]],plant_plots[[14]],plant_plots[[15]],
             plant_plots[[16]],ncol = 2)
dev.off()




