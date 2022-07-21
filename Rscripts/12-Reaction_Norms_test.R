## Load library
library(Rmisc)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

## Subset plant data and remove blanks
plant <- chem[, c(1:6, 22:37)]
plant <- subset(plant, plant$Population_type != "Blank_P")
plant <- subset(plant, plant$Population_type != "Blank_S.P")

## Add new column with population names only
plant$Pop <-with(plant, ifelse(plant$Population_type == "NM_PL14_P", 'PL14', 
                               ifelse(plant$Population_type == "NM_PL35_P", 'PL35',
                                      ifelse(plant$Population_type == "M_PL22_P", 'PL22','PL27'))))

plant$Native <-with(plant, ifelse(plant$Population_type == "NM_PL14_P" & plant$Soil_type == "NM_PL14_S", 'Native', 
                                  ifelse(plant$Population_type == "NM_PL35_P" & plant$Soil_type == "NM_PL35_S", 'Native',
                                         ifelse(plant$Population_type == "M_PL22_P" & plant$Soil_type == "M_PL22_S", 'Native',
                                                ifelse(plant$Population_type == "M_PL27_P" & plant$Soil_type == "M_PL27_S", 'Native', 'Non_native')))))

plant$Ecotype <- factor(plant$Ecotype,
                        levels = c("NM", "M"))

plant$Native <- factor(plant$Native,
                       levels = c("Native", "Non_native"))

plant$Soil_type <- factor(plant$Soil_type,
                          levels = c("NM_PL14_S", "NM_PL35_S",
                                     "M_PL22_S","M_PL27_S"))

plant$Pop <- factor(plant$Pop,
                    levels = c("PL14", "PL35", "PL22", "PL27"))

plant.sub <- plant[,c(1:6,7:10,12:14,16,18,19,21:24)]

# Create Vector of Column Max and Min Values
maxs <- apply(plant[,7:22], 2, max)
mins <- apply(plant[,7:22], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(plant[,7:22], 
                                   center = mins, 
                                   scale = maxs-mins))

plant.center <- cbind(plant[,1:6], scaled.data, plant[,23:24])

#### All sets: names for calculations columns ####
names.set <- c("Pop", "Soil_type",           
               "fresh_weight_plant", "dry_weight_plant",  "Cd_plant",          
               "Zn_plant" , "ELA_T1", "ELA_T2", "RGR1", "RGR3",              
               "Fv_Fm_T1", "Fv_Fm_T2", "PI_abs_T1", "PI_abs_T2", 
               "fweight_se", "dweight_se",  "Cd_se",          
               "Zn_se" , "ELA_T1_se", "ELA_T2_se", "RGR1_se", "RGR3_se",              
               "Fv_Fm_T1_se", "Fv_Fm_T2_se", "PI_abs_T1_se", "PI_abs_T2_se")

#### Set 1: PL14 & PL22 ####
p1 <- c('PL14', 'PL22')
s1 <- c('NM_PL14_S', 'M_PL22_S')

set1 <- plant.center %>%
  filter(Pop %in% p1) %>%
  filter(Soil_type %in% s1)

## Set 1: Calculate mean and se ###
se1 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se1) <- c(1:4)
colnames(se1) <- names.set
se1$Pop <- as.character(c("PL14","PL14",
                          "PL22","PL22"))
se1$Soil_type <- as.character(c("NM_PL14_S","M_PL22_S",
                                "NM_PL14_S","M_PL22_S"))
for(i in 7:18){
  stat1 <- summarySE(set1, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se1[, i - 4] <- as.numeric(stat1[[4]])
  se1[, i + 8] <- as.numeric(stat1[[6]])
}

## Set 1: Plot reaction norm ##
se1$Soil_type <- factor(se1$Soil_type,
                        levels = c("NM_PL14_S", 
                                   "M_PL22_S"))

colnames_se1 <- names(se1)[3:14]
plant_plots1 <- list()
for(i in 1:12){
  plant_plots1[[i]] <- ggplot(se1, aes(x = Soil_type,
                                       y = se1[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se1[,i + 2]  - se1[, i + 14],
                      ymax = se1[,i + 2]  + se1[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylim(0, 1) +
    ylab(colnames_se1[i]) +
    scale_colour_manual(values = c("blue", "red"),
                        breaks = c("PL14", "PL22"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/test_pop_group/reactn_norm_set1_", colnames_se1[i], "_05262022.pdf"),
         plot = plant_plots1[[i]],
         width = 5,
         height = 4,
         units ="in")
}
