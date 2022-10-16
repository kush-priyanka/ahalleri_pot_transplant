## Load library
library(Rmisc)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)

install.packages("scales")                              
library("scales")

setwd("C:/Users/Priyanka/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant")

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

## Subset plant data and remove blanks
plant <- chem[, c(1:6, 21:49)]
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

plant.sub <- plant[,c(1:10,12:14,16,18,19,21,22,35,36,37)]

plant.center <- sapply(plant.sub[,7:18],
                       function(x) rescale(x, to = c(-1, 1)))

plant.center <- cbind(plant.sub[,1:6], plant.center, plant.sub[,19:21])

names <- c("Pop", "Soil_type", "Ecotype",           
           "fresh_weight_plant", "dry_weight_plant",  "Cd_plant",          
           "Zn_plant" , "ELA_T1", "ELA_T2", "RGR1", "RGR3",              
          "Fv_Fm_T1", "Fv_Fm_T2", "PI_abs_T1", "PI_abs_T2", 
          "fweight_se", "dweight_se",  "Cd_se",          
          "Zn_se" , "ELA_T1_se", "ELA_T2_se", "RGR1_se", "RGR3_se",              
          "Fv_Fm_T1_se", "Fv_Fm_T2_se", "PI_abs_T1_se", "PI_abs_T2_se")

#### Standard error- soil type within each ecotype ####
se <- data.frame(matrix(nrow = 16, ncol = 27))
rownames(se) <- c(1:16)
colnames(se) <- names
se$Pop <- as.character(c("PL14","PL14","PL14","PL14",
            "PL35","PL35", "PL35","PL35",
            "PL22","PL22","PL22","PL22",
            "PL27","PL27","PL27","PL27"))
se$Soil_type <- as.character(c("NM_PL14_S","NM_PL35_S","M_PL22_S","M_PL27_S",
                         "NM_PL14_S","NM_PL35_S", "M_PL22_S","M_PL27_S",
                         "NM_PL14_S","NM_PL35_S","M_PL22_S","M_PL27_S",
                         "NM_PL14_S","NM_PL35_S","M_PL22_S","M_PL27_S"))
se$Ecotype <- as.character(c("NM", "NM", "M", "M",
                         "NM", "NM", "M", "M",
                         "NM", "NM", "M", "M",
                         "NM", "NM", "M", "M"))

for(i in 7:18){
stat <- summarySE(plant.center, 
                    measurevar = i, 
                    groupvars = c("Pop", "Soil_type"), 
                    na.rm = TRUE)
se[, i - 3] <- as.numeric(stat[[5]])
se[, i + 9] <- as.numeric(stat[[7]])
}


#### Plot reaction norm: within ecotype ####
se$Soil_type <- factor(se$Soil_type,
                          levels = c("NM_PL14_S", "NM_PL35_S",
                                     "M_PL22_S","M_PL27_S"))
 
colnames_se <- names(se)[4:15]
plant_plots <- list()
plant_plots <- lapply(1:12, function(i){
 ggplot(se, aes(x = Soil_type,
                         y = se[,i + 3],
                         group = interaction(Pop),
                         color = Pop)) +
  geom_line() +
  geom_errorbar(aes_string(ymin = se[,i + 3]  - se[, i + 15],
                    ymax = se[,i + 3]  + se[, i + 15]),
                width = 0.1) +
  geom_point(size = 2, shape = 19) +
  scale_colour_manual(values = c("blue", "red","#FF66B2", "#66B2FF"),
                        breaks = c("PL14", "PL22","PL27", "PL35")) +
  ylab(colnames_se[i]) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/four_soil/reactn_norm_four_soils_", colnames_se[i], "_05052022.pdf"),
         plot = plant_plots[[i]],
         width = 5,
         height = 4,
         units ="in")
 })

#### Plot reaction norm: within ecotype combined ####
pdf("Plots/PlantChem/Reaction_norms/centered/four_soil/reactn_norm_four_soils_combined_05052022.pdf", 
    width = 12, height = 6)
grid.arrange(plant_plots[[1]],
             plant_plots[[2]],
             plant_plots[[3]],
             plant_plots[[4]],
             plant_plots[[5]],
             plant_plots[[6]],
             plant_plots[[7]],
             plant_plots[[8]],
             plant_plots[[9]],
             plant_plots[[10]],
             plant_plots[[11]],
             plant_plots[[12]],
             ncol = 4)
dev.off()


#### Standard error- ecotype ####
se <- data.frame(matrix(nrow = 8, ncol = 27))
rownames(se) <- c(1:8)
colnames(se) <- names
se$Pop <- as.character(c("PL14","PL14",
                         "PL35","PL35",
                         "PL22","PL22",
                         "PL27","PL27"))

se$Ecotype <- as.character(c("NM", "M",
                             "NM", "M",
                             "NM", "M",
                             "NM", "M"))
for(i in 7:18){
  stat <- summarySE(plant.center, 
                    measurevar = i, 
                    groupvars = c("Pop", "Ecotype"), 
                    na.rm = TRUE)
  se[, i - 3] <- as.numeric(stat[[5]])
  se[, i + 9] <- as.numeric(stat[[7]])
}

#### Plot reaction norm: ecotype ####
se$Ecotype <- factor(se$Ecotype,
                       levels = c("NM", "M"))

colnames_se <- names(se)[4:15]
plant_plots <- list()
plant_plots <- lapply(1:12, function(i){
  ggplot(se, aes(x = Ecotype,
                    y = se1[,i + 3],
                                    group = interaction(Pop),
                                    color = Pop)) +
    geom_line() +
    geom_errorbar(aes_string(ymin = se[,i + 3]  - se[, i + 15],
                      ymax = se[,i + 3]  + se[, i + 15]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    scale_colour_manual(values = c("blue", "red","#FF66B2", "#66B2FF"),
                        breaks = c("PL14", "PL22","PL27", "PL35"))+
    ylab(colnames_se[i]) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered_0_1/ecotype/reactn_norm_ecotype_", colnames_se1[i], "_05262022.pdf"),
         plot = plant_plots[[i]],
         width = 5,
         height = 4,
         units ="in")
})

#### Plot reaction norm: ecotype combined ####
pdf("Plots/PlantChem/Reaction_norms/centered_0_1/ecotype/reactn_norm_ecotype_combined_05262022.pdf", 
    width = 12, height = 6)
grid.arrange(plant_plots[[1]],
             plant_plots[[2]],
             plant_plots[[3]],
             plant_plots[[4]],
             plant_plots[[5]],
             plant_plots[[6]],
             plant_plots[[7]],
             plant_plots[[8]],
             plant_plots[[9]],
             plant_plots[[10]],
             plant_plots[[11]],
             plant_plots[[12]],
             ncol = 4)
dev.off()

