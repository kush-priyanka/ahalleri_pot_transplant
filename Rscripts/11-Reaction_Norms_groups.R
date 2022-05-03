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
plant <- chem[, c(1:6, 21:49)]
plant <- subset(plant, plant$Population_type != "Blank_P")
plant <- subset(plant, plant$Population_type != "Blank_S.P")

## Add new column with population names only
plant$Pop <- with(plant, ifelse(plant$Population_type == "NM_PL14_P", 'PL14', 
                               ifelse(plant$Population_type == "NM_PL35_P", 'PL35',
                                      ifelse(plant$Population_type == "M_PL22_P", 'PL22','PL27'))))

plant$Native <- with(plant, ifelse(plant$Population_type == "NM_PL14_P" & plant$Soil_type == "NM_PL14_S", 'Native', 
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
                       function(x) scale(x, scale = FALSE))

plant.center <- cbind(plant.sub[,1:6], plant.center, plant.sub[,19:21])

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
    ylab(colnames_se1[i]) +
    scale_colour_manual(values = c("blue", "red"),
                        breaks = c("PL14", "PL22"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set1/reactn_norm_set1_", colnames_se1[i], "_05022022.pdf"),
         plot = plant_plots1[[i]],
         width = 5,
         height = 4,
         units ="in")
}

#### Set 2: PL14 & PL17 ####
p2 <- c('PL14', 'PL27')
s2 <- c('NM_PL14_S', 'M_PL27_S')

set2 <- plant.sub %>%
  filter(Pop %in% p2) %>%
  filter(Soil_type %in% s2)

## Set 2: Calculate mean and se ###
se2 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se2) <- c(1:4)
colnames(se2) <- names.set
se2$Pop <- as.character(c("PL14","PL14",
                          "PL27","PL27"))
se2$Soil_type <- as.character(c("NM_PL14_S","M_PL27_S",
                                "NM_PL14_S","M_PL27_S"))
for(i in 7:18){
  stat2 <- summarySE(set2, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se2[, i - 4] <- as.numeric(stat2[[4]])
  se2[, i + 8] <- as.numeric(stat2[[6]])
}

## Set 2: Plot reaction norm ##
se2$Soil_type <- factor(se2$Soil_type,
                        levels = c("NM_PL14_S", 
                                   "M_PL27_S"))

colnames_se2 <- names(se2)[3:14]
plant_plots2 <- list()
for(i in 1:12){
  plant_plots2[[i]] <- ggplot(se2, aes(x = Soil_type,
                                       y = se2[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se2[,i + 2]  - se2[, i + 14],
                      ymax = se2[,i + 2]  + se2[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylab(colnames_se2[i]) +
    scale_colour_manual(values = c("blue", "#FF66B2"),
                        breaks = c("PL14", "PL27"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set2/reactn_norm_set2_", colnames_se2[i], "_05022022.pdf"),
         plot = plant_plots2[[i]],
         width = 5,
         height = 4,
         units ="in")
}

#### Set 3: PL14 & PL35 ####
p3 <- c('PL14', 'PL35')
s3 <- c('NM_PL14_S', 'NM_PL35_S')

set3 <- plant.sub %>%
  filter(Pop %in% p3) %>%
  filter(Soil_type %in% s3)

## Set 3: Calculate mean and se ###
se3 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se3) <- c(1:4)
colnames(se3) <- names.set
se3$Pop <- as.character(c("PL14","PL14",
                          "PL35","PL35"))
se3$Soil_type <- as.character(c("NM_PL14_S","NM_PL35_S",
                                "NM_PL14_S","NM_PL35_S"))
for(i in 7:18){
  stat3 <- summarySE(set3, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se3[, i - 4] <- as.numeric(stat3[[4]])
  se3[, i + 8] <- as.numeric(stat3[[6]])
}

## Set 3: Plot reaction norm ##
se3$Soil_type <- factor(se3$Soil_type,
                        levels = c("NM_PL14_S", 
                                   "NM_PL35_S"))

colnames_se3 <- names(se3)[3:14]
plant_plots3 <- list()
for(i in 1:12){
  plant_plots3[[i]] <- ggplot(se3, aes(x = Soil_type,
                                       y = se3[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se3[,i + 2]  - se3[, i + 14],
                      ymax = se3[,i + 2]  + se3[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylab(colnames_se3[i]) +
    scale_colour_manual(values = c("blue", "#66B2FF"),
                        breaks = c("PL14", "PL35"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set3/reactn_norm_set3_", colnames_se3[i], "_05022022.pdf"),
         plot = plant_plots3[[i]],
         width = 5,
         height = 4,
         units ="in")
}

#### Set 4: PL35 & PL22 ####
p4 <- c('PL35', 'PL22')
s4 <- c('NM_PL35_S', 'M_PL22_S')

set4 <- plant.sub %>%
  filter(Pop %in% p4) %>%
  filter(Soil_type %in% s4)

## Set 4: Calculate mean and se ###
se4 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se4) <- c(1:4)
colnames(se4) <- names.set
se4$Pop <- as.character(c("PL35","PL35",
                          "PL22","PL22"))
se4$Soil_type <- as.character(c("NM_PL35_S","M_PL22_S",
                                "NM_PL35_S","M_PL22_S"))
for(i in 7:18){
  stat4 <- summarySE(set4, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se4[, i - 4] <- as.numeric(stat4[[4]])
  se4[, i + 8] <- as.numeric(stat4[[6]])
}

## Set 4: Plot reaction norm ##
se4$Soil_type <- factor(se4$Soil_type,
                        levels = c("NM_PL35_S", 
                                   "M_PL22_S"))

colnames_se4 <- names(se4)[3:14]
plant_plots4 <- list()
for(i in 1:12){
  plant_plots4[[i]] <- ggplot(se4, aes(x = Soil_type,
                                       y = se4[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se4[,i + 2]  - se4[, i + 14],
                      ymax = se4[,i + 2]  + se4[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylab(colnames_se4[i]) +
    scale_colour_manual(values = c("#66B2FF", "red"),
                        breaks = c("PL35", "PL22"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set4/reactn_norm_set4_", colnames_se4[i], "_05022022.pdf"),
         plot = plant_plots4[[i]],
         width = 5,
         height = 4,
         units ="in")
}

#### Set 5: PL35 & PL27 ####
p5 <- c('PL35', 'PL27')
s5 <- c('NM_PL35_S', 'M_PL27_S')

set5 <- plant.sub %>%
  filter(Pop %in% p5) %>%
  filter(Soil_type %in% s5)

## Set 5: Calculate mean and se ###
se5 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se5) <- c(1:4)
colnames(se5) <- names.set
se5$Pop <- as.character(c("PL35","PL35",
                          "PL27","PL27"))
se5$Soil_type <- as.character(c("NM_PL35_S","M_PL27_S",
                                "NM_PL35_S","M_PL27_S"))
for(i in 7:18){
  stat5 <- summarySE(set5, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se5[, i - 4] <- as.numeric(stat5[[4]])
  se5[, i + 8] <- as.numeric(stat5[[6]])
}

## Set 5: Plot reaction norm ##
se5$Soil_type <- factor(se5$Soil_type,
                        levels = c("NM_PL35_S", 
                                   "M_PL27_S"))

colnames_se5 <- names(se5)[3:14]
plant_plots5 <- list()
for(i in 1:12){
  plant_plots5[[i]] <- ggplot(se5, aes(x = Soil_type,
                                       y = se5[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se5[,i + 2]  - se5[, i + 14],
                      ymax = se5[,i + 2]  + se5[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylab(colnames_se5[i]) +
    scale_colour_manual(values = c("#66B2FF", "#FF66B2"),
                        breaks = c("PL35", "PL27"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set5/reactn_norm_set5_", colnames_se5[i], "_05022022.pdf"),
         plot = plant_plots5[[i]],
         width = 5,
         height = 4,
         units ="in")
}

#### Set 6: PL22 & PL27 ####
p6 <- c('PL22', 'PL27')
s6 <- c('M_PL22_S', 'M_PL27_S')

set6 <- plant.sub %>%
  filter(Pop %in% p6) %>%
  filter(Soil_type %in% s6)

## Set 6: Calculate mean and se ###
se6 <- data.frame(matrix(nrow = 4, ncol = 26))
rownames(se6) <- c(1:4)
colnames(se6) <- names.set
se6$Pop <- as.character(c("PL22","PL22",
                          "PL27","PL27"))
se6$Soil_type <- as.character(c("M_PL22_S","M_PL27_S",
                                "M_PL22_S","M_PL27_S"))
for(i in 7:18){
  stat6 <- summarySE(set6, 
                     measurevar = i, 
                     groupvars = c("Pop", "Soil_type"), 
                     na.rm = TRUE)
  se6[, i - 4] <- as.numeric(stat6[[4]])
  se6[, i + 8] <- as.numeric(stat6[[6]])
}

## Set 6: Plot reaction norm ##
se6$Soil_type <- factor(se6$Soil_type,
                        levels = c("M_PL22_S", 
                                   "M_PL27_S"))

colnames_se6 <- names(se6)[3:14]
plant_plots6 <- list()
for(i in 1:12){
  plant_plots6[[i]] <- ggplot(se6, aes(x = Soil_type,
                                       y = se6[,i + 2],
                                       group = interaction(Pop),
                                       color = Pop)) +
    geom_line() +
    geom_errorbar(aes(ymin = se6[,i + 2]  - se6[, i + 14],
                      ymax = se6[,i + 2]  + se6[, i + 14]),
                  width = 0.1) +
    geom_point(size = 2, shape = 19) +
    ylab(colnames_se6[i]) +
    scale_colour_manual(values = c("red", "#FF66B2"),
                        breaks = c("PL22", "PL27"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/set6/reactn_norm_set6_", colnames_se6[i], "_05022022.pdf"),
         plot = plant_plots6[[i]],
         width = 5,
         height = 4,
         units ="in")
}


### Use library(cowplot) to combine figures
com_plots <- list()
colnames_com <- names(se6)[3:14]
for(i in 1:12){
  com_plots[[i]] <- plot_grid(plant_plots1[[i]],
                              plant_plots2[[i]],
                              plant_plots3[[i]],
                              plant_plots4[[i]],
                              plant_plots5[[i]],
                              plant_plots6[[i]],
                              ncol = 3)

  ggsave(filename = paste0("Plots/PlantChem/Reaction_norms/centered/pop_group/pop_combined/centered_", colnames_com[i], "_05022022.pdf"),
         plot = com_plots[[i]],
         width = 12,
         height = 5,
         units ="in")
}
