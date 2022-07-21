## Load libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

## Import Clean Data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

dim(chem) #120 49

## Subset only plant data
plant <- subset(chem, chem$Population != "Blank")
# plant[is.na(plant)] <- 0

plant$Population_type <- factor(plant$Population_type,
                                            levels = c("NM_PL14_P", 
                                                       "NM_PL35_P", 
                                                       "M_PL22_P", 
                                                       "M_PL27_P"))

plant$Soil_type <- factor(plant$Soil_type,
                                levels = c("NM_PL14_S", 
                                           "NM_PL35_S", 
                                           "M_PL22_S", 
                                           "M_PL27_S"))




# lmod = lm(Zn_plant ~Cd_plant, plant)
# 
# ggplot(data = plant, aes(x = Zn_plant, y = Cd_plant))+
#   geom_point(aes(shape = Soil_type,  color = Population_type)) +
#   geom_smooth(method = "lm", se = FALSE, formula = y~ 0 + x, aes(color = Population_type))+
#   stat_regline_equation(label.x=30, label.y=310) +
#   stat_cor(aes(label=..rr.label..), label.x=30, label.y=290)+
#   theme_bw() 
# +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
#   xlab("Zn in plant shoot")+ylab("Cd in plant shoot")
# 
#  
#   
#     geom_smooth(method = "lm", se = FALSE, formula = y~ 0 + x) +



pdf("Plots/PlantChem/Cd_Zn_Corr_Pop_07052022.pdf", 
    width = 7, height = 5)
ggscatter(plant, x = "Zn_plant", y = "Cd_plant",
                color = "Population_type", shape =  "Soil_type", 
                add = "reg.line", conf.int = FALSE) +
  scale_colour_manual(values = c("blue", "#66B2FF", "red", "#FF66B2"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P"))+
      facet_wrap(~ Population_type) +
  stat_cor(method = "pearson", aes(label = ..r.label..), label.x = 3, label.y = 450) +
  stat_regline_equation(label.x = 3,label.y = 530, ) +  
  xlab("Zn in shoot"~ "(mg kg"^-1*")") + ylab("Cd in shoot"~ "(mg kg"^-1*")") +
  guides(color = FALSE)   
dev.off()

pdf("Plots/PlantChem/Cd_Zn_Corr_Soil_07052022.pdf",
    width = 7, height = 5)
ggscatter(plant, x = "Zn_plant", y = "Cd_plant",
          color = "Soil_type", shape =  "Population_type", 
          add = "reg.line", conf.int = FALSE) +
  facet_wrap(~ Soil_type) +
  stat_cor(method = "pearson", aes(label = ..r.label..), label.x = 3, label.y = 450) +
  stat_regline_equation(label.x = 3,label.y = 530) + 
  xlab("Zn in shoot"~ "(mg kg"^-1*")") + ylab("Cd in shoot"~ "(mg kg"^-1*")") +
  guides(color = FALSE)   
dev.off()

# formula = y ~ 0 + x

pdf("Plots/PlantChem/Cd_Zn_Corr_all_POP_07052022.pdf",
    width = 8, height = 5)
ggscatter(plant, x = "Zn_plant", y = "Cd_plant",
          color = "Population_type", shape =  "Soil_type", 
          add = "reg.line", conf.int = FALSE) +
  scale_colour_manual(values = c("blue", "#66B2FF", "red", "#FF66B2"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  stat_cor(method = "pearson", aes(color = Population_type, label = ..r.label..),
           label.x = c(18000,28000,16500,24000), label.y = c(200, 300, 400, 390)) +
  stat_regline_equation(aes(color = Population_type), 
                        label.x = c(18000,28000,16500,24000), label.y = c(220, 320, 420, 410)) +
  xlab("Zn in shoot"~ "(mg kg"^-1*")") + ylab("Cd in shoot"~ "(mg kg"^-1*")") +
  theme(legend.position = "right")
dev.off()

pdf("Plots/PlantChem/Cd_Zn_Corr_all_Soil_07052022.pdf",
    width = 8, height = 5)
ggscatter(plant, x = "Zn_plant", y = "Cd_plant",
          color = "Soil_type", shape =  "Population_type", 
          add = "reg.line", conf.int = FALSE) +
  stat_cor(method = "pearson", aes(color = Soil_type, label = ..r.label..),
           label.x = c(9000,10000,17000,5000), label.y = c(110, 30, 220, 260)) +
  stat_regline_equation(aes(color = Soil_type), 
                        label.x = c(9000,10000,17000,5000), label.y = c(130, 50, 240, 280)) +
  xlab("Zn in shoot"~ "(mg kg"^-1*")") + ylab("Cd in shoot"~ "(mg kg"^-1*")") +
  theme(legend.position = "right")
dev.off()


