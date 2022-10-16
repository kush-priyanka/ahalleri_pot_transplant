# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)
library(cowplot)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

#### Subset raw soil data ####
soil <- chem[, c(1:7, 16, 13:15,17:18,9:10, 12,11,50)]
soil_woblanks <- subset(soil, soil$Population != "Blank")

soil_woblanks$Soil_type <-factor(soil_woblanks$Soil_type,
                                 levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

soil_woblanks$Population_type <-factor(soil_woblanks$Population_type,
                                       levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))

### Plot soil variables using ggplot 
names <- c("pH", "Cd (mg kg-1)", "Pb (mg kg-1)", "Zn (mg kg-1)",
           "N-NH4 (mg kg-1)" ,"N-NO3 (mg kg-1)", "Acid phosphatase \n(mg NP g-1 dm h-1)",     
           "Alkaline phosphatase \n(mg NP g-1 dm h-1)", "Arylsulfatase", "B_glucosidase \n(mg sal g-1 dm 1h-1)") 

soil_plots <- list()
soil_plots <- lapply(8:17, function(i){
  ggplot(soil_woblanks, aes(x = Soil_type,
                            y = soil_woblanks[,i], 
                            fill = Population_type)) +
    geom_boxplot(linetype = "solid", lwd = 0.0000005, outlier.size = 0.2) +
    stat_summary(fun.y = mean, geom ="point", shape = 22, size = 1, color = "black",
                 position = position_dodge2(width = 0.75,   
                                            preserve = "single")) +
    xlab("") +
    ylab(names[i-7]) + 
    
    scale_fill_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
    scale_x_discrete(labels = c("NM_PL14_S" = "NM_PL14_Soil",
                                "NM_PL35_S" = "NM_PL35_Soil",
                                "M_PL22_S" = "M_PL22_Soil",
                                "M_PL27_S" = "M_PL27_Soil")) +
    theme_classic() +
    theme(axis.text = element_text(size = 6,
                                   color = "black"),
          axis.title.y = element_text(size = 9),
          legend.position = "none") 
})

x1 <- plot_grid(soil_plots[[1]], soil_plots[[2]], soil_plots[[3]],
               soil_plots[[4]],soil_plots[[5]],soil_plots[[6]],
               ncol = 2, align = "v")

x2 <- plot_grid(soil_plots[[7]],soil_plots[[8]],soil_plots[[9]],soil_plots[[10]],
                ncol = 2, align = "v")

#### Save the plant plot ####
ggsave(file = "Soil_Properties_A_08152022.pdf", 
       plot = x1,
       width = 174,
       height = 127,
       units ="mm")

ggsave(file = "Soil_Properties_B_08152022.pdf", 
       plot = x2,
       width = 174,
       height = 85,
       units ="mm")
