# Load libraries
library(ggplot2)
library(cowplot)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

#### Subset raw soil data ####
plant <- chem[, c(1:7, 24, 25, 23,34)]
plant <- subset(plant, plant$Population != "Blank")

plant$Soil_type <-factor(plant$Soil_type,
                                 levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

plant$Population_type <-factor(plant$Population_type,
                                       levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))

names <- c("Cd shoot (mg/kg-1)","Zn shoot (mg kg-1)", "Dry shoot biomass (g)", "Fv/Fm")

plant_plots <- list()
plant_plots <- lapply(8:11, function(i){
  ggplot(plant, aes(x = Soil_type,
                            y = plant[,i], 
                            fill = Population_type)) +
    geom_boxplot(linetype = "solid", lwd = 0.0000005, outlier.size = 0.2) +
    stat_summary(fun.y = mean, geom = "point", shape = 22, size = 1, color = "black",
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

cd <- plant_plots[[1]] +
  geom_hline(yintercept =  100, size = 0.4, 
             linetype = "dashed", color = "#606060")

zn <- plant_plots[[2]] +
  geom_hline(yintercept =  3000, size = 0.4, 
             linetype = "dashed", color = "#606060")

fig2 <- plot_grid(cd, zn, plant_plots[[3]], plant_plots[[4]],
                ncol = 2, align = "v")

#### Save the plant plot ####
ggsave(file = "Plant_Properties_08152022.pdf", 
       plot = fig2,
       width = 174,
       height = 100,
       units ="mm")
