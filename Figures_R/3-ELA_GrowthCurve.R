## Load libraries
library(ggplot2)
library(dplyr)
library(agricolae)

chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

dim(chem) #120 49

## Subset only plant data
plant <- subset(chem, chem$Population != "Blank")
# plant[is.na(plant)] <- 0

#### Calculate average + stdev values ####
pop <- unique(plant$Population_type)
dat1 <- data.frame()
dat2 <- data.frame()
dat3 <- data.frame()

for(p in pop) {
plant_growth1 <- plant %>%
  filter(Population_type == p) %>%
  group_by(Soil_type) %>%
  summarize(ELA_avg = mean(ELA_T0),
            ELA_sd = sd(ELA_T0),
            ELA_se = sd(ELA_T0)/sqrt(n()),)

df1 <- data.frame(plant_growth1)
dat1 <- rbind(dat1, df1)

plant_growth2 <- plant %>%
  filter(Population_type == p) %>%
  group_by(Soil_type) %>%
  summarize(ELA_avg = mean(ELA_T1),
            ELA_sd = sd(ELA_T1),
            ELA_se = sd(ELA_T1)/sqrt(n()),)

df2 <- data.frame(plant_growth2)
dat2 <- rbind(dat2, df2)

plant_growth3 <- plant %>%
  filter(Population_type == p) %>%
  group_by(Soil_type) %>%
  summarize(ELA_avg = mean(ELA_T2),
            ELA_sd = sd(ELA_T2),
            ELA_se = sd(ELA_T2)/sqrt(n()),)

df3 <- data.frame(plant_growth3)
dat3 <- rbind(dat3, df3)
}

## Combine the 3 datasets, add days, and population_type
plant.growth_time <- rbind(dat1, dat2)
plant.growth_time <- rbind(plant.growth_time, dat3)
plant.growth_time$days <- as.numeric(rep(c(1,129,178), 
                                         each = 16))
plant.growth_time$Population_type <- as.character(rep(c("M_PL22_Population", 
                                                        "M_PL27_Population", 
                                                        "NM_PL14_Population", 
                                                        "NM_PL35_Population"), 
                                                      each = 4))

plant.growth_time$Population_type <- factor(plant.growth_time$Population_type,
                                            levels = c("NM_PL14_Population", 
                                                       "NM_PL35_Population", 
                                                       "M_PL22_Population", 
                                                       "M_PL27_Population"))

plant.growth_time$Soil_type <- factor(plant.growth_time$Soil_type, 
                                      levels = c("NM_PL14_S", "NM_PL35_S",
                                                 "M_PL22_S","M_PL27_S"))

fig3 <- ggplot(plant.growth_time, 
                     aes(x = days, 
                         y = plant.growth_time$ELA_avg,
                         group = Soil_type)) +
  geom_line(color ="#808080", lwd = 0.7) +
  geom_point(aes(shape =  Soil_type), size = 2) +
  scale_shape_manual(values = c(19, 15, 17, 7)) + 
  facet_wrap(~Population_type) + 
  geom_errorbar(aes(x = days, 
                    ymin = plant.growth_time$ELA_avg - plant.growth_time$ELA_se, 
                    ymax = plant.growth_time$ELA_avg + plant.growth_time$ELA_se),
                width = 0.2) +
  xlab("Days of experiment") +
  ylab("Leaf area (cm^2)") +
  theme_classic() + 
  theme(axis.text = element_text(size = 6,
                                 color = "black"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "bottom")

ggsave(file = "LeafArea_Growth_legend_08152022.pdf", 
       plot = fig3,
       width = 114,
       height = 90,
       units ="mm")

### Stats ELA_norm T1 & T2
# plant_norm <- read.table("Data/Plant_bestNormalize_clean_data_04062022.txt", 
#                          header = T, row.names = 1, sep = "\t")
# 
# pl14  <- subset(plant_norm, plant_norm$Population_type == "NM_PL14_P")
# pl35 <- subset(plant_norm, plant_norm$Population_type == "NM_PL35_P")
# pl22 <- subset(plant_norm, plant_norm$Population_type == "M_PL22_P")
# pl27 <- subset(plant_norm, plant_norm$Population_type == "M_PL27_P")
# 
# p14.1 <- kruskal(pl14$ELA_T1, pl14$Soil_type, group = T, p.adj="BH")
# p35.1 <- kruskal(pl35$ELA_T1, pl35$Soil_type, group = T, p.adj="BH")
# p22.1 <- kruskal(pl22$ELA_T1, pl22$Soil_type, group = T, p.adj="BH")
# p27.1 <- kruskal(pl27$ELA_T1, pl27$Soil_type, group = T, p.adj="BH")
# 
# p14.2 <- kruskal(pl14$ELA_T2, pl14$Soil_type, group = T, p.adj="BH")
# p35.2 <- kruskal(pl35$ELA_T2, pl35$Soil_type, group = T, p.adj="BH")
# p22.2 <- kruskal(pl22$ELA_T2, pl22$Soil_type, group = T, p.adj="BH")
# p27.2 <- kruskal(pl27$ELA_T2, pl27$Soil_type, group = T, p.adj="BH")
