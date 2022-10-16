## Plot plant growth parameters over time
## Load libraries
library(ggplot2)
library(dplyr)

## Import Clean Data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

dim(chem) #120 49

## Subset only plant data
plant <- subset(chem, chem$Population != "Blank")
# plant[is.na(plant)] <- 0

#### Calculate average values ####
## Avg of ELA, FvFm, PI_abs at soil type for each population sing for loop
pop <- unique(plant$Population_type)
dat1 <- data.frame()
dat2 <- data.frame()
dat3 <- data.frame()

for(p in pop) {
 plant_growth1 <- plant %>%
    filter(Population_type == p) %>%
    group_by(Soil_type) %>%
    summarize(ELA_avg = mean(ELA_T0),
              Fv_Fm_avg = mean(Fv_Fm_T0),
              PI_abs_avg = mean(PI_abs_T0),
              ELA_sd = sd(ELA_T0),
              Fv_Fm_sd = sd(Fv_Fm_T0),
              PI_abs_sd = sd(PI_abs_T0),
              ELA_se = sd(ELA_T0)/sqrt(n()),
              Fv_Fm_se = sd(Fv_Fm_T0)/sqrt(n()),
              PI_abs_se = sd(PI_abs_T0)/sqrt(n()))
 
 df1 <- data.frame(plant_growth1)
 dat1 <- rbind(dat1, df1)
 plant_growth2 <- plant %>%
   filter(Population_type == p) %>%
   group_by(Soil_type) %>%
   summarize(ELA_avg = mean(ELA_T1),
             Fv_Fm_avg = mean(Fv_Fm_T1),
             PI_abs_avg = mean(PI_abs_T1),
             ELA_sd = sd(ELA_T1),
             Fv_Fm_sd = sd(Fv_Fm_T1),
             PI_abs_sd = sd(PI_abs_T1),
             ELA_se = sd(ELA_T1)/sqrt(n()),
             Fv_Fm_se = sd(Fv_Fm_T1)/sqrt(n()),
             PI_abs_se = sd(PI_abs_T1)/sqrt(n()))
 df2 <- data.frame(plant_growth2)
 dat2 <- rbind(dat2, df2)
 plant_growth3 <- plant %>%
   filter(Population_type == p) %>%
   group_by(Soil_type) %>%
   summarize(ELA_avg = mean(ELA_T2),
             Fv_Fm_avg = mean(Fv_Fm_T2),
             PI_abs_avg = mean(PI_abs_T2),
             ELA_sd = sd(ELA_T2),
             Fv_Fm_sd = sd(Fv_Fm_T2),
             PI_abs_sd = sd(PI_abs_T2),
             ELA_se = sd(ELA_T2)/sqrt(n()),
             Fv_Fm_se = sd(Fv_Fm_T2)/sqrt(n()),
             PI_abs_se = sd(PI_abs_T2)/sqrt(n()))
 df3 <- data.frame(plant_growth3)
 dat3 <- rbind(dat3, df3)
}

## Combine the 3 datasets, add days, and population_type
plant.growth_time <- rbind(dat1, dat2)
plant.growth_time <- rbind(plant.growth_time, dat3)
plant.growth_time$days <- as.numeric(rep(c(1,129,178), 
                                         each = 16))
plant.growth_time$Population_type <- as.character(rep(c("M_PL22_P", 
                                                        "M_PL27_P", 
                                                        "NM_PL14_P", 
                                                        "NM_PL35_P"), 
                                                      each = 4))

plant.growth_time$Population_type <- factor(plant.growth_time$Population_type,
                                            levels = c("NM_PL14_P", 
                                                       "NM_PL35_P", 
                                                       "M_PL22_P", 
                                                       "M_PL27_P"))

plant.growth_time$Soil_type <- factor(plant.growth_time$Soil_type, 
                                      levels = c("NM_PL14_S", "NM_PL35_S",
                                                 "M_PL22_S","M_PL27_S"))

## Assign column names to an object
colnames<- names(plant.growth_time)[2:4]
#### Plot ELA, FvFm, PI_abs over days with standard errors and Save files ####
for(i in 2:4) {
    fig_growth <- ggplot(plant.growth_time, 
                         aes(x = days, 
                            y = plant.growth_time[, i])) +
    geom_point(aes(color =  Soil_type), size = 1.5) +
    geom_line(aes(color = Soil_type), size = 1) +
    facet_wrap(~Population_type) + 
    geom_errorbar(aes(x = days, 
                      ymin = plant.growth_time[, i]- plant.growth_time[, i + 6], 
                     ymax = plant.growth_time[, i] + plant.growth_time[, i + 6]),
                      width = 0.2) +
    scale_color_manual(values = c("NM_PL14_S"="#2B3990",
                                  "NM_PL35_S"="#5686C5",
                                  "M_PL22_S" = "#E23E26",
                                  "M_PL27_S" ="#F58776")) +
    ylab(colnames[i-1]) +
    theme_bw() + 
    ggtitle(colnames[i-1])+
    theme(legend.position="bottom")

  ggsave(filename = paste0("Plots/PlantChem/plant_growth/PlantGrowthRate_StdError", colnames[i-1], "_04062022.pdf"), 
         plot = fig_growth,
         width = 10,
         height = 6,
         units ="in")
}

