## Load libraries
library(ggplot2)
library(dplyr)

## Import Raw Data
chem <- read.table("Data/Ahalleri_soil_plant_02172022.txt", 
                   header=T, row.names=1, sep="\t")

chem <- chem[c(1:120),c(1:20,23:48)]
dim(chem) #120 46

## Subset only plant data
plant <- subset(chem, chem$Population != "Blank")

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
    summarize(ELA_value = mean(ELA_T0),
              Fv_Fm_value = mean(Fv_Fm_T0),
              PI_abs_value = mean(PI_abs_T0))
 df1 <- data.frame(plant_growth1)
 dat1 <- rbind(dat1, df1)
 plant_growth2 <- plant %>%
   filter(Population_type == p) %>%
   group_by(Soil_type) %>%
   summarize(ELA_value = mean(ELA_T1),
             Fv_Fm_value = mean(Fv_Fm_T1),
             PI_abs_value = mean(PI_abs_T1))
 df2 <- data.frame(plant_growth2)
 dat2 <- rbind(dat2, df2)
 plant_growth3 <- plant %>%
   filter(Population_type == p) %>%
   group_by(Soil_type) %>%
   summarize(ELA_value = mean(ELA_T2),
             Fv_Fm_value = mean(Fv_Fm_T2),
             PI_abs_value = mean(PI_abs_T2))
 df3 <- data.frame(plant_growth3)
 dat3 <- rbind(dat3, df3)
}

## Combine the 3 datasets, add days, and population_type
plant.growth_time <- rbind(dat1, dat2)
plant.growth_time <- rbind(plant.growth_time, dat3)
plant.growth_time$days <- as.numeric(rep(c(1,129,178), each = 16))
plant.growth_time$Population_type <- as.character(rep(c("M_PL22_P", "M_PL27_P", 
                                                        "NM_PL14_P", "NM_PL35_P"), each = 4))
## Assign column names to an object
colnames<- names(plant.growth_time)[2:4]

#### Plot ELA, FvFm, PI_abs over days and Save files ####

for(i in 2:4) {
    fig_growth <- ggplot(plant.growth_time, aes(x = days, y = plant.growth_time[, i])) +
    geom_point(aes(color =  Soil_type), size = 1.5) +
    geom_line(aes(color = Soil_type), size = 1) +
    scale_color_manual(values = c("NM_PL14_S"="#2B3990",
                                  "NM_PL35_S"="#5686C5",
                                  "M_PL22_S" = "#E23E26",
                                  "M_PL27_S" ="#F58776")) +
    facet_wrap(~Population_type) + 
    ylab(colnames[i-1])+
    theme_bw() + 
    ggtitle(colnames[i-1])+
    theme(legend.position="bottom")

  ggsave(filename = paste0("Plots/PlantChem/PlantGrowthRate_", colnames[i-1], "_02242022.pdf"), 
         plot = fig_growth,
         width = 10,
         height = 6,
         units ="in")
}


