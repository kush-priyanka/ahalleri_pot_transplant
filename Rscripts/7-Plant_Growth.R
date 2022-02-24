# Load libraries
library(ggplot2)
library(dplyr)

# Import Raw Data
chem <- read.table("Data/Ahalleri_soil_plant_02172022.txt", 
                   header=T, row.names=1, sep="\t")

chem <- chem[c(1:120),c(1:20,23:48)]
dim(chem) #120 46

# Subset only plant data
plant <- subset(chem, chem$Population != "Blank")

#### Plot ELA, FvFm, PI_abs over days ####
# NM_PL14_P Calculate mean for ELA, FvFm, PI_abs
pl14.growth1 <- plant %>% 
  filter(Population_type == "NM_PL14_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T0),
            Fv_Fm_value = mean(Fv_Fm_T0),
                      PI_abs_value = mean(PI_abs_T0))

pl14.growth2 <- plant %>% 
  filter(Population_type == "NM_PL14_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T1),
            Fv_Fm_value = mean(Fv_Fm_T1),
            PI_abs_value = mean(PI_abs_T1))

pl14.growth3 <- plant %>% 
  filter(Population_type == "NM_PL14_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T2),
            Fv_Fm_value = mean(Fv_Fm_T2),
            PI_abs_value = mean(PI_abs_T2))

# NM_PL35_P Calculate mean for ELA, FvFm, PI_abs
pl35.growth1 <- plant %>% 
  filter(Population_type == "NM_PL35_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T0),
            Fv_Fm_value = mean(Fv_Fm_T0),
            PI_abs_value = mean(PI_abs_T0))

pl35.growth2 <- plant %>% 
  filter(Population_type == "NM_PL35_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T1),
            Fv_Fm_value = mean(Fv_Fm_T1),
            PI_abs_value = mean(PI_abs_T1))

pl35.growth3 <- plant %>% 
  filter(Population_type == "NM_PL35_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T2),
            Fv_Fm_value = mean(Fv_Fm_T2),
            PI_abs_value = mean(PI_abs_T2))

# M_PL22_P Calculate mean for ELA, FvFm, PI_abs
pl22.growth1 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T0),
            Fv_Fm_value = mean(Fv_Fm_T0),
            PI_abs_value = mean(PI_abs_T0))

pl22.growth2 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T1),
            Fv_Fm_value = mean(Fv_Fm_T1),
            PI_abs_value = mean(PI_abs_T1))

pl22.growth3 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T2),
            Fv_Fm_value = mean(Fv_Fm_T2),
            PI_abs_value = mean(PI_abs_T2))

# M_PL27_P Calculate mean for ELA, FvFm, PI_abs
pl27.growth1 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T0),
            Fv_Fm_value = mean(Fv_Fm_T0),
            PI_abs_value = mean(PI_abs_T0))

pl27.growth2 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T1),
            Fv_Fm_value = mean(Fv_Fm_T1),
            PI_abs_value = mean(PI_abs_T1))

pl27.growth3 <- plant %>% 
  filter(Population_type == "M_PL22_P") %>%
  group_by(Soil_type) %>%
  summarize(ELA_value = mean(ELA_T2),
            Fv_Fm_value = mean(Fv_Fm_T2),
            PI_abs_value = mean(PI_abs_T2))

# NM_PL14_P Add Days and Population type
pl14.growth_time <- rbind(pl14.growth1, pl14.growth2)
pl14.growth_time <- rbind(pl14.growth_time, pl14.growth3)
pl14.growth_time$days <- as.numeric(rep(c(1,129,178), each = 4))
pl14.growth_time$Population <- as.character(rep(c("NM_PL14_P"),each = 12))

# NM_PL35_P Add Days and Population type
pl35.growth_time <- rbind(pl35.growth1, pl35.growth2)
pl35.growth_time <- rbind(pl35.growth_time, pl35.growth3)
pl35.growth_time$days <- as.numeric(rep(c(1,129,178), each = 4))
pl35.growth_time$Population <- as.character(rep(c("NM_PL35_P"),each = 12))

# M_PL22_P Add Days and Population type
pl22.growth_time <- rbind(pl22.growth1, pl22.growth2)
pl22.growth_time <- rbind(pl22.growth_time, pl22.growth3)
pl22.growth_time$days <- as.numeric(rep(c(1,129,178), each = 4))
pl22.growth_time$Population <- as.character(rep(c("M_PL22_P"),each = 12))

# M_PL27_P Add Days and Population type
pl27.growth_time <- rbind(pl27.growth1, pl27.growth2)
pl27.growth_time <- rbind(pl27.growth_time, pl27.growth3)
pl27.growth_time$days <- as.numeric(rep(c(1,129,178), each = 4))
pl27.growth_time$Population <- as.character(rep(c("M_PL27_P"),each = 12))

# Combine all four population type
all_growth_time <- rbind(pl14.growth_time, pl35.growth_time)
all_growth_time <- rbind(all_growth_time, pl22.growth_time)
all_growth_time <- rbind(all_growth_time, pl27.growth_time)

# Order Soil_type and Population_type
all_growth_time$Soil_type <- factor(all_growth_time$Soil_type, 
                                 levels = c("NM_PL14_S", "NM_PL35_S","M_PL22_S","M_PL27_S"))
all_growth_time$Population <- factor(all_growth_time$Population, 
                                  levels = c("NM_PL14_P", "NM_PL35_P","M_PL22_P","M_PL27_P"))

# Plot leaf area over days
pdf("Plots/PlantChem/PlantGrowthRate_ELA_02232022.pdf", width= 10, height= 6)
ggplot(all_growth_time, aes(x = days, y = ELA_value)) +
  geom_point(aes(color =  Soil_type), size = 1.5) +
  geom_line(aes(color = Soil_type), size = 1) +
  scale_color_manual(values = c("NM_PL14_S"="#2B3990",
                                "NM_PL35_S"="#5686C5",
                                "M_PL22_S" = "#E23E26",
                                "M_PL27_S" ="#F58776")) +
  facet_wrap(~Population) + 
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

# Plot FV_Fm over days
pdf("Plots/PlantChem/PlantGrowthRate_FvFm_02232022.pdf", width= 10, height= 6)
ggplot(all_growth_time, aes(x = days, y = Fv_Fm_value)) +
  geom_point(aes(color =  Soil_type), size = 1.5) +
  geom_line(aes(color = Soil_type), size = 1) +
  scale_color_manual(values = c("NM_PL14_S"="#2B3990",
                                "NM_PL35_S"="#5686C5",
                                "M_PL22_S" = "#E23E26",
                                "M_PL27_S" ="#F58776")) +
  facet_wrap(~Population) + 
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

# Plot PI_abs over days
pdf("Plots/PlantChem/PlantGrowthRate_PI_abs_02232022.pdf", width= 10, height= 6)
ggplot(all_growth_time, aes(x = days, y = PI_abs_value)) +
  geom_point(aes(color =  Soil_type), size = 1.5) +
  geom_line(aes(color = Soil_type), size = 1) +
  scale_color_manual(values = c("NM_PL14_S"="#2B3990",
                                "NM_PL35_S"="#5686C5",
                                "M_PL22_S" = "#E23E26",
                                "M_PL27_S" ="#F58776")) +
  facet_wrap(~Population) + 
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

