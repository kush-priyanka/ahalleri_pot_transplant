# Install packages if not installed already
install.packages("ggplot2")
install.packages("gridExtra")

# Load libraries
library(ggplot2)
library(gridExtra) #multiple panels in 1 figure

# Import Data
rte3 <- read.table("Data/Ahalleri_soil_plant_02172022.txt", 
                   header=T, row.names=1, sep="\t")

rte3 <- rte3[1:120,c(1:20,23:24,26:48)]

#### Soil Outliers Graph ####
#### Soil Population_type ####
colnames<- names(rte3)[7:20]
soil_plots <- list()
soil_plots <- lapply(7:20, function(i){
  ggplot(rte3, aes(x = Soil_type,y = rte3[,i])) +
    geom_boxplot() +
    ylab(colnames[i-6]) + 
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
})

## Display plot
grid.arrange(grobs = soil_plots, ncol = 4) 

# Save Soil plots plot
ggsave(file = "Plots/SoilChem/Soil_outliers/Soil_outliers_03012022.pdf", 
       arrangeGrob(grobs = soil_plots, ncol = 4),
       width = 10,
       height = 8,
       units ="in")

#### Soil Ecotype ####
ecotype<- list()
ecotype <- lapply(7:20, function(i){
  ggplot(rte3, aes(x = Ecotype,y = rte3[,i])) +
    geom_boxplot() +
    ylab(colnames[i-6]) + 
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

## Display Ecotype plot
grid.arrange(grobs = ecotype, ncol = 4) 

## Save Soil plots plot
ggsave(file = "Plots/SoilChem/Soil_outliers/Soil_ecotype_03012022.pdf", 
       arrangeGrob(grobs = ecotype, ncol = 4),
       width = 10,
       height = 8,
       units ="in")

#### Plant Outliers Graph ####

# Subset data with plants only pots
plant <- subset(rte3, rte3$Population != "Blank")

#### Function for RGR ####
# Calculate number of days between t0, t1, t2
survey <- data.frame(date=c("2018/09/28", "2018/11/16"),
                     tx_start=c("2018/05/22","2018/09/28"))

survey$date_diff <- as.Date(as.character(survey$date), format="%Y/%m/%d")-
  as.Date(as.character(survey$tx_start), format="%Y/%m/%d")

survey
d1 <- 129
d2 <- 49
d3 <- d1 + d2

# RGR between T0 & T1
rel_growth_rate1 <- function(ela0, ela1, d) {
  rgr1 <- ((log(ela1)-log(ela0))/d)
  return(rgr1)
}

# RGR between T1 & T2
rel_growth_rate2 <- function(ela1, ela2, d) {
  rgr2 <- ((log(ela2)-log(ela1))/d)
  return(rgr2)
}

# RGR between T0 & T2
rel_growth_rate3 <- function(ela0, ela2, d) {
  rgr3 <- ((log(ela2)-log(ela0))/d)
  return(rgr3)
}

# Calculate RGRs
plant$RGR1 <- rel_growth_rate1(ela0 = plant$ELA_T0, 
                               ela1 = plant$ELA_T1, 
                               d = d1)

plant$RGR2 <- rel_growth_rate2(ela1 = plant$ELA_T1, 
                               ela2 = plant$ELA_T2, 
                               d = d2)

plant$RGR3 <- rel_growth_rate3(ela0 = plant$ELA_T0, 
                               ela2 = plant$ELA_T2, 
                               d = d3)

plant <- plant[, c(1:22,26:27, 23:25,46:48,40:45, 28:39)]

#### Plant Population_type ####
colnames_plant <- names(plant)[21:36]
plant_plots <- list()
plant_plots <- lapply(21:36, function(i){
  ggplot(plant, aes(x = Soil_type,y = plant[,i])) +
    geom_boxplot() +
    ylab(colnames_plant[i-20]) + 
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

## Display plot
grid.arrange(grobs = plant_plots, ncol = 4) 

# Save Soil plots plot
ggsave(file = "Plots/PlantChem/Plant_outliers/Plant_outliers_03012022.pdf", 
       arrangeGrob(grobs = plant_plots, ncol = 4),
       width = 10,
       height = 8,
       units ="in")

#### Plant Ecotype ####
p.ecotype<- list()
p.ecotype <- lapply(21:36, function(i){
  ggplot(plant, aes(x = Ecotype,y = plant[,i])) +
    geom_boxplot() +
    ylab(colnames_plant[i-20]) + 
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

## Display plant Ecotype plot
grid.arrange(grobs = p.ecotype, ncol = 4) 

## Save Soil plots plot
ggsave(file = "Plots/PlantChem/Plant_outliers/Plant_ecotype_03012022.pdf", 
       arrangeGrob(grobs = p.ecotype, ncol = 4),
       width = 10,
       height = 8,
       units ="in")
