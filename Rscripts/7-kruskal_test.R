# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)

## Import raw soil + plant data
chem <- read.table("Data/Ahalleri_soil_plant_02172022.txt", 
                   header=T, row.names=1, sep="\t")

## Subset raw soil data
soil <- chem[, c(1:6, 19, 7:18, 20)]
soil$Soil_type <- factor(soil$Soil_type,
                              levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

## Import Normalized Data
soil_norm <- read.table("Data/Soil_bestNormalize_02232022.txt", 
                   header=T, row.names=1, sep="\t")

## Add new column with order of Soil types
soil_norm$Soil_num <-with(soil_norm, ifelse(soil_norm$Soil_type == "NM_PL14_S", 'Soil1', 
                                      ifelse(soil_norm$Soil_type == "NM_PL35_S", 'Soil2',
                                             ifelse(soil_norm$Soil_type == "M_PL22_S", 'Soil3','Soil4'))))
                                              
#soil_woblanks <- subset(soil, soil$Population != "Blank")

#### Matrix for sig letters Soil ####
kw.matrix <- matrix(data = NA, nrow = 4, ncol = 14)
rownames(kw.matrix) = c(1:4)
colnames(kw.matrix) = colnames(soil_norm[,7:20])

for(i in 7:20)
{
  bla <- kruskal(soil_norm[,i], soil_norm$Soil_num, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  kw.matrix[, i - 6] <- as.character(bla.group.ordered[,2])
  print(bla.group.ordered)
}

#### Plot soil variables #### 
## with sig letters
pdf("Plots/SoilChem/Soil_soil_type_KW_03012022.pdf", width = 15, height = 10)
par(mfrow=c(4,4))
par(mar = c(3, 5, 1, 1), oma = c( 0, 0, 0, 0)) 
ylabels <- colnames(soil[,7:20])
#xlabels <- c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S") 

for (i in 7:20) 
{
  boxplot(soil[,i]~ Soil_type, data = soil,xlab = NULL, ylab = ylabels[i-6], 
          ylim = c(min(soil[,i], na.rm=T), 
                   (max(soil[,i], na.rm=T) + (max(soil[,i], na.rm=T))*0.03)), 
          cex.lab=1.5, cex.axis=1.2)
  text(x = c(1:4), y = rep(max(soil[,i], na.rm=T) + max(soil[,i], na.rm=T)*0.03,4), 
       label = as.character(kw.matrix[,i-6]), cex=1.5)
}
dev.off()

#### Import plant datasets ####
## Subset raw soil data
plant <- subset(chem, chem$Population != "Blank")
plant[is.na(plant)] <- 0
plant <- plant[, c(1:20,23:24,29:30, 26:28, 49:51, 43: 48, 31:42)]
plant$Soil_type <- factor(plant$Soil_type,
                         levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

## Import Normalized Data
plant_norm <- read.table("Data/Plant_bestNormalize_02232022.txt", 
                         header=T, row.names=1, sep="\t")


## Add new column with order of Soil types
plant_norm$Soil_num <-with(plant_norm, ifelse(plant_norm$Soil_type == "NM_PL14_S", 'Soil1', 
                                            ifelse(plant_norm$Soil_type == "NM_PL35_S", 'Soil2',
                                                   ifelse(plant_norm$Soil_type == "M_PL22_S", 'Soil3','Soil4'))))

#### Matrix for sig letters Plant ####
kw.matrix1 <- matrix(data = NA, nrow = 4, ncol = 16)
rownames(kw.matrix1) = c(1:4)
colnames(kw.matrix1) = colnames(plant_norm[,7:22])

for(i in 7:22)
{
  bla <- kruskal(plant_norm[,i], plant_norm$Soil_num, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  kw.matrix1[, i - 6] <- as.character(bla.group.ordered[,2])
  print(bla.group.ordered)
}

#### Plot plant variables #### 
## with sig letters
pdf("Plots/PlantChem/Plant_soil_type_KW_03012022.pdf", width = 15, height = 10)
par(mfrow=c(4,4))
par(mar = c(3, 5, 1, 1), oma = c( 0, 0, 0, 0)) 
ylabels <- colnames(plant[,21:36])

for (i in 21:36) 
{
  boxplot(plant[,i]~ Soil_type, data = plant,xlab = NULL, ylab = ylabels[i-20], 
          ylim = c(min(plant[,i], na.rm=T), 
                   (max(plant[,i], na.rm=T) + (max(plant[,i], na.rm=T))*0.03)), 
          cex.lab=1.5, cex.axis=1.2)
  text(x = c(1:4), y = rep(max(plant[,i], na.rm=T) + max(plant[,i], na.rm=T)*0.03,4), 
       label = as.character(kw.matrix1[,i-20]), cex=1.5)
}
dev.off()

### Plot plant variables using ggplot 

colnames_plant <- names(plant)[21:36]
plant_plots <- list()
plant_plots <- lapply(21:36, function(i){
  ggplot(plant, aes(x = Soil_type,y = plant[,i])) +
    geom_boxplot() +
    ylab(colnames_plant[i-20]) + 
    annotate( "text", x = 1, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-20][1])+
    annotate( "text", x = 2, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-20][2])+
    annotate( "text", x = 3, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-20][3])+
    annotate( "text", x = 4, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-20][3])+
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

grid.arrange(grobs = plant_plots, ncol = 4)

# Save Soil plots plot
ggsave(file = "Plots/PlantChem/Plant_soil_type_KW_ggplot_03012022.pdf", 
       arrangeGrob(grobs = plant_plots, ncol = 4),
       width = 10,
       height = 10,
       units ="in")
