# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header=T, row.names=1, sep="\t")

## Subset raw soil data
soil <- chem[, c(1:6, 8:20, 49)]
soil_woblanks <- subset(soil, soil$Population != "Blank")
soil_woblanks$Group <-factor(soil_woblanks$Group,
                             levels = c("NM_PL14_S_PL14_P", "NM_PL14_S_PL35_P", "NM_PL14_S_PL22_P", "NM_PL14_S_PL27_P",
                                        "NM_PL35_S_PL14_P", "NM_PL35_S_PL35_P", "NM_PL35_S_PL22_P", "NM_PL35_S_PL27_P",
                                        "M_PL22_S_PL14_P", "M_PL22_S_PL35_P", "M_PL22_S_PL22_P", "M_PL22_S_PL27_P",
                                        "M_PL27_S_PL14_P", "M_PL27_S_PL35_P", "M_PL27_S_PL22_P", "M_PL27_S_PL27_P"))

## Import Normalized Data
soil_norm <- read.table("Data/Soil_bestNormalize_clean_data_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")
soil_norm <- soil_norm[, c(1:17,19,18)]

soil_norm_woblanks <- subset(soil_norm, soil_norm$Population != "Blank")
soil_norm_woblanks <- cbind(soil_norm_woblanks, soil_woblanks$Group)

colnames(soil_norm_woblanks)[20] <- 'Group'

## Add new column with order of Group
soil_norm_woblanks$Level <- with(soil_norm_woblanks, ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL14_P", 'L1', 
                                                ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL35_P", 'L2',
                                                ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL22_P", 'L3',
                                                ifelse(soil_norm_woblanks$Group == "NM_PL14_S_PL27_P", 'L4',
                                                ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL35_P", 'L6',
                                                ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL22_P", 'L7',
                                                ifelse(soil_norm_woblanks$Group == "NM_PL35_S_PL27_P", 'L8',
                                                ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL14_P", 'L9', 
                                                ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL35_P", 'L10',
                                                ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL22_P", 'L11',
                                                ifelse(soil_norm_woblanks$Group == "M_PL22_S_PL27_P", 'L12',
                                                ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL14_P", 'L13', 
                                                ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL35_P", 'L14',
                                                ifelse(soil_norm_woblanks$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))
                                                    
#### Matrix for sig letters Soil ####
kw.matrix <- matrix(data = NA, nrow = 16, ncol = 13)
rownames(kw.matrix) = c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(kw.matrix) = colnames(soil_norm_woblanks[, 7:19])

for(i in 7:19)
{
  bla <- kruskal(soil_norm_woblanks[,i], soil_norm_woblanks$Level, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  bla.group.ordered <- bla.group.ordered[c(1,9:16,2:8),]
  kw.matrix[, i - 6] <- as.character(bla.group.ordered[,2])
  print(bla.group.ordered)
}

### Plot soil variables using ggplot 
colnames_soil <- names(soil_woblanks)[7:19]
soil_plots <- list()
soil_plots <- lapply(7:19, function(i){
  ggplot(soil_woblanks, aes(x = Group,
                   y = soil_woblanks[,i])) +
    geom_boxplot() +
    ylab(colnames_soil[i-6]) + 
    annotate( "text", x = 1, 
              y = (max(soil[,i] + (max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][1])+
    annotate( "text", x = 2, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][2])+
    annotate( "text", x = 3, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][3])+
    annotate( "text", x = 4, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][4])+
    annotate( "text", x = 5, 
              y = (max(soil[,i] + (max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][5])+
    annotate( "text", x = 6, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][6])+
    annotate( "text", x = 7, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][7])+
    annotate( "text", x = 8, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][8])+
    annotate( "text", x = 9, 
              y = (max(soil[,i] + (max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][9])+
    annotate( "text", x = 10, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][10])+
    annotate( "text", x = 11, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][11])+
    annotate( "text", x = 12, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][12])+
    annotate( "text", x = 13, 
              y = (max(soil[,i] + (max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][13])+
    annotate( "text", x = 14, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][14])+
    annotate( "text", x = 15, 
              y = (max(soil[,i] +(max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][15])+
    annotate( "text", x = 16, 
              y = (max(soil[,i] + (max(soil[,i]))*0.03)), 
              label = kw.matrix[,i-6][16])+
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

pdf("Plots/SoilChem/Soil_16Group_KW_clean_data1_ggplot_04062022.pdf", width = 8, height = 8)
grid.arrange(soil_plots[[1]],soil_plots[[2]],soil_plots[[3]], ncol = 1)
dev.off()

pdf("Plots/SoilChem/Soil_16Group_KW_clean_data2_ggplot_04062022.pdf", width = 8, height = 8)
grid.arrange(soil_plots[[4]],soil_plots[[5]],soil_plots[[6]], ncol = 1)
dev.off()

pdf("Plots/SoilChem/Soil_16Group_KW_clean_data3_ggplot_04062022.pdf", width = 8, height = 8)
grid.arrange(soil_plots[[7]],soil_plots[[8]],soil_plots[[9]], ncol = 1)
dev.off()

pdf("Plots/SoilChem/Soil_16Group_KW_clean_data4_ggplot_04062022.pdf", width = 8, height = 9)
grid.arrange(soil_plots[[10]],soil_plots[[11]],soil_plots[[12]],soil_plots[[13]], ncol = 1)
dev.off()


#### Import plant datasets ####
## Subset raw soil data
plant <- subset(chem, chem$Population != "Blank")
plant <- plant[, c(1:6,21:36,49)]

plant$Group <-factor(plant$Group,
                             levels = c("NM_PL14_S_PL14_P", "NM_PL14_S_PL35_P", "NM_PL14_S_PL22_P", "NM_PL14_S_PL27_P",
                                        "NM_PL35_S_PL14_P", "NM_PL35_S_PL35_P", "NM_PL35_S_PL22_P", "NM_PL35_S_PL27_P",
                                        "M_PL22_S_PL14_P", "M_PL22_S_PL35_P", "M_PL22_S_PL22_P", "M_PL22_S_PL27_P",
                                        "M_PL27_S_PL14_P", "M_PL27_S_PL35_P", "M_PL27_S_PL22_P", "M_PL27_S_PL27_P"))

## Import Normalized Data
plant_norm <- read.table("Data/Plant_bestNormalize_clean_data_04062022.txt", 
                         header = T, row.names = 1, sep = "\t")
plant_norm <- plant_norm[, c(1:22)]

plant_norm <- cbind(plant_norm, plant$Group)
colnames(plant_norm)[23] <- 'Group'

## Add new column with order of Group
plant_norm$Level <- with(plant_norm, ifelse(plant_norm$Group == "NM_PL14_S_PL14_P", 'L1', 
                                             ifelse(plant_norm$Group == "NM_PL14_S_PL35_P", 'L2',
                                                    ifelse(plant_norm$Group == "NM_PL14_S_PL22_P", 'L3',
                                                           ifelse(plant_norm$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                  ifelse(plant_norm$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                         ifelse(plant_norm$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                                ifelse(plant_norm$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                       ifelse(plant_norm$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                              ifelse(plant_norm$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                     ifelse(plant_norm$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                            ifelse(plant_norm$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                   ifelse(plant_norm$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                          ifelse(plant_norm$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                 ifelse(plant_norm$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                        ifelse(plant_norm$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))

#### Matrix for sig letters Plant: Soill_type ####
kw.matrix1 <- matrix(data = NA, nrow = 16, ncol = 16)
rownames(kw.matrix1) = c('L1', 'L2', 'L3','L4',
                         'L5','L6', 'L7', 'L8', 
                         'L9', 'L10', 'L11', 'L12', 
                         'L13', 'L14','L15', 'L16')
colnames(kw.matrix1) = colnames(plant_norm[,7:22])

for(i in 7:22)
{
  bla1 <- kruskal(plant_norm[,i], plant_norm$Level, group = T, p.adj="BH")
  bla.groups1 <- bla1$groups
  bla.group.ordered1 <- bla.groups1[order(as.character(row.names(bla.groups1))),]
  bla.group.ordered1 <- bla.group.ordered1[c(1,9:16,2:8),]
  kw.matrix1[, i - 6] <- as.character(bla.group.ordered1[,2])
  print(bla.group.ordered1)
}

### Plot plant variables using ggplot 
colnames_plant <- names(plant)[7:22]
plant_plots <- list()
plant_plots <- lapply(7:22, function(i){
  ggplot(plant, aes(x = Group,
                    y = plant[,i])) +
    geom_boxplot() +
    ylab(colnames_plant[i-6]) + 
    annotate( "text", x = 1, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][1])+
    annotate( "text", x = 2, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][2])+
    annotate( "text", x = 3, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][3])+
    annotate( "text", x = 4, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][4])+
    annotate( "text", x = 5, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][5])+
    annotate( "text", x = 6, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][6])+
    annotate( "text", x = 7, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][7])+
    annotate( "text", x = 8, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][8])+
    annotate( "text", x = 9, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][9])+
    annotate( "text", x = 10, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][10])+
    annotate( "text", x = 11, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][11])+
    annotate( "text", x = 12, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][12])+
    annotate( "text", x = 13, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][13])+
    annotate( "text", x = 14, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][14])+
    annotate( "text", x = 15, 
              y = (max(plant[,i] +(max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][15])+
    annotate( "text", x = 16, 
              y = (max(plant[,i] + (max(plant[,i]))*0.03)), 
              label = kw.matrix1[,i-6][16])+
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

pdf("Plots/PlantChem/Plant_16Group_KW_clean_data1_ggplot_04062022.pdf", width = 12, height = 8)
grid.arrange(plant_plots[[1]],plant_plots[[2]],plant_plots[[3]],plant_plots[[4]], ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_16Group_KW_clean_data2_ggplot_04062022.pdf", width = 12, height = 8)
grid.arrange(plant_plots[[5]],plant_plots[[6]],plant_plots[[7]],plant_plots[[8]], ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_16Group_KW_clean_data3_ggplot_04062022.pdf", width = 12, height = 8)
grid.arrange(plant_plots[[9]],plant_plots[[10]],plant_plots[[11]],plant_plots[[12]], ncol = 2)
dev.off()

pdf("Plots/PlantChem/Plant_16Group_KW_clean_data4_ggplot_04062022.pdf", width = 12, height = 8)
grid.arrange(plant_plots[[13]],plant_plots[[14]],plant_plots[[15]],plant_plots[[16]], ncol = 2)
dev.off()


