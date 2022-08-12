## Load libraries
library(dplyr)
library(agricolae)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header=T, row.names=1, sep="\t")

#### Subset raw soil data ####
chem <- chem[, c(1:37, 50)]
soil <- chem[, c(1:21)]

soil$Soil_type <- factor(soil$Soil_type,
                    levels = c("NM_PL14_S", "NM_PL35_S",
                               "M_PL22_S", "M_PL27_S"))

##### Starting Soil ####
soil.start <- subset(soil, soil$Population_type == "Blank_S.P")
soil.blank <- subset(soil, soil$Population_type == "Blank_P")

name1 <- paste(colnames(soil.start[, 9:21]), "avg", sep ="_")
name2 <- paste(colnames(soil.start[, 9:21]), "std", sep ="_")
name3 <- paste(colnames(soil.start[, 9:21]), "kw", sep ="_")
name4 <- paste(colnames(soil.start[, 9:21]), "wx", sep ="_")

### Avg and STD: Starting soil ###
# NM vs M
avg.st <- matrix(data = NA, nrow = 2, ncol = 26)
rownames(avg.st) <- c('NM', 'M')
colnames(avg.st) <- c(name1, name2)

wx <- matrix(data = NA, nrow = 1, ncol = 13)
colnames(wx) = name3


for(i in 9:21)
{
  avg1 <- aggregate(soil.start[,i],
                    list(soil.start$Site_type),
                    FUN = mean)
  avg.st[, i - 8] <- as.numeric(avg1[,2])
  sd1 <- aggregate(soil.start[,i],
                    list(soil.start$Site_type),
                    FUN = sd)
  avg.st[, i + 5] <- as.numeric(sd1[,2])
  w <- wilcox.test(soil.start[,i] ~ soil.start$Site_type, 
                    data = soil.start, paired = FALSE)
  wx[, i - 8] <- w$p.value
}

# 4 Sites
avg.st1 <- matrix(data = NA, nrow = 4, ncol = 26)
rownames(avg.st1) <- c("NM_PL14_S", "NM_PL35_S",
                       "M_PL22_S", "M_PL27_S")
colnames(avg.st1) <- c(name1, name2)

kw <- matrix(data = NA, nrow = 4, ncol = 13)
rownames(kw) <- c("NM_PL14_S", "NM_PL35_S",
                  "M_PL22_S", "M_PL27_S")
colnames(kw) <- c(name3)

for(i in 9:21)
{
  avg2 <- aggregate(soil.start[,i],
                    list(soil.start$Soil_type),
                    FUN = mean)
  avg.st1[, i - 8] <- as.numeric(avg2[,2])
  sd2 <- aggregate(soil.start[,i],
                   list(soil.start$Soil_type),
                   FUN = sd)
  avg.st1[, i + 5] <- as.numeric(sd2[,2])
  bla <- kruskal(soil.start[,12], soil.start$Soil_type, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  bla.group.ordered <- bla.group.ordered[c(3:4, 1:2),]
  kw[, i - 8] <- as.character(bla.group.ordered[,2])
}


start <- rbind(avg.st, avg.st1)

write.table(start, file = "Results/Starting_Blank_Soil/Starting_Soil_Avd_Std_08122022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

### Avg and STD: Blank soil ###
# NM vs M
avg.bl <- matrix(data = NA, nrow = 2, ncol = 26)
rownames(avg.bl) <- c('NM', 'M')
colnames(avg.bl) <- c(name1, name2)

wx.bl <- matrix(data = NA, nrow = 1, ncol = 13)
colnames(wx.bl) = name4


for(i in 9:21)
{
  avg2 <- aggregate(soil.blank[,i],
                    list(soil.blank$Site_type),
                    FUN = mean)
  avg.bl[, i - 8] <- as.numeric(avg2[,2])
  sd2 <- aggregate(soil.blank[,i],
                   list(soil.blank$Site_type),
                   FUN = sd)
  avg.bl[, i + 5] <- as.numeric(sd2[,2])
  w2 <- wilcox.test(soil.blank[,i] ~ soil.blank$Site_type, 
                   data = soil.blank, paired = FALSE)
  wx.bl[, i - 8] <- w2$p.value
}

# 4 Sites
avg.bl1 <- matrix(data = NA, nrow = 4, ncol = 26)
rownames(avg.bl1) <- c("NM_PL14_S", "NM_PL35_S",
                       "M_PL22_S", "M_PL27_S")
colnames(avg.bl1) <- c(name1, name2)

kw.bl <- matrix(data = NA, nrow = 4, ncol = 13)
rownames(kw.bl) <- c("NM_PL14_S", "NM_PL35_S",
                  "M_PL22_S", "M_PL27_S")
colnames(kw.bl) <- c(name3)

for(i in 9:21)
{
  avg3 <- aggregate(soil.start[,i],
                    list(soil.blank$Soil_type),
                    FUN = mean)
  avg.bl1[, i - 8] <- as.numeric(avg3[,2])
  sd3 <- aggregate(soil.start[,i],
                   list(soil.blank$Soil_type),
                   FUN = sd)
  avg.bl1[, i + 5] <- as.numeric(sd3[,2])
  bla1 <- kruskal(soil.blank[,12], soil.blank$Soil_type, group = T, p.adj="BH")
  bla.groups1 <- bla1$groups
  bla.group.ordered1 <- bla.groups1[order(as.character(row.names(bla.groups1))),]
  bla.group.ordered1 <- bla.group.ordered1[c(3:4, 1:2),]
  kw.bl[, i - 8] <- as.character(bla.group.ordered1[,2])
}


blank <- rbind(avg.bl, avg.bl1)

write.table(blank, file = "Results/Starting_Blank_Soil/Blank_Soil_Avd_Std_08122022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

