## Load libraries
library(dplyr)

## Import raw soil + plant data
chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header=T, row.names=1, sep="\t")

#### Subset raw soil data ####
chem <- chem[, c(1:37, 50)]
chem_woblanks <- subset(chem, chem$Population != "Blank")

chem_woblanks$Site_type <-factor(chem_woblanks$Site_type,
                                 levels = c("NM", "M"))

chem_woblanks$Level <- with(chem_woblanks, ifelse(chem_woblanks$Group == "NM_PL14_S_PL14_P", 'L1', 
                                                            ifelse(chem_woblanks$Group == "NM_PL14_S_PL35_P", 'L2',
                                                                   ifelse(chem_woblanks$Group == "NM_PL14_S_PL22_P", 'L3',
                                                                          ifelse(chem_woblanks$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                                 ifelse(chem_woblanks$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                                        ifelse(chem_woblanks$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                                               ifelse(chem_woblanks$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                                      ifelse(chem_woblanks$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                                             ifelse(chem_woblanks$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                                    ifelse(chem_woblanks$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                                           ifelse(chem_woblanks$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                                  ifelse(chem_woblanks$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                                         ifelse(chem_woblanks$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                                ifelse(chem_woblanks$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                                       ifelse(chem_woblanks$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))
#### Calculate average by 16 treatments ####
avg.matrix <- matrix(data = NA, nrow = 16, ncol = 29)
rownames(avg.matrix) <- c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(avg.matrix) <- colnames(chem_woblanks[, 9:37])


for(i in 9:37)
{
  avg <- aggregate(chem_woblanks[,i],
                   list(chem_woblanks$Level),
                   FUN = mean)
  avg <- avg[c(1,9:16,2:8),]
  avg.matrix[, i - 8] <- as.numeric(avg[,2])
}

colnames(avg.matrix) <- paste( colnames(avg.matrix), 
                               "avg" , sep = "_")

#### Calculate stdev by 16 treatments ####
std.matrix <- matrix(data = NA, nrow = 16, ncol = 29)
rownames(std.matrix) <- c('L1', 'L2', 'L3','L4',
                          'L5','L6', 'L7', 'L8', 
                          'L9', 'L10', 'L11', 'L12', 
                          'L13', 'L14','L15', 'L16')
colnames(std.matrix) <- colnames(avg.matrix)



for(i in 9:37)
{
  std <- aggregate(chem_woblanks[,i],
                   list(chem_woblanks$Level),
                   FUN = sd)
  std <- std[c(1,9:16,2:8),]
  std.matrix[, i - 8] <- as.numeric(std[,2])
}

colnames(std.matrix) <- paste( colnames(std.matrix), 
                               "std" , sep = "_")
#### Combine avg + sd 16 groups ####
avg.sd <- cbind(avg.matrix, std.matrix)

rownames(avg.sd) <- c("NM_PL14_S_PL14_P",
                      "NM_PL14_S_PL35_P",
                      "NM_PL14_S_PL22_P",
                      "NM_PL14_S_PL27_P",
                      "NM_PL35_S_PL14_P",
                      "NM_PL35_S_PL35_P",
                      "NM_PL35_S_PL22_P",
                      "NM_PL35_S_PL27_P",
                      "M_PL22_S_PL14_P",
                      "M_PL22_S_PL35_P",
                      "M_PL22_S_PL22_P",
                      "M_PL22_S_PL27_P",
                      "M_PL27_S_PL14_P",
                      "M_PL27_S_PL35_P",
                      "M_PL27_S_PL22_P",
                      "M_PL27_S_PL27_P")

# write.table(avg.sd, file = "Data/Soil_Plant_Avd_Std_16group_08102022.txt", 
#             sep = "\t", quote = F, row.names = T, col.names = NA)

#### Avg Site type M vs NM ####
avg.matrix1 <- matrix(data = NA, nrow = 2, ncol = 29)
rownames(avg.matrix1) <- c('NM', 'M')
colnames(avg.matrix1) <- colnames(chem_woblanks[, 9:37])

for(i in 9:37)
{
  avg1 <- aggregate(chem_woblanks[,i],
                   list(chem_woblanks$Site_type),
                   FUN = mean)
  avg.matrix1[, i - 8] <- as.numeric(avg1[,2])
}

colnames(avg.matrix1) <- paste(colnames(avg.matrix1), 
                               "avg" , sep = "_")
#### Calculate stdev by Site type NM vs M ####
std.matrix1 <- matrix(data = NA, nrow = 2, ncol = 29)
rownames(std.matrix1) <- c('NM', 'M')
colnames(std.matrix1) <- colnames(avg.matrix1)

for(i in 9:37)
{
  std1 <- aggregate(chem_woblanks[,i],
                   list(chem_woblanks$Site_type),
                   FUN = sd)
  std.matrix1[, i - 8] <- as.numeric(std1[,2])
}

colnames(std.matrix1) <- paste( colnames(std.matrix1), 
                               "std" , sep = "_")
## Merge Site & 16 groups
site <- cbind(avg.matrix1, std.matrix1)

final <- rbind(site, avg.sd) ##Soil & Plant

#### Microbial diversity data #####
div <- read.table("Data/ahalleri_clonal_Diversity_07112022.txt", 
                  header=T, row.names=1, sep="\t") #80 14

div$Site_type <-factor(div$Site_type,
                                 levels = c("NM", "M"))
## Add new column with order of Group
div$Level <- with(div, ifelse(div$Soil_type == "NM_PL14_S" & div$Population_type =="NM_PL14_P", 'L1', 
                              ifelse(div$Soil_type == "NM_PL14_S" & div$Population_type == "NM_PL35_P", 'L2',
                                     ifelse(div$Soil_type == "NM_PL14_S" & div$Population_type == "M_PL22_P", 'L3',
                                            ifelse(div$Soil_type == "NM_PL14_S" & div$Population_type == "M_PL27_P", 'L4',
                                                   ifelse(div$Soil_type == "NM_PL35_S" & div$Population_type == "NM_PL14_P", 'L5', 
                                                          ifelse(div$Soil_type == "NM_PL35_S" & div$Population_type == "NM_PL35_P", 'L6',
                                                                 ifelse(div$Soil_type == "NM_PL35_S" & div$Population_type == "M_PL22_P", 'L7',
                                                                        ifelse(div$Soil_type == "NM_PL35_S" & div$Population_type == "M_PL27_P", 'L8',
                                                                               ifelse(div$Soil_type == "M_PL22_S" & div$Population_type == "NM_PL14_P", 'L9', 
                                                                                      ifelse(div$Soil_type == "M_PL22_S" & div$Population_type == "NM_PL35_P", 'L10',
                                                                                             ifelse(div$Soil_type == "M_PL22_S" & div$Population_type == "M_PL22_P", 'L11',
                                                                                                    ifelse(div$Soil_type == "M_PL22_S" & div$Population_type == "M_PL27_P", 'L12',
                                                                                                           ifelse(div$Soil_type == "M_PL27_S" & div$Population_type == "NM_PL14_P", 'L13', 
                                                                                                                  ifelse(div$Soil_type == "M_PL27_S" & div$Population_type == "NM_PL35_P", 'L14',
                                                                                                                         ifelse(div$Soil_type == "M_PL27_S" & div$Population_type == "M_PL22_P", 'L15','L16'))))))))))))))))

## Shuffle columns for easy for loop
div <- div[,c(1:8, 11:12, 9:10, 13:15)]

#### Calculate average by 16 treatments ####
avg.div <- matrix(data = NA, nrow = 16, ncol = 4)
rownames(avg.div) <- c('L1', 'L2', 'L3','L4',
                          'L5','L6', 'L7', 'L8', 
                          'L9', 'L10', 'L11', 'L12', 
                          'L13', 'L14','L15', 'L16')

colnames(avg.div) <- colnames(div[, 7:10])

for(i in 7:10)
{
  avg <- aggregate(div[,i],
                   list(div$Level),
                   FUN = mean)
  avg <- avg[c(1,9:16,2:8),]
  avg.div[, i - 6] <- as.numeric(avg[,2])
}

colnames(avg.div) <- paste( colnames(avg.div), 
                               "avg" , sep = "_")

#### Calculate stdev by 16 treatments ####
sd.div <- matrix(data = NA, nrow = 16, ncol = 4)
rownames(sd.div) <- c('L1', 'L2', 'L3','L4',
                       'L5','L6', 'L7', 'L8', 
                       'L9', 'L10', 'L11', 'L12', 
                       'L13', 'L14','L15', 'L16')

colnames(sd.div) <- colnames(div[, 7:10])

for (i in 7:10){
std <- aggregate(div[,i],
                 list(div$Level),
                 FUN = sd)
std <- std[c(1,9:16,2:8),]
sd.div[, i - 6] <- as.numeric(std[,2])
}

colnames(sd.div) <- paste( colnames(sd.div), 
                            "std" , sep = "_")
#### Combine microbial avg + sd 16 groups ####
m <- cbind(avg.div, sd.div)

rownames(m) <- c("NM_PL14_S_PL14_P",
                 "NM_PL14_S_PL35_P",
                 "NM_PL14_S_PL22_P",
                 "NM_PL14_S_PL27_P",
                 "NM_PL35_S_PL14_P",
                 "NM_PL35_S_PL35_P",
                 "NM_PL35_S_PL22_P",
                 "NM_PL35_S_PL27_P",
                 "M_PL22_S_PL14_P",
                 "M_PL22_S_PL35_P",
                 "M_PL22_S_PL22_P",
                 "M_PL22_S_PL27_P",
                 "M_PL27_S_PL14_P",
                 "M_PL27_S_PL35_P",
                 "M_PL27_S_PL22_P",
                 "M_PL27_S_PL27_P")

#### Microbial Avg Site type M vs NM ####
avg.div <- matrix(data = NA, nrow = 2, ncol = 4)
rownames(avg.div) <- c('NM', 'M')
colnames(avg.div) <- colnames(div[, 7:10])

for(i in 7:10)
{
  avg1 <- aggregate(div[,i],
                    list(div$Site_type),
                    FUN = mean)
  avg.div[, i - 6] <- as.numeric(avg1[,2])
}

colnames(avg.div) <- paste(colnames(avg.div), 
                               "avg" , sep = "_")

#### Calculate stdev by Site type NM vs M ####
std.div <- matrix(data = NA, nrow = 2, ncol = 4)
rownames(std.div) <- c('NM', 'M')
colnames(std.div) <- colnames(div[, 7:10])

for(i in 7:10)
{
  std1 <- aggregate(div[,i],
                    list(div$Site_type),
                    FUN = sd)
  std.div[, i - 6] <- as.numeric(std1[,2])
}

colnames(std.div) <- paste( colnames(std.div), 
                                "std" , sep = "_")
## Merge Site & 16 groups
m.site <- cbind(avg.div, std.div)

m.final <- rbind(m.site, m) #Microbial diversity

#### Merge Soil + Plant + Microbe #####
master.final <- cbind(final, m.final)

master.final <- round(master.final, digits = 4)

write.table(master.final, file = "Data/Avg_Std_all_08102022.txt", 
            sep="\t", quote = F, row.names = T, col.names = NA)

