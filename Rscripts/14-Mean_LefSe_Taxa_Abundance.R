## Load libraries
library(tidyverse)
library(dplyr)

#### Import data ####
taxa.metal <- read.table('Data/LefSe_taxa_metal_05052022.txt', 
                         header = T, sep = '\t', row.names = 1)
dim(taxa.metal) #80 137

### Subset and transpose only taxa values ###
taxa <- t(taxa.metal[, c(9:137)])
meta <- taxa.metal[, c(1:4)]

taxa.meta <- taxa.metal[, c(1:4, 9:137)]

all(colnames(taxa) == rownames(meta)) #TRUE

## Create 16 subset groups ####
# Soil 1
pl14s.pl14p <- meta %>%
  filter(Soil_type == "NM_PL14_S" & Population_type == "NM_PL14_P")

pl14s.pl35p <- meta %>%
  filter(Soil_type == "NM_PL14_S" & Population_type == "NM_PL35_P")

pl14s.pl22p <- meta %>%
  filter(Soil_type == "NM_PL14_S" & Population_type == "M_PL22_P")

pl14s.pl27p <- meta %>%
  filter(Soil_type == "NM_PL14_S" & Population_type == "M_PL27_P")

## Soil 2
pl35s.pl14p <- meta %>%
  filter(Soil_type == "NM_PL35_S" & Population_type == "NM_PL14_P")

pl35s.pl35p <- meta %>%
  filter(Soil_type == "NM_PL35_S" & Population_type == "NM_PL35_P")

pl35s.pl22p <- meta %>%
  filter(Soil_type == "NM_PL35_S" & Population_type == "M_PL22_P")

pl35s.pl27p <- meta %>%
  filter(Soil_type == "NM_PL35_S" & Population_type == "M_PL27_P")

## Soil 3
pl22s.pl14p <- meta %>%
  filter(Soil_type == "M_PL22_S" & Population_type == "NM_PL14_P")

pl22s.pl35p <- meta %>%
  filter(Soil_type == "M_PL22_S" & Population_type == "NM_PL35_P")

pl22s.pl22p <- meta %>%
  filter(Soil_type == "M_PL22_S" & Population_type == "M_PL22_P")

pl22s.pl27p <- meta %>%
  filter(Soil_type == "M_PL22_S" & Population_type == "M_PL27_P")

## Soil 4
pl27s.pl14p <- meta %>%
  filter(Soil_type == "M_PL27_S" & Population_type == "NM_PL14_P")

pl27s.pl35p <- meta %>%
  filter(Soil_type == "M_PL27_S" & Population_type == "NM_PL35_P")

pl27s.pl22p <- meta %>%
  filter(Soil_type == "M_PL27_S" & Population_type == "M_PL22_P")

pl27s.pl27p <- meta %>%
  filter(Soil_type == "M_PL27_S" & Population_type == "M_PL27_P")


#### Subset taxa using soils ####
t1 <- as.data.frame(t(taxa[,rownames(pl14s.pl14p)]))
t2 <- as.data.frame(t(taxa[,rownames(pl14s.pl35p)]))
t3 <- as.data.frame(t(taxa[,rownames(pl14s.pl22p)]))
t4 <- as.data.frame(t(taxa[,rownames(pl14s.pl27p)]))

t5 <- as.data.frame(t(taxa[,rownames(pl35s.pl14p)]))
t6 <- as.data.frame(t(taxa[,rownames(pl35s.pl35p)]))
t7 <- as.data.frame(t(taxa[,rownames(pl35s.pl22p)]))
t8 <- as.data.frame(t(taxa[,rownames(pl35s.pl27p)]))

t9 <- as.data.frame(t(taxa[,rownames(pl22s.pl14p)]))
t10 <- as.data.frame(t(taxa[,rownames(pl22s.pl35p)]))
t11 <- as.data.frame(t(taxa[,rownames(pl22s.pl22p)]))
t12 <- as.data.frame(t(taxa[,rownames(pl22s.pl27p)]))

t13 <- as.data.frame(t(taxa[,rownames(pl27s.pl14p)]))
t14 <- as.data.frame(t(taxa[,rownames(pl27s.pl35p)]))
t15 <- as.data.frame(t(taxa[,rownames(pl27s.pl22p)]))
t16 <- as.data.frame(t(taxa[,rownames(pl27s.pl27p)]))

### Calculate average for each subset ####
t1.mean <- as.data.frame(colMeans(t1[sapply(t1, is.numeric)]))
t2.mean <- as.data.frame(colMeans(t2[sapply(t2, is.numeric)]))
t3.mean <- as.data.frame(colMeans(t3[sapply(t3, is.numeric)]))
t4.mean <- as.data.frame(colMeans(t4[sapply(t4, is.numeric)]))
t5.mean <- as.data.frame(colMeans(t5[sapply(t5, is.numeric)]))
t6.mean <- as.data.frame(colMeans(t6[sapply(t6, is.numeric)]))
t7.mean <- as.data.frame(colMeans(t7[sapply(t7, is.numeric)]))
t8.mean <- as.data.frame(colMeans(t8[sapply(t8, is.numeric)]))
t9.mean <- as.data.frame(colMeans(t9[sapply(t9, is.numeric)]))
t10.mean <- as.data.frame(colMeans(t10[sapply(t10, is.numeric)]))
t11.mean <- as.data.frame(colMeans(t11[sapply(t11, is.numeric)]))
t12.mean <- as.data.frame(colMeans(t12[sapply(t12, is.numeric)]))
t13.mean <- as.data.frame(colMeans(t13[sapply(t13, is.numeric)]))
t14.mean <- as.data.frame(colMeans(t14[sapply(t14, is.numeric)]))
t15.mean <- as.data.frame(colMeans(t15[sapply(t15, is.numeric)]))
t16.mean <- as.data.frame(colMeans(t16[sapply(t16, is.numeric)]))

options(scipen=999)

### Add treatment name to columns ###
colnames(t1.mean) <- 'pl14s.pl14p'
colnames(t2.mean)  <- 'pl14s.pl35p'
colnames(t3.mean)  <- 'pl14s.pl22p'
colnames(t4.mean)  <- 'pl14s.pl27p'

colnames(t5.mean)  <- 'pl35s.pl14p'
colnames(t6.mean)  <- 'pl35s.pl35p'
colnames(t7.mean)  <- 'pl35s.pl22p'
colnames(t8.mean) <- 'pl35s.pl27p'

colnames(t9.mean)  <- 'pl22s.pl14p'
colnames(t10.mean)  <- 'pl22s.pl35p'
colnames(t11.mean)  <- 'pl22s.pl22p'
colnames(t12.mean)  <- 'pl22s.pl27p'

colnames(t13.mean)  <- 'pl27s.pl14p'
colnames(t14.mean) <- 'pl27s.pl35p'
colnames(t15.mean)  <- 'pl27s.pl22p'
colnames(t16.mean)  <- 'pl27s.pl27p'

## Merge datasets ###
taxa.mean <- cbind(t1.mean, t2.mean)
taxa.mean <- cbind(taxa.mean, t3.mean)
taxa.mean <- cbind(taxa.mean, t4.mean)
taxa.mean <- cbind(taxa.mean, t5.mean)
taxa.mean <- cbind(taxa.mean, t6.mean)
taxa.mean <- cbind(taxa.mean, t7.mean)
taxa.mean <- cbind(taxa.mean, t8.mean)
taxa.mean <- cbind(taxa.mean, t9.mean)
taxa.mean <- cbind(taxa.mean, t10.mean)
taxa.mean <- cbind(taxa.mean, t11.mean)
taxa.mean <- cbind(taxa.mean, t12.mean)
taxa.mean <- cbind(taxa.mean, t13.mean)
taxa.mean <- cbind(taxa.mean, t14.mean)
taxa.mean <- cbind(taxa.mean, t15.mean)
taxa.mean <- cbind(taxa.mean, t16.mean)


write.table(taxa.mean, file = "Results/LefSe_Taxa_Mean_Treatment_07062022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## Merge Mean of rel abundance with corr and q-values ###
corr.b2 <- read.table('Results/Correlation/Taxa_BAF_Corr_coefficient_05052022.txt', 
                         header = T, sep = '\t', row.names = 1)

q.b2 <- read.table('Results/Correlation/Taxa_BAF_Corr_qvalues_fdr_05052022.txt', 
                      header = T, sep = '\t', row.names = 1)

taxa.mean.corr <- cbind(t(corr.b2), t(q.b2), taxa.mean)

write.table(taxa.mean.corr, file = "Results/LefSe_Taxa_Mean_Treatment_Corr_07062022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)