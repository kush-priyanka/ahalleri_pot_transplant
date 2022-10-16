library(vegan) #for multivariate statistics
library(ggplot2) #for visualization and plotting
library("devtools")
library(reshape)
library(ggforce) #for hulls
library(dplyr) #for summary
library(ggpubr)
library(gridExtra)
library(ggbiplot)
library(ecodist)
library(ape)
library(agricolae)

asv.tax <- read.table("16S_analysis_PK/Jan_Feb2022/Post_dada2_files/ahalleri_clonal_16S_Taxa_woBlanks_01102022.txt", sep="\t", header=T, row.names=1)
dim(asv.tax) #10534 110

#Extract only the sequencing information & remove taxonomy
asv<-t(asv.tax[,-c(104:110)])
dim(asv) #103 10534
taxa<-t(asv.tax[,c(104:110)])
dim(taxa) #7 10534

#Read metadata/mapping file
metadata<-read.table("16S_analysis_PK/Jan_Feb2022/Post_dada2_files/Arabidopsis_16S_MappigFile_woblanks_02022022.txt", sep="\t", header=T, row.names=1, check.names=F)
dim(metadata) #103 10

#Match sample names, all should be True
metadata<-metadata[rownames(asv),]
dim(metadata)
all(rownames(asv)==rownames(metadata))

#check number of sequences per sample 
hist(rowSums(asv))
sort(rowSums(asv))
summary(rowSums(asv))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#28921   41987   45950   45640   49061   6259

rar.asv = rrarefy(asv, 28921)
rar.asv = as.data.frame(rar.asv)

plant<-subset(metadata,metadata$Population!="Blank")


plant.asv <- rar.asv[rownames(plant),]
plant.asv <- subset(plant.asv, select=colSums(plant.asv)!=0)
dim(plant.asv) #80 9455

plant$Bac.Richness <-specnumber(plant.asv)
plant$Bac.Shannon <-diversity(plant.asv, index="shannon")

plant.dist<-vegdist(plant.asv, method="bray")
plant.nmds<-metaMDS(plant.dist, k=2)
plant.nmds$stress  #9.111137e-05

plant$Bac_NMDS1<-plant.nmds$points[,1]
plant$Bac_NMDS2<-plant.nmds$points[,2]


ggplot(plant, aes(Bac_NMDS1, Bac_NMDS2))+
  geom_point(aes(color = Population_type, shape = Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                     breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  theme_bw() +
  theme(legend.position ="right")

adonis2(plant.dist~plant$Soil_type)
adonis2(plant.dist~plant$Population)
adonis2(plant.dist~plant$Ecotype)

adonis2(plant.dist~plant$Ecotype/plant$Soil_type, strata=plant$Ecotype)


---
fasv.tax <- read.table("ITS_analysis_PK/Jan_Feb2022/Post_dada2_files/ahalleri_clonal_ITS_Taxa_woBlanks_01102022.txt", sep="\t", header=T, row.names=1)
dim(fasv.tax) #7986 110

#Extract only the sequencing information & remove taxonomy
fasv<-t(fasv.tax[,-c(104:110)])
dim(fasv) #103 7986


#Read metadata/mapping file
fmetadata<-read.table("ITS_analysis_PK/Jan_Feb2022/Post_dada2_files/ahalleri_clonal_ITS_MappingFile_woblanks_02022022.txt", sep="\t", header=T, row.names=1, check.names=F)
dim(fmetadata) #7 7986

#Match sample names, all should be True
fmetadata<-fmetadata[rownames(fasv),]
dim(fmetadata)
all(rownames(fasv)==rownames(fmetadata))

#check number of sequences per sample 
hist(rowSums(fasv))
sort(rowSums(fasv))
summary(rowSums(fasv))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10843   24798   28841   28785   33653   48534 

frar.asv = rrarefy(fasv, 10843)
frar.asv = as.data.frame(frar.asv)

fplant<-subset(fmetadata,fmetadata$Population!="Blank")


fplant.asv<-frar.asv[rownames(fplant),]
fplant.asv<-subset(fplant.asv, select=colSums(fplant.asv)!=0)
dim(fplant.asv) #80 5735

fplant$Fun.Richness <-specnumber(fplant.asv)
fplant$Fun.Shannon <-diversity(fplant.asv, index="shannon")

fplant.dist<-vegdist(fplant.asv, method="bray")
fplant.nmds<-metaMDS(fplant.dist, k=2)
fplant.nmds$stress #0.02401963

fplant$Fun_NMDS1<-fplant.nmds$points[,1]
fplant$Fun_NMDS2<-fplant.nmds$points[,2]


ggplot(fplant, aes(Fun_NMDS1, Fun_NMDS2))+
  geom_point(aes(color = Population_type, shape = Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                     breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  theme_bw() +
  theme(legend.position ="right")

adonis2(fplant.dist~fplant$Soil_type)
adonis2(fplant.dist~fplant$Population)
adonis2(fplant.dist~fplant$Ecotype)

adonis2(fplant.dist~fplant$Ecotype/fplant$Soil_type, strata=fplant$Ecotype)

### Merge Bac +Fun ###
all(names(plant$Sample)== names(fplant$Sample))

micro <- plant

micro$Fun.Richness <- fplant$Fun.Richness
micro$Fun.Shannon <- fplant$Fun.Shannon
micro$Fun_NMDS1 <- fplant$Fun_NMDS1
micro$Fun_NMDS2 <-fplant$Fun_NMDS2

rownames(micro) <- micro$Sample

micro <- micro %>%
  select(-Barcode, -Plate, -Well, -Sample)

write.table(micro, file = "Data/ahalleri_clonal_Diversity_07112022.txt", sep="\t", quote=F, row.names=T, col.names=NA)
