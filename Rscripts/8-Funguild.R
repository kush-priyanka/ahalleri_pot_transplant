# Load libraries
library(vegan) #for multivariate statistics
library(ggplot2)
library(reshape)
library(gridExtra)
library(ggbiplot)
library(agricolae)

# Import mapping file with funguild proportion
metadata<-read.table("Functional_Analysis/FUNGuild/ahalleri_clonal_ITS_MappingFile_woblanks_02022022.txt", 
                     sep="\t", header=T, row.names=1)#19 26
dim(metadata) #103 10

#### Subset files at Soil type #####
pl14.p<-subset(metadata,metadata$Population_type=="NM_PL14_P") #20 10
pl35.p<-subset(metadata,metadata$Population_type=="NM_PL35_P") #20 10
pl22.p<-subset(metadata,metadata$Population_type=="M_PL22_P") #20 10
pl27.p<-subset(metadata,metadata$Population_type=="M_PL27_P") #20 10


guilds <- read.table(file="Functional_Analysis/FUNGuild/Ahalleri_ITS_funguild_results_propor_02122022.txt", 
                     sep="\t", header=T, row.names=1)

#Check for columns with many zeroes
guilds.t <- t(guilds)
guilds.t <- subset(guilds.t, select=colSums(guilds.t)!=0)
guilds.subset <- guilds.t[, c(21,40,50,60,63, 64, 71,76,79, 80, 84,86,91)]

pl14.fun<-guilds.subset[rownames(pl14.p),] #20 55
dim(pl14.fun) #20 13

map.pl14<-cbind(pl14.p, pl14.fun) #20 57

pl35.fun<-guilds.subset[rownames(pl35.p),] #20 55
dim(pl35.fun) #20 13

map.pl35<-cbind(pl35.p, pl35.fun)

pl22.fun<-guilds.subset[rownames(pl22.p),] #20 55
dim(pl22.fun) #20 13

map.pl22<-cbind(pl22.p, pl22.fun)

pl27.fun<-guilds.subset[rownames(pl27.p),] #20 55
dim(pl27.fun) #20 13

map.pl27<-cbind(pl27.p, pl27.fun)

#### PL14 Kruskal-Wallis ##### 
kw.matrix <- matrix(data = NA, nrow = 4, ncol = 13)
rownames(kw.matrix) = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S")
colnames(kw.matrix) = colnames(map.pl14[,11:23])

map.pl14$Soil_type <- factor(map.pl14$Soil_type,
                                  levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))


for(i in 11:23)
{
  bla <- kruskal(map.pl14[,i], map.pl14$Soil_type, group = T, p.adj="BH")
  bla.group <- bla$groups
  print(bla.group)
}

kw.matrix[,1] <- c("c", "b", "c", "a")
kw.matrix[,2] <- c("a", "a", "a", "a")
kw.matrix[,3] <- c("c", "b", "d", "a")
kw.matrix[,4] <- c("a", "a", "a", "a")
kw.matrix[,5] <- c("b", "b", "a", "b")
kw.matrix[,6] <- c("b", "b", "b", "a")
kw.matrix[,7] <- c("a", "a", "a", "a")
kw.matrix[,8] <- c("a", "a", "a", "a")
kw.matrix[,9] <- c("b", "b", "b", "a")
kw.matrix[,10] <- c("b", "ab", "c", "a")
kw.matrix[,11] <- c("a", "a", "a", "a")
kw.matrix[,12] <- c("b", "a", "b", "a")
kw.matrix[,13] <- c("a", "c", "bc", "b")

par(mfrow=c(2,5))
par(mar = c(4, 3, 0.5, 0.5), oma = c( 0, 0, 0, 0)) # to use when a main title is added
ylabels <- colnames(map.pl14[,11:23])

for (i in 11:23) 
{
  boxplot(map.pl14[,i]~Soil_type, data = map.pl14, 
          xlab = "", ylab = ylabels[i-10] , cex.lab=1.0, cex.axis=1.0)
  text(x = c(1:4), y = rep(max(map.pl14[,i], na.rm=T) + max(map.pl14[,i], na.rm=T)*0.01,4), 
       label = as.character(kw.matrix[,i-10]), cex=1.0)
}
