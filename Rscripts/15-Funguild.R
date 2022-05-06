# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)

#### Funguild analysis ####
## Import Rarefied file: Bac/Arch rarefied file
bl.asv.tax <- read.table("ahalleri_clonal_ITS_ASV_Rarefy_01102022.txt", 
                         sep = "\t", header = T, row.names = 1)
dim(bl.asv.tax) #7986 110
colnames(bl.asv.tax)

#Subset asv read and taxonomy info into two variables
bac.otu <- bl.asv.tax[,-c(104:110)]
dim(bac.otu) #7986 103

tax.table <- bl.asv.tax[,c(104:110)]
dim(tax.table) #7986 7

all(rownames(bac.otu) == rownames(tax.table))

## Assign taxonomy in 1 column
bac.otu$taxonomy <- paste(tax.table$Kingdom, tax.table$Phylum, tax.table$Class,
                        tax.table$Order, tax.table$Family, tax.table$Genus,
                        tax.table$Species, sep=";")


## Save file for Funguild analyses
write.table(bac.otu, file="Ahalleri_16S_asv_funguild_02022022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### Calculate proportion for funguild groups ####
## Import results from Funguild analyses
fun.otu <- read.table("Ahalleri_ITS_asv_funguild_02022022.txt", 
                      sep = "\t", header = T, row.names = 1)
dim(fun.otu) #7986 104

funguild <- t(read.table("Ahalleri_ITS_asv_funguild_02022022.guilds_matched.txt", 
                         sep = "\t", header = T, row.names = 1))
dim(funguild) #113 3305
rownames(funguild)

## Check if Funguild and ASV samples are matched
fun.asv.t <- t(fun.otu)
all(rownames(fun.asv.t[1:103,]) == rownames(funguild[1:103,]))

fun.asv.t <- as.data.frame(funguild[1:103,])
fun.asv.t <- apply(fun.asv.t, 2, function (x) {as.numeric(as.character(x))})
rownames(fun.asv.t) <- rownames(funguild[1:103,])

## calculate proportion for guilds
guilds <- as.data.frame(apply((apply(fun.asv.t, 1, function (x) by(x, as.factor(funguild[108,]), sum))), 2, function (x) {x/sum(x)}))
dim(guilds) #93 103

write.table(guilds, file="Ahalleri_ITS_funguild_results_propor_02122022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

## Check for columns with many zeroes
guilds.t <- t(guilds)
guilds.t <- subset(guilds.t, select = colSums(guilds.t)!=0) #103 91


map <- read.table("ahalleri_clonal_ITS_MappingFile_woblanks_02022022.txt", sep="\t", header=T, row.names=1)#19 26
dim(map) #103 10

## Merge funguild proportion & metadata
all(rownames(guilds.t) == rownames(map))
map.guild <- cbind(map, guilds.t)

write.table(map.guild, file = "ahalleriITS_Mapping_funguild_results_propor_02122022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### Statistical Test ####
## Import file wo blank soils and remove any funguild with colSum =0
funguild_woblanks <- read.table("Functional_Analysis/FUNGuild/ahalleriITS_Mapping_funguild_results_propor_woblanks_05032022.txt", 
                         sep = "\t", header = T, row.names = 1) #80 96

funguild_woblanks <- subset(funguild_woblanks, 
                            select = colSums(funguild_woblanks[, c(12:96)])!= 0) #80 94


funguild_woblanks$Group <- factor(funguild_woblanks$Group,
                                  levels = c("NM_PL14_S_PL14_P", "NM_PL14_S_PL35_P", "NM_PL14_S_PL22_P", "NM_PL14_S_PL27_P",
                                             "NM_PL35_S_PL14_P", "NM_PL35_S_PL35_P", "NM_PL35_S_PL22_P", "NM_PL35_S_PL27_P",
                                             "M_PL22_S_PL14_P", "M_PL22_S_PL35_P", "M_PL22_S_PL22_P", "M_PL22_S_PL27_P",
                                             "M_PL27_S_PL14_P", "M_PL27_S_PL35_P", "M_PL27_S_PL22_P", "M_PL27_S_PL27_P"))


## Add new column with order of Group
funguild_woblanks$Level <- with(funguild_woblanks, ifelse(funguild_woblanks$Group == "NM_PL14_S_PL14_P", 'L1', 
                                                            ifelse(funguild_woblanks$Group == "NM_PL14_S_PL35_P", 'L2',
                                                                   ifelse(funguild_woblanks$Group == "NM_PL14_S_PL22_P", 'L3',
                                                                          ifelse(funguild_woblanks$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                                 ifelse(funguild_woblanks$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                                        ifelse(funguild_woblanks$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                                               ifelse(funguild_woblanks$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                                      ifelse(funguild_woblanks$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                                             ifelse(funguild_woblanks$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                                    ifelse(funguild_woblanks$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                                           ifelse(funguild_woblanks$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                                  ifelse(funguild_woblanks$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                                         ifelse(funguild_woblanks$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                                ifelse(funguild_woblanks$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                                       ifelse(funguild_woblanks$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))

#### Matrix for sig letters Soil ####
kw.matrix <- matrix(data = NA, nrow = 16, ncol = 85)
rownames(kw.matrix) = c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(kw.matrix) = colnames(funguild_woblanks[, 12:96])

for(i in 12:96)
{
  bla <- kruskal(funguild_woblanks[,i], funguild_woblanks$Level, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  bla.group.ordered <- bla.group.ordered[c(1,9:16,2:8),]
  kw.matrix[, i - 11] <- as.character(bla.group.ordered[,2])
  print(bla.group.ordered)
}

write.table(kw.matrix, file = "Results/ahalleriITS_funguild_KW_05052022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

## Subset the significantly different guilds ###
kw.matrix1 <- kw.matrix[, c(2, 4:15, 17:26, 29:33, 37, 39:48, 50:55, 57:64, 67, 70:74, 76:85)]
fungi <- cbind(funguild_woblanks[, 1:11], funguild_woblanks[, colnames(kw.subset)])

#### Plot funguild groups using ggplot ####
colnames_fungi  <- names(fungi)[12:80]
plots <- list()
plots <- lapply(12:80, function(i){
  ggplot(fungi , aes(x = Group,
                    y = fungi[,i])) +
    geom_boxplot() +
    ylab(colnames_fungi [i-11]) + 
    annotate( "text", x = 1, 
              y = (max(fungi[,i] + (max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][1])+
    annotate( "text", x = 2, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][2])+
    annotate( "text", x = 3, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][3])+
    annotate( "text", x = 4, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][4])+
    annotate( "text", x = 5, 
              y = (max(fungi[,i] + (max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][5])+
    annotate( "text", x = 6, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][6])+
    annotate( "text", x = 7, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][7])+
    annotate( "text", x = 8, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][8])+
    annotate( "text", x = 9, 
              y = (max(fungi[,i] + (max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][9])+
    annotate( "text", x = 10, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][10])+
    annotate( "text", x = 11, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][11])+
    annotate( "text", x = 12, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][12])+
    annotate( "text", x = 13, 
              y = (max(fungi[,i] + (max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][13])+
    annotate( "text", x = 14, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][14])+
    annotate( "text", x = 15, 
              y = (max(fungi[,i] +(max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][15])+
    annotate( "text", x = 16, 
              y = (max(fungi[,i] + (max(fungi[,i]))*0.03)), 
              label = kw.matrix1[,i-11][16])+
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

pdf("Plots/Funguild/Funguild_KW1_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]], 
             plots[[5]],plots[[6]],plots[[7]],plots[[8]],
             plots[[9]],plots[[10]],plots[[11]],plots[[12]], ncol = 3)
dev.off()

pdf("Plots/Funguild/Funguild_KW2_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[13]],plots[[14]],plots[[15]],plots[[16]], 
             plots[[17]],plots[[18]],plots[[19]],plots[[20]],
             plots[[21]],plots[[22]],plots[[23]],plots[[24]], ncol = 3)
dev.off()


pdf("Plots/Funguild/Funguild_KW3_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[25]],plots[[26]],plots[[27]],plots[[28]], 
             plots[[29]],plots[[30]],plots[[31]],plots[[32]],
             plots[[33]],plots[[34]],plots[[35]],plots[[36]], ncol = 3)
dev.off()

pdf("Plots/Funguild/Funguild_KW4_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[37]],plots[[38]],plots[[39]],plots[[40]], 
             plots[[41]],plots[[42]],plots[[43]],plots[[44]],
             plots[[45]],plots[[46]],plots[[47]],plots[[48]], ncol = 3)
dev.off()

pdf("Plots/Funguild/Funguild_KW5_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[49]],plots[[50]],plots[[51]],plots[[52]], 
             plots[[53]],plots[[54]],plots[[55]],plots[[56]],
             plots[[57]],plots[[58]],plots[[59]],plots[[60]], ncol = 3)
dev.off()

pdf("Plots/Funguild/Funguild_KW6_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[61]],plots[[62]],plots[[63]],plots[[64]], 
             plots[[65]],plots[[66]],plots[[67]],plots[[68]],
             plots[[69]], ncol = 3)
dev.off()


pdf("Plots/Funguild/Funguild_Selected_Funguild_05052022.pdf", width = 14, height = 10)
grid.arrange(plots[[16]],plots[[29]],plots[[37]],plots[[48]], 
             plots[[49]],plots[[55]],plots[[58]],plots[[59]],
             plots[[62]], plots[[64]],plots[[69]],ncol = 3)
dev.off()

