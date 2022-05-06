# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)
library(dendextend)
library(circlize)
library(ComplexHeatmap)

#### FAPROTAX Analysis ####
## Import Rarefied file: Bac/Arch rarefied file
#bl.asv.tax <- read.table("ahalleri_clonal_16S_ASV_Rarefy_01102022.txt", sep="\t", header=T, row.names=1)
#dim(bl.asv.tax) #16712 162
#colnames(bl.asv.tax)

#Subset asv read and taxonomy info into two variables
#bac.otu <- bl.asv.tax[,-c(156:162)]
#dim(bac.otu) #16712 155

#tax.table <- bl.asv.tax[,c(156:162)]
#dim(tax.table) #16712 7

#all(rownames(bac.otu) == rownames(tax.table))
#bac.otu$taxonomy<-paste(tax.table$Kingdom, tax.table$Phylum, tax.table$Class,
#                        tax.table$Order, tax.table$Family, tax.table$Genus,
#                        tax.table$Species, sep=";")

#write.table(bac.otu, file = "KMW_16S_asv_faprotax_01262022.txt", 
#sep = "\t", quote = F, row.names = T, col.names = NA)

#Faprtox python code, run from inside Faprotax folder
#python collapse_table.py -i /home/u22/pkushwaha/oak_16S_KMW/16S_oak_asv_faprotax_12142021.txt -o oak_faprotax.txt -g FAPROTAXls.txt -d "taxonomy" --omit_columns '0' -r report_16Soak.txt -v

#### Calculate proportion for faprotax groups ####
bac.otu <- read.table("Ahalleri_16S_asv_faprotax_02022022.txt", sep="\t", header=T, row.names=1)
dim(bac.otu) #10534 104

faprotax <- t(read.table("Ahalleri_16S_faprotax.txt", sep="\t", header=T, row.names=1))
dim(faprotax) #103 92

faprotax.filter <- subset(faprotax, select = colSums(faprotax) != 0 )
dim(faprotax.filter) #103 55

bac.otu.t <- t(bac.otu[,-104]) #103 10534

faprotax.prop <- faprotax.filter/rowSums(bac.otu.t) #103 55
faprotax.prop <- subset(faprotax.prop, select = colSums(faprotax.prop) != 0) #103 55

write.table(t(faprotax.prop), file="Arabidopsis_16S_faprotax_results_propor_03142022.txt", sep="\t", quote=F, row.names=T, col.names=NA)

## Import mapping file
map <- read.table("Arabidopsis_16S_MappigFile_woblanks_02022022.txt", sep="\t", header=T, row.names=1)#19 26
dim(map) #103 10

all(rownames(faprotax.prop) == rownames(map))
map.faprotax <- cbind(map, faprotax.prop)

write.table(map.faprotax, file="Ahalleri_16S_Mapping_faprotax_results_propor_03142022.txt", sep="\t", quote=F, row.names=T, col.names=NA)


#### Statistical Test ####
## Import file wo blank soils and remove any funguild with colSum = 0
map.faprotax <- read.table("Functional_Analysis/FAPROTAX/Ahalleri_16S_Mapping_faprotax_results_propor_woblaks_05052022.txt", 
                           sep = "\t", header = T, row.names = 1)
dim(map.faprotax) # 80 51

map.faprotax$Group <- factor(map.faprotax$Group,
                                  levels = c("NM_PL14_S_PL14_P", "NM_PL14_S_PL35_P", "NM_PL14_S_PL22_P", "NM_PL14_S_PL27_P",
                                             "NM_PL35_S_PL14_P", "NM_PL35_S_PL35_P", "NM_PL35_S_PL22_P", "NM_PL35_S_PL27_P",
                                             "M_PL22_S_PL14_P", "M_PL22_S_PL35_P", "M_PL22_S_PL22_P", "M_PL22_S_PL27_P",
                                             "M_PL27_S_PL14_P", "M_PL27_S_PL35_P", "M_PL27_S_PL22_P", "M_PL27_S_PL27_P"))

## Add new column with order of Group
map.faprotax$Level <- with(map.faprotax, ifelse(map.faprotax$Group == "NM_PL14_S_PL14_P", 'L1', 
                                                ifelse(map.faprotax$Group == "NM_PL14_S_PL35_P", 'L2',
                                                       ifelse(map.faprotax$Group == "NM_PL14_S_PL22_P", 'L3',
                                                              ifelse(map.faprotax$Group == "NM_PL14_S_PL27_P", 'L4',
                                                                     ifelse(map.faprotax$Group == "NM_PL35_S_PL14_P", 'L5', 
                                                                            ifelse(map.faprotax$Group == "NM_PL35_S_PL35_P", 'L6',
                                                                                   ifelse(map.faprotax$Group == "NM_PL35_S_PL22_P", 'L7',
                                                                                          ifelse(map.faprotax$Group == "NM_PL35_S_PL27_P", 'L8',
                                                                                                 ifelse(map.faprotax$Group == "M_PL22_S_PL14_P", 'L9', 
                                                                                                        ifelse(map.faprotax$Group == "M_PL22_S_PL35_P", 'L10',
                                                                                                               ifelse(map.faprotax$Group == "M_PL22_S_PL22_P", 'L11',
                                                                                                                      ifelse(map.faprotax$Group == "M_PL22_S_PL27_P", 'L12',
                                                                                                                             ifelse(map.faprotax$Group == "M_PL27_S_PL14_P", 'L13', 
                                                                                                                                    ifelse(map.faprotax$Group == "M_PL27_S_PL35_P", 'L14',
                                                                                                                                           ifelse(map.faprotax$Group == "M_PL27_S_PL22_P", 'L15','L16'))))))))))))))))

#### Matrix for sig letters Soil ####
kw.matrix <- matrix(data = NA, nrow = 16, ncol = 40)
rownames(kw.matrix) = c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(kw.matrix) = colnames(map.faprotax[, 12:51])

for(i in 12:51)
{
  bla <- kruskal(map.faprotax[,i], map.faprotax$Level, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  bla.group.ordered <- bla.group.ordered[c(1,9:16,2:8),]
  kw.matrix[, i - 11] <- as.character(bla.group.ordered[,2])
  # print(bla.group.ordered)
}

write.table(kw.matrix, file = "C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Results/ahalleriITS_faprotax_KW_05052022.txt", 
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### Plot funguild groups using ggplot ####
colnames_fap  <- names(map.faprotax)[12:51]
plots <- list()
plots <- lapply(12:51, function(i){
  ggplot(fungi , aes(x = Group,
                     y = map.faprotax[,i])) +
    geom_boxplot() +
    ylab(colnames_fap[i-11]) + 
    annotate( "text", x = 1, 
              y = (max(map.faprotax[,i] + (max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][1])+
    annotate( "text", x = 2, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][2])+
    annotate( "text", x = 3, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][3])+
    annotate( "text", x = 4, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][4])+
    annotate( "text", x = 5, 
              y = (max(map.faprotax[,i] + (max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][5])+
    annotate( "text", x = 6, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][6])+
    annotate( "text", x = 7, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix1[,i-11][7])+
    annotate( "text", x = 8, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][8])+
    annotate( "text", x = 9, 
              y = (max(map.faprotax[,i] + (max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][9])+
    annotate( "text", x = 10, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix1[,i-11][10])+
    annotate( "text", x = 11, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][11])+
    annotate( "text", x = 12, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][12])+
    annotate( "text", x = 13, 
              y = (max(map.faprotax[,i] + (max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][13])+
    annotate( "text", x = 14, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix1[,i-11][14])+
    annotate( "text", x = 15, 
              y = (max(map.faprotax[,i] +(max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][15])+
    annotate( "text", x = 16, 
              y = (max(map.faprotax[,i] + (max(map.faprotax[,i]))*0.03)), 
              label = kw.matrix[,i-11][16])+
    theme_bw() +
    theme(text = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
})

pdf("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Plots/Faprotax/Faprotax_KW1_05052022.pdf", 
    width = 12, height = 10)
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]], 
             plots[[5]],plots[[6]],plots[[7]],plots[[8]],
              ncol = 2)
dev.off()

pdf("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Plots/Faprotax/Faprotax_KW2_05052022.pdf", 
    width = 12, height = 10)
grid.arrange(plots[[9]],plots[[10]],plots[[11]],plots[[12]],
             plots[[13]],plots[[14]],plots[[15]],plots[[16]], 
              ncol = 2)
dev.off()

pdf("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Plots/Faprotax/Faprotax_KW3_05052022.pdf", 
    width = 10, height = 10)
grid.arrange(plots[[17]],plots[[18]],plots[[19]],plots[[20]],
plots[[21]],plots[[22]],plots[[23]],plots[[24]],ncol = 2)
dev.off()

pdf("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Plots/Faprotax/Faprotax_KW4_05052022.pdf", 
    width = 12, height = 10)
grid.arrange(plots[[25]],plots[[26]],plots[[27]],plots[[28]], 
             plots[[29]],plots[[30]],plots[[31]],plots[[32]],
              ncol = 2)
dev.off()

pdf("C:/Users/priya/Box/AZ_UA_Jan2022/2021_Ongoing_Projects/Ahalleri_Pot_Transplant/Plots/Faprotax/Faprotax_KW5_05052022.pdf", 
    width = 12, height = 10)
grid.arrange(plots[[33]],plots[[34]],plots[[35]],plots[[36]],
             plots[[37]],plots[[38]],plots[[39]],plots[[40]], 
              ncol = 2)
dev.off()

#### Hetamap ####
## For loop to calculate mean for each treatment
treat <- unique(map.faprotax$Group)
func_ratio <- data.frame()

for(t in treat) {
  func <- map.faprotax  %>%
    group_by(Group) %>%
    summarize(methanotrophy = mean(methanotrophy),
              methanol_oxidation = mean(methanol_oxidation),
              methylotrophy = mean(methylotrophy),
              aerobic_ammonia_oxidation = mean (aerobic_ammonia_oxidation),
              nitrification = mean(nitrification),
              sulfate_respiration = mean(sulfate_respiration),
              respiration_of_sulfur_compounds  = mean(respiration_of_sulfur_compounds),
              nitrate_denitrification = mean(nitrate_denitrification),
              nitrite_denitrification = mean(nitrite_denitrification),
              nitrous_oxide_denitrification = mean(nitrous_oxide_denitrification),
              denitrification = mean(denitrification),
              chitinolysis = mean(chitinolysis),
              nitrogen_fixation  = mean(nitrogen_fixation),
              nitrite_respiration = mean(nitrite_respiration),
              cellulolysis = mean(cellulolysis),
              dark_oxidation_of_sulfur_compounds  = mean(dark_oxidation_of_sulfur_compounds),
              fermentation = mean(fermentation),
              aerobic_chemoheterotrophy = mean(aerobic_chemoheterotrophy),
              chemoheterotrophy = mean(chemoheterotrophy),
              aromatic_hydrocarbon_degradation = mean(aromatic_hydrocarbon_degradation),
              aromatic_compound_degradation = mean(aromatic_compound_degradation),
              aliphatic_non_methane_hydrocarbon_degradation = mean(aliphatic_non_methane_hydrocarbon_degradation),
              hydrocarbon_degradation = mean(hydrocarbon_degradation),
              iron_respiration = mean(iron_respiration),
              nitrate_respiration = mean(nitrate_respiration),
              nitrogen_respiration = mean(nitrogen_respiration),
              anoxygenic_photoautotrophy_S_oxidizing = mean(anoxygenic_photoautotrophy_S_oxidizing),
              aerobic_anoxygenic_phototrophy = mean(aerobic_anoxygenic_phototrophy),
              anoxygenic_photoautotrophy = mean(anoxygenic_photoautotrophy),
              photoautotrophy = mean(photoautotrophy),
              photoheterotrophy = mean(photoheterotrophy),
              phototrophy = mean(phototrophy),
              ureolysis = mean(ureolysis))
  func_ratio <- data.frame(func)
}

options(scipen = 999)

## write out the file with averages per group
write.table(func_ratio,
            file = "Results/Faprotax_Selected_Functions_AvgProportion_050622022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

### Heatmap at treatment level ###
my_matrix <- as.matrix(func_ratio[, c(2:34)])
rownames(my_matrix) <- func_ratio$Group

col_fun = colorRamp2(c(0, 0.001, 0.005, 0.15), c("white", "yellow", "orange", "red"))
# row_dend = as.dendrogram(hclust(dist(my_matrix)))

pdf("Plots/Faprotax/Ahalleri__Faprotax_cluster_treatment_05062022.pdf",
    width = 12, height = 8)
Heatmap(my_matrix,
        cluster_columns = FALSE,
        col = col_fun,
        rect_gp = gpar(col = "darkgray", lwd = 1),
        row_dend_width =  unit(2, "cm"),
        column_names_gp = grid::gpar(fontsize = 8.5),
        row_names_gp = grid::gpar(fontsize = 9))
dev.off()

## Sample level
matrix <- as.matrix(map.faprotax[, c(12:29, 51, 33:40, 44:50)])
rownames(matrix) <- map.faprotax$Group

row_dend1 = as.dendrogram(hclust(dist(matrix)))

pdf("Plots/Faprotax/Ahalleri_Faprotax_cluster_sample_05062022.pdf",
    width = 10, height = 9)
Heatmap(matrix,
        cluster_columns = FALSE,
        col = col_fun,
        rect_gp = gpar(col = "darkgray", lwd = 1),
        column_names_gp = grid::gpar(fontsize = 7.5),
        row_names_gp = grid::gpar(fontsize = 4.5),
        # row_km = 2,
        row_dend_width =  unit(2, "cm"))
dev.off()

