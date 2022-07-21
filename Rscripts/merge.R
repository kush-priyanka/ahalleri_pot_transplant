chem <- read.table("Data/SoilPlant_RGR_DataClean_04062022.txt", 
                   header = T, row.names = 1, sep = "\t")

chem_woblanks <- subset(chem, chem$Population != "Blank")

taxa <- read.table("Data/KeystoneTaxa_metal_BAF_05052022.txt", 
                           header = T, row.names = 1, sep = "\t")

new_chem <- chem_woblanks[order(row.names(chem_woblanks)), ]

pls.data <- cbind(new_chem, taxa)

pls.data1 <- pls.data[, c(1:48,187:188, 58:186)]

write.table(pls.data1, file="Data/Ahalleri_pot_soil_plant_microbial_05092022.txt",
            sep="\t", quote=F, row.names=T, col.names=NA)

pls <- read.table("Data/Ahalleri_pot_soil_plant_microbial_05092022.txt", 
                   header = T, row.names = 1, sep = "\t")

bac <- read.table("Data/ahalleri_clonal_16S_Richness_02162022.txt", 
                  header = T, row.names = 1)
fungi <- read.table("Data/ahalleri_clonal_ITS_Richness_02162022.txt", 
                    header = T, row.names = 1)

bac <- subset(bac, bac$Population != "Blank")
fungi <- subset(fungi, fungi$Population != "Blank")


new_bac <- bac[order(bac$Sample), ]
new_fungi <- fungi[order(fungi$Sample), ]

pls.final <- cbind(pls[, c(1:51)], 
                   new_bac[, c(12:15)], 
                   new_fungi[, c(12:15)], 
                   pls[, c(52:180)])

pls.final <- pls.final[,-c(8,29,30)]

write.table(pls.final, file="Data/Ahalleri_pot_soil_plant_microbial_final_05122022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)