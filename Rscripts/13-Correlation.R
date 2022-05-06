## Load libraries
library(Hmisc)
library(corrplot)
library(qvalue)

#### Import data ####
taxa.metal <- read.table('Data/LefSe_taxa_metal_05052022.txt', 
                         header = T, sep = '\t', row.names = 1)
dim(taxa.metal) #155 78

#### Calculate Bioaccumulation Factor ####
taxa.metal$Zn_BAF <- as.numeric(taxa.metal$Zn_plant/taxa.metal$Zn_soil)
taxa.metal$Cd_BAF <- as.numeric(taxa.metal$Cd_plant/taxa.metal$Cd_soil)

write.table(taxa.metal, file="KeystoneTaxa_metal_BAF_05052022.txt",
            sep="\t", quote=F, row.names=T, col.names=NA)

## Subset for columns with data to be co-related
taxa.metal.sub <- as.matrix(taxa.metal[,c(9:139)])
taxa.metal.sub[is.na(taxa.metal.sub)] <- 0
dim(taxa.metal.sub) #80 131

## Correlation and p.value adjustments
corr <- rcorr(taxa.metal.sub, type = c("spearman"))
r <- corr$r
dim(r) #131 131
p <- corr$P


## fdr based on Benjamini & Hochberg
qobj <- qvalue(p = p, lamda = 0, fdr.level = 0.05)
q <- qobj$qvalue
dim(q) #131 131

### Subset only microbe + metal correlations : r & q-values ####
corr.b2 <- r[c(130:131), c(1:129)] #2 129
corr.b2 <- as.data.frame(corr.b2)
q.b2 <- q[c(130:131), c(1:129)] #2 129
q.b2 <- as.data.frame(q.b2)

write.table(corr.b2, file = "Results/Taxa_BAF_Corr_coefficient_05052022.txt",
sep="\t", quote = F, row.names = T, col.names = NA)

write.table(q.b2, file = "Results/Taxa_BAF_Corr_qvalues_fdr_05052022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### Subset bac/arc taxa + BAF correlations ####
corr.bac <- as.data.frame(r[c(130:131), c(1:62)]) 
q.bac <- as.data.frame(q[c(130:131), c(1:62)]) 

#### Subset fun taxa + BAF correlations ####
corr.fun <- as.data.frame(r[c(130:131), c(63:129)]) 
q.fun <- as.data.frame(q[c(130:131), c(63:129)]) 


#### Plot ####
pdf("Plots/Correlation/Bac_BAF_corr_05052022.pdf", 
    width = 10, 
    height = 4)
corrplot(as.matrix(corr.bac),
         p.mat = as.matrix(q.bac), 
         method ="color", 
         addgrid.col = 'white',
         insig = "label_sig", 
         cl.pos ='b',
         cl.ratio = 1,
         sig.level = c(0.001,.01,0.05),
         tl.col = "black", 
         tl.cex = 0.5, 
         pch.cex = 0.7, 
         pch.col = "#202020")
dev.off()

pdf("Plots/Correlation/Fungi_BAF_corr_05052022.pdf", 
    width = 10, 
    height = 4)
corrplot(as.matrix(corr.fun),
         p.mat = as.matrix(q.fun), 
         method ="color", 
         addgrid.col = 'white',
         insig = "label_sig", 
         cl.pos ='b',
         cl.ratio = 1,
         sig.level = c(0.001,.01,0.05),
         tl.col = "black", 
         tl.cex = 0.5, 
         pch.cex = 0.7, 
         pch.col = "#202020")
dev.off()

