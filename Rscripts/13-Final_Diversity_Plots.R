# Load libraries
library(ggplot2)
library(agricolae)
library(gridExtra)
library(tibble)
library(dplyr)

## Import microbial diversity data for plants only
div <- read.table("Data/ahalleri_clonal_Diversity_07112022.txt", 
                   header=T, row.names=1, sep="\t") #80 14


div$Soil_type <-factor(div$Soil_type,
                                 levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

div$Population_type <-factor(div$Population_type,
                                       levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))



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

#### KW: bac and fungi ####
kw.matrix <- matrix(data = NA, nrow = 16, ncol = 4)
rownames(kw.matrix) = c('L1', 'L2', 'L3','L4',
                        'L5','L6', 'L7', 'L8', 
                        'L9', 'L10', 'L11', 'L12', 
                        'L13', 'L14','L15', 'L16')
colnames(kw.matrix) = colnames(div[, 7:10])

for(i in 7:10)
{
  bla <- kruskal(div[,i], div$Level, group = T, p.adj="BH")
  bla.groups <- bla$groups
  bla.group.ordered <- bla.groups[order(as.character(row.names(bla.groups))),]
  bla.group.ordered <- bla.group.ordered[c(1,9:16,2:8),]
  kw.matrix[, i - 6] <- as.character(bla.group.ordered[,2])
  print(bla.group.ordered)
}

#### WX: M vs NM diversity ####
wx.matrix <- matrix(data = NA, nrow = 1, ncol = 4)
colnames(wx.matrix) = colnames(div[, 7:10])

for(i in 7:10)
{
  wx <- wilcox.test(div[,i] ~ div$Site_type, 
                     data = div, paired = FALSE)
  wx.matrix[, i - 6] <- wx$p.value
}
### ANOVA###
# for(i in 7:10)
# {
#   a1 <- aov(div[,i]~ Level, data = div)
#   tukey <- TukeyHSD(a1)
#   Cld <- multcompLetters4(a1, tukey)
#   cld <- as.data.frame.list(Cld$Level)
#   cld.ordered <- cld[order(as.character(row.names(cld))),]
#   cld.ordered <- cld.ordered[c(1,9:16,2:8),]
#   cld.let <- cld.ordered$Letters
#   kw.matrix[, i - 6] <- cld.let
# }

### Assign max value for sig letters position ###
names <- c("Soil_type","Population_type", "Bac.Richness_yloc","Bac.Shannon_yloc",
           "Fungi.Richness_yloc" ,"Fungi.Shannon_yloc",
           "Bac.Richness_sig","Bac.Shannon_sig","Fungi.Richness_sig" ,"Fungi.Shannon_sig")

sig <- data.frame(matrix(nrow = 16, ncol = 10))
rownames(sig) <- c(1:16)
colnames(sig) <- names

sig$Population_type <- as.character(c("NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P", "M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P",
                                      "NM_PL14_P","NM_PL35_P","M_PL22_P","M_PL27_P"))

sig$Soil_type <- as.character(c("NM_PL14_S","NM_PL14_S", "NM_PL14_S","NM_PL14_S",
                                "NM_PL35_S", "NM_PL35_S", "NM_PL35_S","NM_PL35_S",
                                "M_PL22_S", "M_PL22_S","M_PL22_S","M_PL22_S",
                                "M_PL27_S","M_PL27_S", "M_PL27_S","M_PL27_S"))

for(i in 7:10){
  sig[, i - 4] <- (max(div[,i] +(max(div[,i]))*0.03))
  sig[, i] <- kw.matrix[,i-6]
}

### Plot soil variables using ggplot 
colnames_div <- c("Bacterial/Archaeal Richness","Bacterial/Archaeal Shannon", 
                  "Fungal Richness","Fungal Shannon")
div_plots <- list()
div_plots <- lapply(7:10, function(i){
  ggplot(div, aes(x = Soil_type,
                            y = div[,i], 
                            fill = Population_type)) +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom ="point", shape = 22, size = 3, color = "black",
                 position = position_dodge2(width = 0.75,   
                                            preserve = "single")) +
    ylab(colnames_div[i-6]) + 
    scale_fill_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
    theme_bw() +
    theme(text = element_text(size = 8),
          axis.title.x = element_blank(),
          legend.position="bottom") +
    geom_text(data = sig, aes(y = sig[,i-4], label = sig[,i]), 
              position = position_dodge(width = .75))
})

pdf("Plots/Bac_ADiversity_07112022.pdf", width = 10, height = 6)
grid.arrange(div_plots[[1]],div_plots[[2]], ncol = 1)
dev.off()


pdf("Plots/Fun_ADiversity_07112022.pdf", width = 10, height = 6)
grid.arrange(div_plots[[3]],div_plots[[4]], ncol = 1)
dev.off()

##### Plot NMDS Plots ####
pdf("Plots/Ahalleri_Bac_NMDS_Plants_Only_07112022.pdf", width=8)
ggplot(div, aes(Bac_NMDS1, Bac_NMDS2))+
  geom_point(aes(color = Population_type, shape = Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                    breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  annotate( "text", x = 0.5,
            y = -0.24,
            label = "Stress = < 0.001") +
  annotate( "text", x = 0.43,
            y = 0.22,
            label = bquote(R^2~'(Site type)= 0.48***')) +
  annotate( "text", x = 0.43,
            y = 0.20,
            label = bquote(R^2~'(Site type X Soil type)= 0.38***')) +
  theme_bw() +
  theme(legend.position ="right")
dev.off()

pdf("Plots/Ahalleri_Fungi_NMDS_Plants_Only_07112022.pdf", width=8)
ggplot(div, aes(Fun_NMDS1, Fun_NMDS2))+
  geom_point(aes(color = Population_type, shape= Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                     breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  annotate( "text", x = 0.5,
            y = -0.52,
            label = "Stress = 0.024") +
  annotate( "text", x = 0.4,
            y = 0.45,
            label = bquote(R^2~'(Site type)= 0.29***')) +
  annotate( "text", x = 0.4,
            y = 0.4,
            label = bquote(R^2~'(Site type X Soil type)= 0.53***')) +
  theme_bw() +
  theme(legend.position ="right")
  theme_bw() +
  theme(legend.position = "right")
dev.off()

