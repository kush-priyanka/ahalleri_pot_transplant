## Load library
library(ggplot2)
library(tidyverse)
library(reshape2)

## Import mean average proportion for selected microbial functions
taxa <- read.table("Results/Heatmap_PLS_Taxa_09072022.txt", 
                       sep = "\t", header = T)

taxa.melt <- melt(taxa[,c(1,2,5:20)])


# rownames(taxa) <- taxa$Taxa #change rownames to asv numbers
# read <- t(taxa[, 2:17]) #tranpose dataframe so sample id is rownames and asv_num columns
# ID <- rownames(read) #create a new column that has rownames
# rownames(read) <- NULL #remove the actual rownames
# read <- cbind(ID, read) #merge ID column and read counts
# read <- data.frame(read)
# 



## Set the order of labels backwards for correct order output
taxa.melt$Taxa <- factor(taxa.melt$Taxa,
                             levels = c("s_Metarhizium_carneum", "s_Clonostachys_divergens", 
                                          "g_Pseudeurotium" , "g_Mortierella", "g_Clonostachys", 
                                          "f_Thelebolaceae", "f_Pseudeurotiaceae", "f_Mortierellaceae", "f_Chaetomiaceae", 
                                          "f_Bionectriaceae" , "o_Tremellales" , "o_Thelebolales","o_Mortierellales" , 
                                          "c_Tremellomycetes",    "c_Mortierellomycetes",   "p_Mortierellomycota" , 
                                          "p_Ascomycota" , "s_Phallus_impudicus",  "g_Oidiodendron" , "g_Cenococcum",
                                          "f_Phallaceae", "f_Myxotrichaceae", "f_Gloniaceae", "o_Thelephorales","o_Mytilinidales", 
                                          "o_Eurotiales",    "c_Polyangia", "p_Basidiomycota", "f_Rhodanobacteraceae", 
                                          "f_Oxalobacteraceae", "f_Caulobacteraceae",  "f_Bacillaceae", "o_Xanthomonadales", 
                                          "o_Frankiales", "c_Clostridia", "c_Acidobacteriae", "p_Acidobacteriota", "k_Archaea" , 
                                          "g_Pseudaminobacter", "g_Devosia","g_Bradyrhizobium", "f_Rhizobiaceae", "f_Nocardioidaceae", 
                                          "f_Nitrosomonadaceae", "f_Micromonosporaceae", "f_Microbacteriaceae", "o_Rhizobiales", 
                                          "o_Propionibacteriales",     "o_Polyangiales","o_Microtrichales", "o_Micromonosporales", 
                                          "o_Micrococcales", "o_Corynebacteriales", "c_Acidimicrobiia", "p_Myxococcota", 
                                          "p_Actinobacteriota"))                                    

taxa.melt$variable <- factor(taxa.melt$variable, 
                              levels = c("pl14_S_pl14p","pl14_S_pl35p","pl14_S_pl22p","pl14_S_pl27p",
                                         "pl35_S_pl14p","pl35_S_pl35p","pl35_S_pl22p","pl35_S_pl27p",
                                         "pl22_S_pl14p","pl22_S_pl35p","pl22_S_pl22p","pl22_S_pl27p",
                                         "pl27_S_pl14p","pl27_S_pl35p","pl27_S_pl22p","pl27_S_pl27p"))

fig7 <- ggplot(data = taxa.melt, aes(x = variable, 
                      y = Taxa, 
                      fill = value)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(x = variable, 
                y = Taxa, label = value), size = 1.8)+
  # coord_fixed(ratio = 0.3) +
  scale_fill_gradientn(colors = c( 
    "#FFFFCC","#FFFF99",
    "#FFFF33", 
    "orange", 
    "red"), 
    breaks = c(0,0.2, 0.5,0.7, 0.9))+
  scale_x_discrete(position = "top") +
  ylab("") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank()) +
  theme(axis.ticks = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 6 ,  
                                   face = "bold", 
                                   color = "black"),
        axis.text.y = element_text(size = 6, 
                                   face = "italic", 
                                   color = "black"),
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6), 
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-10,-10,-10,-10)) +
  guides(fill = guide_colorbar(title = "Relative abundance", 
                               title.position = "top"))

ggsave(file = "Heatmap_PLS_Taxa_09072022.pdf", 
       plot = fig7,
       width = 160,
       height = 135,
       units ="mm")
