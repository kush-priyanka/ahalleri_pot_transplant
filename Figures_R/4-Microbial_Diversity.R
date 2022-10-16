# Load libraries
library(ggplot2)
library(cowplot)

## Import microbial diversity data for plants only
div <- read.table("Data/ahalleri_clonal_Diversity_07112022.txt", 
                  header = T, row.names = 1, sep = "\t") #80 14


div$Soil_type <-factor(div$Soil_type,
                       levels = c("NM_PL14_S", "NM_PL35_S", "M_PL22_S", "M_PL27_S"))

div$Population_type <-factor(div$Population_type,
                             levels = c("NM_PL14_P", "NM_PL35_P", "M_PL22_P", "M_PL27_P"))

div <- div[,c(1:7,11,9:10, 13:14)]

#### Plots Bac/Arc & Fungal Richness 
names <- c("Number of ASVs (phylotypes)","")

div_plots <- list()
div_plots <- lapply(7:8, function(i){
  ggplot(div, aes(x = Soil_type,
                            y = div[,i], 
                            fill = Population_type)) +
    geom_boxplot(linetype = "solid", lwd = 0.0000005, outlier.size = 0.2) +
    stat_summary(fun.y = mean, geom ="point", shape = 22, size = 1, color = "black",
                 position = position_dodge2(width = 0.75,   
                                            preserve = "single")) +
    xlab("") +
    ylab(names[i-6]) + 
    
    scale_fill_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                      breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
    scale_x_discrete(labels = c("NM_PL14_S" = "NM_PL14_Soil",
                                "NM_PL35_S" = "NM_PL35_Soil",
                                "M_PL22_S" = "M_PL22_Soil",
                                "M_PL27_S" = "M_PL27_Soil")) +
    theme_classic() +
    theme(axis.text = element_text(size = 6,
                                   color = "black"),
          axis.title.y = element_text(size = 9),
          legend.position = "none") 
})


### Plot NMDS Bac/arc
bac.nmds <- ggplot(div, aes(Bac_NMDS1, Bac_NMDS2))+
  geom_point(aes(color = Population_type, shape = Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                     breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  scale_shape_manual(values = c(19, 15, 17, 7)) + 
  xlab("NMDS1") +
  ylab("NMDS2") +
  annotate( "text", x = 0.4,
            y = -0.24,
            label = "Stress = < 0.001",
            size = 3) +
  annotate( "text", x = 0.38,
            y = 0.22,
            label = bquote(R^2~'(Site type) = 0.48***'),
            size = 3) +
  annotate( "text", x = 0.25,
            y = 0.18,
            label = bquote(R^2~'(Site type X Location) = 0.38***'),
            size = 3) +
  theme_classic() +
  theme(axis.text = element_text(size = 6,
                                 color = "black"),
        axis.title.y = element_text(size = 9),
        legend.position = "none")

### Plot Fungal NMDS
fun.nmds <- ggplot(div, aes(Fun_NMDS1, Fun_NMDS2))+
  geom_point(aes(color = Population_type, shape = Soil_type), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#5E35B9", "#487DBF", "#E4300D", "#EF7B6C"),
                     breaks = c("NM_PL14_P","NM_PL35_P", "M_PL22_P", "M_PL27_P")) +
  scale_shape_manual(values = c(19, 15, 17, 7)) + 
  xlab("NMDS1") +
  ylab("NMDS2") +
  annotate( "text", x = 0.35,
            y = -0.52,
            label = "Stress = 0.024",
            size =3) +
  annotate( "text", x = 0.3,
            y = 0.4,
            label = bquote(R^2~'(Site type) = 0.29***'),
            size =3) +
  annotate( "text", x = 0.28,
            y = 0.32,
            label = bquote(R^2~'(Site type X Location) = 0.53***'),
            size =3) +
  theme_classic() +
  theme(axis.text = element_text(size = 6,
                                 color = "black"),
        axis.title.y = element_text(size = 9),
        legend.position = "none")


fig4 <- plot_grid(div_plots[[1]], div_plots[[2]],bac.nmds, fun.nmds, 
                  ncol = 2, align = "v")

#### Save the plant plot ####
ggsave(file = "Microbial_Diversity_wolegend_09142022.pdf", 
       plot = fig4,
       width = 174,
       height = 150,
       units ="mm")
