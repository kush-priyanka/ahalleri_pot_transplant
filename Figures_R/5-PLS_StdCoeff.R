## Load libraries
library(ggplot2)
library(tidyverse)
library(ggtext)
library(cowplot)
library(ggpattern)

##Import file

cd.pls <- read.table("Results/PLS/final_results_VIP1.0/Cd_P_all_stdcoeff_group_07142022.txt", 
                  header=T, row.names=1, sep="\t")

zn.pls <- read.table("Results/PLS/final_results_VIP1.0/Zn_P_all_stdcoeff_group_07142022.txt", 
                     header=T, row.names=1, sep="\t")

cd.pls$Variable <- rownames(cd.pls)
zn.pls$Variable <- rownames(zn.pls)


cd.pls$Order <-  with(cd.pls, ifelse(cd.pls$Coefficient > 0,  "Pos", "Neg"))

cd.pls$Order <- factor(cd.pls$Order, 
                               levels = c("Pos", "Neg"))
cd.plot <- cd.pls %>%
  mutate(Variable = fct_reorder(Variable, Coefficient, .desc = TRUE),
         label_y = if_else(Order == "Pos", -0.00001, 0.00001),
         label_hjust = if_else(Order  == "Pos", 1, 0)) %>%
  ggplot(aes(x = Variable,
             y = Coefficient,
             label = Variable, 
             fill = Category)) +
  geom_bar_pattern(aes(pattern = Category), width = 0.7,
                   stat = "identity", color = "black", lwd = 0.05,
                   pattern_color = "darkgray", 
                   pattern_fill = "black") +
  geom_richtext(aes(y = label_y,
                    hjust = label_hjust),
                angle = 90,
                fill = NA,
                label.color = NA,
                size = 2) +
  labs(y = "St. coeff. Cdshoot", x = "") +
  scale_fill_manual(values = c("#EE7C00", "#FFF6B1", "#EE7C00",  "#FFF6B1","#A4BE19", "#A0D9F7")) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(size = 9))

###Zn PLS 
zn.pls$Order <-  with(zn.pls, ifelse(zn.pls$Coefficient > 0,  "Pos", "Neg"))

zn.pls$Order <- factor(zn.pls$Order, 
                       levels = c("Pos", "Neg"))
zn.plot <- zn.pls %>%
  mutate(Variable = fct_reorder(Variable, Coefficient, .desc = TRUE),
         label_y = if_else(Order == "Pos", -0.00001, 0.00001),
         label_hjust = if_else(Order  == "Pos", 1, 0)) %>%
  ggplot(aes(x = Variable,
             y = Coefficient,
             label = Variable, 
             fill = Category)) +
  geom_bar_pattern(stat = "identity", color = "black",
                   width = 0.7,lwd = 0.05,
                   pattern_color = "darkgray", 
                   pattern_fill = "black", 
                   aes(pattern = Category)) +
  geom_richtext(aes(y = label_y,
                    hjust = label_hjust),
                angle = 90,
                fill = NA,
                label.color = NA,
                size = 2) +
  labs(y = "St. coeff. Znshoot", x = "") +
  scale_fill_manual(values = c("#EE7C00", "#FFF6B1", "#EE7C00","#FFF6B1","#A4BE19", "#A0D9F7")) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(size = 9))

fig5 <- plot_grid(cd.plot, zn.plot,
                  ncol = 1, align = "v")

#### Save the plant plot ####
# ggsave(file = "PLS_Std_Cd_08152022.pdf", 
#        plot = cd.plot,
#        width = 160,
#        height = 80,
#        units ="mm")
# 
# ggsave(file = "PLS_Std_Zn_08152022.pdf", 
#        plot = zn.plot,
#        width = 174,
#        height = 80,
#        units ="mm")

ggsave(file = "PLS_Std_08172022.pdf", 
       plot = fig5,
       width = 174,
       height = 120,
       units ="mm")