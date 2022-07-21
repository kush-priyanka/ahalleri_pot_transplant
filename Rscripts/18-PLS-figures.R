library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)

#### PL35_P Zn ####

# pls_pl35_zn_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl35_zn_baf_final_06222022.txt", 
#                           header = T, row.names = 1, sep = "\t")
# 
# pls_pl35_zn_baf$variable <- rownames(pls_pl35_zn_baf)
# 
# 
# pls_pl35_zn_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl35_zn_plant_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl35_zn_plant$variable <- rownames(pls_pl35_zn_plant)

pls <- read.table("Results/PLS/Combined_PLS__VIP1.0_Subset_Results_06222022.txt", 
                  header=T, row.names=1, sep="\t")

pls$variable <- rownames(pls)

pls_pl35_zn_baf <- pls %>% 
  select(pl35_zn_baf_coeff, variable, Group) %>%
  filter(pl35_zn_baf_coeff != "NA")

pls_pl35_zn_plant <- pls %>% 
  select(pl35_zn_plant_coeff, variable, Group) %>%
  filter(pl35_zn_plant_coeff != "NA")


a <- ggplot(data = pls_pl35_zn_plant, 
            aes(x = variable, y = pl35_zn_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn Plant") +
  xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL35_P_Zn")

b <- ggplot(data = pls_pl35_zn_baf, 
            aes(x = variable, y = pl35_zn_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn BAF") +
  xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL35_P_Zn")


pdf("Plots/PLS/PLS_PL35_P_Zn_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a,b, ncol = 1)
dev.off()

#### PL35_P Cd ####
# pls_pl35_cd_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl35_cd_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl35_cd_baf$variable <- rownames(pls_pl35_cd_baf)
# 
# pls_pl35_cd_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl35_cd_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl35_cd_plant$variable <- rownames(pls_pl35_cd_plant)

pls_pl35_cd_baf <- pls %>% 
  select(pl35_cd_baf_coeff, variable, Group) %>%
  filter(pl35_cd_baf_coeff != "NA")

pls_pl35_cd_plant <- pls %>% 
  select(pl35_cd_plant_coeff, variable, Group) %>%
  filter(pl35_cd_plant_coeff != "NA")


a1 <- ggplot(data = pls_pl35_cd_plant, 
            aes(x = variable, y = pl35_cd_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd Plant") +
  xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL35_P_Cd")

b1 <- ggplot(data = pls_pl35_cd_baf, 
            aes(x = variable, y = pl35_cd_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd BAF") +
  xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL35_P_Cd")


pdf("Plots/PLS/PLS_PL35_P_Cd_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a1, b1, ncol = 1)
dev.off()

#### PL14_P Zn ####
# pls_pl14_zn_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl14_zn_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl14_zn_baf$variable <- rownames(pls_pl14_zn_baf)
# 
# pls_pl14_zn_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl14_zn_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl14_zn_plant$variable <- rownames(pls_pl14_zn_plant)

pls_pl14_zn_baf <- pls %>% 
  select(pl14_zn_baf_coeff, variable, Group) %>%
  filter(pl14_zn_baf_coeff != "NA")

pls_pl14_zn_plant <- pls %>% 
  select(pl14_zn_plant_coeff, variable, Group) %>%
  filter(pl14_zn_plant_coeff != "NA")


a2 <- ggplot(data = pls_pl14_zn_plant, 
            aes(x = variable, y = pl14_zn_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL14_P_Zn")

b2 <- ggplot(data = pls_pl14_zn_baf, 
            aes(x = variable, y = pl14_zn_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL14_P_Zn")


pdf("Plots/PLS/PLS_PL14_P_Zn_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a2,b2, ncol = 1)
dev.off()

#### PL14_P Cd ####
# pls_pl14_cd_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl14_cd_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl14_cd_baf$variable <- rownames(pls_pl14_cd_baf)
# 
# pls_pl14_cd_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl14_cd_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl14_cd_plant$variable <- rownames(pls_pl14_cd_plant)

pls_pl14_cd_baf <- pls %>% 
  select(pl14_cd_baf_coeff, variable, Group) %>%
  filter(pl14_cd_baf_coeff != "NA")

pls_pl14_cd_plant <- pls %>% 
  select(pl14_cd_plant_coeff, variable, Group) %>%
  filter(pl14_cd_plant_coeff != "NA")


a3 <- ggplot(data = pls_pl14_cd_plant, 
             aes(x = variable, y = pl14_cd_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL14_P_Cd")

b3 <- ggplot(data = pls_pl14_cd_baf, 
             aes(x = variable, y = pl14_cd_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL14_P_Cd")


pdf("Plots/PLS/PLS_PL14_P_Cd_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a3,b3, ncol = 1)
dev.off()

#### PL22_P Zn ####
# pls_pl22_zn_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl22_zn_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl22_zn_baf$variable <- rownames(pls_pl22_zn_baf)
# 
# pls_pl22_zn_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl22_zn_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl22_zn_plant$variable <- rownames(pls_pl22_zn_plant)

pls_pl22_zn_baf <- pls %>% 
  select(pl22_zn_baf_coeff, variable, Group) %>%
  filter(pl22_zn_baf_coeff != "NA")

pls_pl22_zn_plant <- pls %>% 
  select(pl22_zn_plant_coeff, variable, Group) %>%
  filter(pl22_zn_plant_coeff != "NA")

a4 <- ggplot(data = pls_pl22_zn_plant, 
             aes(x = variable, y = pl22_zn_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL22_P_Zn")

b4 <- ggplot(data = pls_pl22_zn_baf, 
             aes(x = variable, y = pl22_zn_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL22_P_Zn")


pdf("Plots/PLS/PLS_PL22_P_Zn_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a4,b4, ncol = 1)
dev.off()

#### PL22_P Cd ####
# pls_pl22_cd_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl22_cd_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl22_cd_baf$variable <- rownames(pls_pl22_cd_baf)
# 
# pls_pl22_cd_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl22_cd_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl22_cd_plant$variable <- rownames(pls_pl22_cd_plant)

pls_pl22_cd_baf <- pls %>% 
  select(pl22_cd_baf_coeff, variable, Group) %>%
  filter(pl22_cd_baf_coeff != "NA")

pls_pl22_cd_plant <- pls %>% 
  select(pl22_cd_plant_coeff, variable, Group) %>%
  filter(pl22_cd_plant_coeff != "NA")


a5 <- ggplot(data = pls_pl22_cd_plant, 
             aes(x = variable, y = pl22_cd_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL22_P_Cd")

b5 <- ggplot(data = pls_pl22_cd_baf, 
             aes(x = variable, y = pl22_cd_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL22_P_Cd")


pdf("Plots/PLS/PLS_PL22_P_Cd_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a5,b5, ncol = 1)
dev.off()

#### PL27_P Zn ####
# pls_pl27_zn_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl27_zn_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl27_zn_baf$variable <- rownames(pls_pl27_zn_baf)
# 
# pls_pl27_zn_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl27_zn_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl27_zn_plant$variable <- rownames(pls_pl27_zn_plant)

pls_pl27_zn_baf <- pls %>% 
  select(pl27_zn_baf_coeff, variable, Group) %>%
  filter(pl27_zn_baf_coeff != "NA")

pls_pl27_zn_plant <- pls %>% 
  select(pl27_zn_plant_coeff, variable, Group) %>%
  filter(pl27_zn_plant_coeff != "NA")

a6 <- ggplot(data = pls_pl27_zn_plant, 
             aes(x = variable, y = pl27_zn_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL27_P_Zn")

b6 <- ggplot(data = pls_pl27_zn_baf, 
             aes(x = variable, y = pl27_zn_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Zn BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL27_P_Zn")


pdf("Plots/PLS/PLS_PL27_P_Zn_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a6,b6, ncol = 1)
dev.off()

#### PL27_P Cd ####
# pls_pl27_cd_baf <- read.table("Results/PLS/final_results_VIP1.0/pls_pl27_cd_baf_final_06222022.txt", 
#                               header = T, row.names = 1, sep = "\t")
# 
# pls_pl27_cd_baf$variable <- rownames(pls_pl27_cd_baf)
# 
# pls_pl27_cd_plant <- read.table("Results/PLS/final_results_VIP1.0/pls_pl27_cd_plant_final_06222022.txt", 
#                                 header = T, row.names = 1, sep = "\t")
# 
# pls_pl27_cd_plant$variable <- rownames(pls_pl27_cd_plant)

pls_pl27_cd_baf <- pls %>% 
  select(pl27_cd_baf_coeff, variable, Group) %>%
  filter(pl27_cd_baf_coeff != "NA")

pls_pl27_cd_plant <- pls %>% 
  select(pl27_cd_plant_coeff, variable, Group) %>%
  filter(pl27_cd_plant_coeff != "NA")


a7 <- ggplot(data = pls_pl27_cd_plant, 
             aes(x = variable, y = pl27_cd_plant_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd Plant") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL27_P_Cd")

b7 <- ggplot(data = pls_pl27_cd_baf, 
             aes(x = variable, y = pl27_cd_baf_coeff, fill = Group)) +
  geom_bar(stat = "identity") +
  ylab("Std. Coefficient Cd BAF") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("PL27_P_Cd")


pdf("Plots/PLS/PLS_PL27_P_Cd_06232022.pdf", 
    width = 12, height = 8)
grid.arrange(a7,b7, ncol = 1)
dev.off()