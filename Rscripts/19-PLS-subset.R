## Subset data for PLS files
library(dplyr)

#### PL35_P_Zn ####
## Import VIP file
pl35_zn_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_Zn_VIP.txt", 
                   header = T, row.names = 1, sep = "\t")
pl35_zn_vip  <- pl35_zn_vip[order(row.names(pl35_zn_vip)), ] 

## Import Std coeffiencient for Zn_plant
pl35_zn_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_std_Zn_plant.txt", 
                   header = T, row.names = 1, sep = "\t")
dim(pl35_zn_plant)

pl35_zn_plant <- pl35_zn_plant[order(row.names(pl35_zn_plant)), ] 
colnames(pl35_zn_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                         "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Zn_BAF
pl35_zn_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_std_Zn_BAF.txt", 
                        header = T, row.names = 1, sep = "\t")

pl35_zn_baf  <- pl35_zn_baf[order(row.names(pl35_zn_baf)), ] 
colnames(pl35_zn_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                         "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Zn plant/BAF std coeffcients
all(rownames(pl35_zn_vip)== rownames(pl35_zn_baf))
all(rownames(pl35_zn_vip)== rownames(pl35_zn_plant))

## Merge the three files
pls_pl35_zn <- cbind(pl35_zn_vip, pl35_zn_plant, pl35_zn_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl35_zn_plant.sub <- pls_pl35_zn %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
  -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl35_zn_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl35_zn_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl35_zn_baf.sub <- pls_pl35_zn %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl35_zn_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl35_zn_baf_final_06222022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)


#### PL35_P_Cd ####
## Import VIP file
pl35_cd_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_Cd_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")

pl35_cd_vip  <- pl35_cd_vip[order(row.names(pl35_cd_vip)), ] 

## Import Std coeffiencient for Cd_plant
pl35_cd_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_std_Cd_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl35_cd_plant)

pl35_cd_plant <- pl35_cd_plant[order(row.names(pl35_cd_plant)), ] 
colnames(pl35_cd_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Cd_BAF
pl35_cd_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL35_P_std_Cd_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl35_cd_baf  <- pl35_cd_baf[order(row.names(pl35_cd_baf)), ] 
colnames(pl35_cd_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Cd plant/BAF std coeffcients
all(rownames(pl35_cd_vip) == rownames(pl35_cd_baf))
all(rownames(pl35_cd_vip) == rownames(pl35_cd_plant))

## Merge the three files
pls_pl35_cd <- cbind(pl35_cd_vip, pl35_cd_plant, pl35_cd_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl35_cd_plant.sub <- pls_pl35_cd %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl35_cd_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl35_cd_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl35_cd_baf.sub <- pls_pl35_cd %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl35_cd_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl35_cd_baf_final_06222022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)

#### PL14_P_Zn ####
## Import VIP file
pl14_zn_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_Zn_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl14_zn_vip  <- pl14_zn_vip[order(row.names(pl14_zn_vip)), ] 

## Import Std coeffiencient for Zn_plant
pl14_zn_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_std_Zn_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl14_zn_plant)

pl14_zn_plant <- pl14_zn_plant[order(row.names(pl14_zn_plant)), ] 
colnames(pl14_zn_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Zn_BAF
pl14_zn_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_std_Zn_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl14_zn_baf  <- pl14_zn_baf[order(row.names(pl14_zn_baf)), ] 
colnames(pl14_zn_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Zn plant/BAF std coeffcients
all(rownames(pl14_zn_vip) == rownames(pl14_zn_baf))
all(rownames(pl14_zn_vip) == rownames(pl14_zn_plant))

## Merge the three files
pls_pl14_zn <- cbind(pl14_zn_vip, pl14_zn_plant, pl14_zn_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl14_zn_plant.sub <- pls_pl14_zn %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl14_zn_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl14_zn_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl14_zn_baf.sub <- pls_pl14_zn %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl14_zn_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl14_zn_baf_final_06222022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)

#### PL14_P_Cd ####
## Import VIP file
pl14_cd_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_Cd_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl14_cd_vip  <- pl14_cd_vip[order(row.names(pl14_cd_vip)), ] 

## Import Std coeffiencient for Cd_plant
pl14_cd_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_std_Cd_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl14_cd_plant)

pl14_cd_plant <- pl14_cd_plant[order(row.names(pl14_cd_plant)), ] 
colnames(pl14_cd_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Cd_BAF
pl14_cd_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL14_P_std_Cd_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl14_cd_baf  <- pl14_cd_baf[order(row.names(pl14_cd_baf)), ] 
colnames(pl14_cd_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Cd plant/BAF std coeffcients
all(rownames(pl14_cd_vip) == rownames(pl14_cd_baf))
all(rownames(pl14_cd_vip) == rownames(pl14_cd_plant))

## Merge the three files
pls_pl14_cd <- cbind(pl14_cd_vip, pl14_cd_plant, pl14_cd_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl14_cd_plant.sub <- pls_pl14_cd %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl14_cd_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl14_cd_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl14_cd_baf.sub <- pls_pl14_cd %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant ,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl14_cd_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl14_cd_baf_final_06222022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)

#### PL22_P_Zn ####
## Import VIP file
pl22_zn_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_Zn_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl22_zn_vip  <- pl22_zn_vip[order(row.names(pl22_zn_vip)), ] 

## Import Std coeffiencient for Zn_plant
pl22_zn_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_std_Zn_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl22_zn_plant)

pl22_zn_plant <- pl22_zn_plant[order(row.names(pl22_zn_plant)), ] 
colnames(pl22_zn_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Zn_BAF
pl22_zn_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_std_Zn_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl22_zn_baf  <- pl22_zn_baf[order(row.names(pl22_zn_baf)), ] 
colnames(pl22_zn_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Zn plant/BAF std coeffcients
all(rownames(pl22_zn_vip) == rownames(pl22_zn_baf))
all(rownames(pl22_zn_vip) == rownames(pl22_zn_plant))

## Merge the three files
pls_pl22_zn <- cbind(pl22_zn_vip, pl22_zn_plant, pl22_zn_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl22_zn_plant.sub <- pls_pl22_zn %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl22_zn_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl22_zn_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl22_zn_baf.sub <- pls_pl22_zn %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl22_zn_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl22_zn_baf_final_06222022.txt",
            sep="\t", quote = F, row.names = T, col.names = NA)

#### PL22_P_Cd ####
## Import VIP file
pl22_cd_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_Cd_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl22_cd_vip  <- pl22_cd_vip[order(row.names(pl22_cd_vip)), ] 

## Import Std coeffiencient for Cd_plant
pl22_cd_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_std_Cd_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl22_cd_plant)

pl22_cd_plant <- pl22_cd_plant[order(row.names(pl22_cd_plant)), ] 
colnames(pl22_cd_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Cd_BAF
pl22_cd_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL22_P_std_Cd_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl22_cd_baf  <- pl22_cd_baf[order(row.names(pl22_cd_baf)), ] 
colnames(pl22_cd_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Cd plant/BAF std coeffcients
all(rownames(pl22_cd_vip) == rownames(pl22_cd_baf))
all(rownames(pl22_cd_vip) == rownames(pl22_cd_plant))

## Merge the three files
pls_pl22_cd <- cbind(pl22_cd_vip, pl22_cd_plant, pl22_cd_baf) #182 16

## Filter VIP1 or VIP 2 > 0.8 & Std coeffcient values
## Plant hyperaccumulation
pls_pl22_cd_plant.sub <- pls_pl22_cd %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl22_cd_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl22_cd_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl22_cd_baf.sub <- pls_pl22_cd %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl22_cd_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl22_cd_baf_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### PL27_P_Zn ####
## Import VIP file
pl27_zn_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_Zn_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl27_zn_vip  <- pl27_zn_vip[order(row.names(pl27_zn_vip)), ] 

## Import Std coeffiencient for Zn_plant
pl27_zn_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_std_Zn_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl27_zn_plant)

pl27_zn_plant <- pl27_zn_plant[order(row.names(pl27_zn_plant)), ] 
colnames(pl27_zn_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Zn_BAF
pl27_zn_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_std_Zn_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl27_zn_baf  <- pl27_zn_baf[order(row.names(pl27_zn_baf)), ] 
colnames(pl27_zn_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Zn plant/BAF std coeffcients
all(rownames(pl27_zn_vip) == rownames(pl27_zn_baf))
all(rownames(pl27_zn_vip) == rownames(pl27_zn_plant))

## Merge the three files
pls_pl27_zn <- cbind(pl27_zn_vip, pl27_zn_plant, pl27_zn_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl27_zn_plant.sub <- pls_pl27_zn %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl27_zn_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl27_zn_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl27_zn_baf.sub <- pls_pl27_zn %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl27_zn_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl27_zn_baf_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### PL27_P_Cd ####
## Import VIP file
pl27_cd_vip <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_Cd_VIP.txt", 
                          header = T, row.names = 1, sep = "\t")
pl27_cd_vip  <- pl27_cd_vip[order(row.names(pl27_cd_vip)), ] 

## Import Std coeffiencient for Cd_plant
pl27_cd_plant <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_std_Cd_plant.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(pl27_cd_plant)

pl27_cd_plant <- pl27_cd_plant[order(row.names(pl27_cd_plant)), ] 
colnames(pl27_cd_plant) <- c("Coefficient_plant", "Std_deviation_plant",    
                             "Lower_bound_plant","Upper_bound_plant")

## Import Std coeffiencient for Cd_BAF
pl27_cd_baf <- read.table("Results/PLS/pls_files_for_subset/PLS_PL27_P_std_Cd_BAF.txt", 
                          header = T, row.names = 1, sep = "\t")

pl27_cd_baf  <- pl27_cd_baf[order(row.names(pl27_cd_baf)), ] 
colnames(pl27_cd_baf) <- c("Coefficient_baf", "Std_deviation_baf",    
                           "Lower_bound_baf","Upper_bound_baf")

## Check if rownames match for VIP, Cd plant/BAF std coeffcients
all(rownames(pl27_cd_vip) == rownames(pl27_cd_baf))
all(rownames(pl27_cd_vip) == rownames(pl27_cd_plant))

## Merge the three files
pls_pl27_cd <- cbind(pl27_cd_vip, pl27_cd_plant, pl27_cd_baf) #182 16

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_pl27_cd_plant.sub <- pls_pl27_cd %>%
  select(-Coefficient_baf,-Std_deviation_baf, 
         -Lower_bound_baf,-Upper_bound_baf) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_plant > 0 & Upper_bound_plant > 0 |
           Lower_bound_plant < 0 & Upper_bound_plant < 0)

write.table(pls_pl27_cd_plant.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl27_cd_plant_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## BAF
pls_pl27_cd_baf.sub <- pls_pl27_cd %>%
  select(-Coefficient_plant,-Std_deviation_plant, 
         -Lower_bound_plant,-Upper_bound_plant) %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_baf > 0 & Upper_bound_baf > 0 |
           Lower_bound_baf < 0 & Upper_bound_baf < 0)

write.table(pls_pl27_cd_baf.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_pl27_cd_baf_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

##### Merge BAF files #####
dim(pls_pl35_cd_baf.sub) #63 12
dim(pls_pl14_cd_baf.sub) #41 12
dim(pls_pl22_cd_baf.sub) #58 12
dim(pls_pl27_cd_baf.sub) #54 12

pls_pl35_cd_baf.sub$variable <- rownames(pls_pl35_cd_baf.sub)
pls_pl14_cd_baf.sub$variable <- rownames(pls_pl14_cd_baf.sub)
pls_pl22_cd_baf.sub$variable <- rownames(pls_pl22_cd_baf.sub)
pls_pl27_cd_baf.sub$variable <- rownames(pls_pl27_cd_baf.sub)

pls_pl35_cd_baf.sub1 <- pls_pl35_cd_baf.sub[, c(9,13)]
pls_pl14_cd_baf.sub1 <- pls_pl14_cd_baf.sub[, c(9,13)]
pls_pl22_cd_baf.sub1 <- pls_pl22_cd_baf.sub[, c(9,13)]
pls_pl27_cd_baf.sub1 <- pls_pl27_cd_baf.sub[, c(9,13)]

colnames(pls_pl35_cd_baf.sub1) <- c("pl35_cd_baf_coeff", "variable")
colnames(pls_pl14_cd_baf.sub1) <- c("pl14_cd_baf_coeff", "variable")
colnames(pls_pl22_cd_baf.sub1) <- c("pl22_cd_baf_coeff", "variable")
colnames(pls_pl27_cd_baf.sub1) <- c("pl27_cd_baf_coeff", "variable")

cd_baf_merge <- merge(pls_pl14_cd_baf.sub1, 
                      pls_pl35_cd_baf.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

cd_baf_merge <- merge(cd_baf_merge, 
                      pls_pl22_cd_baf.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

cd_baf_merge <- merge(cd_baf_merge, 
                      pls_pl27_cd_baf.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

write.table(cd_baf_merge, 
            file = "Results/PLS/final_results_VIP1.0/cd_baf_merge_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

dim(pls_pl35_zn_baf.sub) #43 12
dim(pls_pl14_zn_baf.sub) #39 12
dim(pls_pl22_zn_baf.sub) #59 12
dim(pls_pl27_zn_baf.sub) #34 12

pls_pl35_zn_baf.sub$variable <- rownames(pls_pl35_zn_baf.sub)
pls_pl14_zn_baf.sub$variable <- rownames(pls_pl14_zn_baf.sub)
pls_pl22_zn_baf.sub$variable <- rownames(pls_pl22_zn_baf.sub)
pls_pl27_zn_baf.sub$variable <- rownames(pls_pl27_zn_baf.sub)

pls_pl35_zn_baf.sub1 <- pls_pl35_zn_baf.sub[, c(9,13)]
pls_pl14_zn_baf.sub1 <- pls_pl14_zn_baf.sub[, c(9,13)]
pls_pl22_zn_baf.sub1 <- pls_pl22_zn_baf.sub[, c(9,13)]
pls_pl27_zn_baf.sub1 <- pls_pl27_zn_baf.sub[, c(9,13)]

colnames(pls_pl35_zn_baf.sub1) <- c("pl35_zn_baf_coeff", "variable")
colnames(pls_pl14_zn_baf.sub1) <- c("pl14_zn_baf_coeff", "variable")
colnames(pls_pl22_zn_baf.sub1) <- c("pl22_zn_baf_coeff", "variable")
colnames(pls_pl27_zn_baf.sub1) <- c("pl27_zn_baf_coeff", "variable")

zn_baf_merge <- merge(pls_pl14_zn_baf.sub1, 
                  pls_pl35_zn_baf.sub1, 
                   by = "variable",
                  all.x = TRUE,
                  all.y = TRUE)

zn_baf_merge <- merge(zn_baf_merge, 
                      pls_pl22_zn_baf.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

zn_baf_merge <- merge(zn_baf_merge, 
                      pls_pl27_zn_baf.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

cd_zn_baf_merge <- merge(cd_baf_merge,
                         zn_baf_merge, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

write.table(zn_baf_merge, 
            file = "Results/PLS/final_results_VIP1.0/zn_baf_merge_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

##### Merge Plant files #####
dim(pls_pl35_cd_plant.sub) #46 12
dim(pls_pl14_cd_plant.sub) #51 12
dim(pls_pl22_cd_plant.sub) #59 12
dim(pls_pl27_cd_plant.sub) #37 12

pls_pl35_cd_plant.sub$variable <- rownames(pls_pl35_cd_plant.sub)
pls_pl14_cd_plant.sub$variable <- rownames(pls_pl14_cd_plant.sub)
pls_pl22_cd_plant.sub$variable <- rownames(pls_pl22_cd_plant.sub)
pls_pl27_cd_plant.sub$variable <- rownames(pls_pl27_cd_plant.sub)

pls_pl35_cd_plant.sub1 <- pls_pl35_cd_plant.sub[, c(9,13)]
pls_pl14_cd_plant.sub1 <- pls_pl14_cd_plant.sub[, c(9,13)]
pls_pl22_cd_plant.sub1 <- pls_pl22_cd_plant.sub[, c(9,13)]
pls_pl27_cd_plant.sub1 <- pls_pl27_cd_plant.sub[, c(9,13)]

colnames(pls_pl35_cd_plant.sub1) <- c("pl35_cd_plant_coeff", "variable")
colnames(pls_pl14_cd_plant.sub1) <- c("pl14_cd_plant_coeff", "variable")
colnames(pls_pl22_cd_plant.sub1) <- c("pl22_cd_plant_coeff", "variable")
colnames(pls_pl27_cd_plant.sub1) <- c("pl27_cd_plant_coeff", "variable")

cd_plant_merge <- merge(pls_pl35_cd_plant.sub1, 
                      pls_pl14_cd_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

cd_plant_merge <- merge(cd_plant_merge, 
                      pls_pl22_cd_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

cd_plant_merge <- merge(cd_plant_merge, 
                      pls_pl27_cd_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)
write.table(cd_baf_merge, 
            file = "Results/PLS/final_results_VIP1.0/cd_plant_merge_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

dim(pls_pl35_zn_plant.sub) #57 12
dim(pls_pl14_zn_plant.sub) #60 12
dim(pls_pl22_zn_plant.sub) #54 12
dim(pls_pl27_zn_plant.sub) #29 12

pls_pl35_zn_plant.sub$variable <- rownames(pls_pl35_zn_plant.sub)
pls_pl14_zn_plant.sub$variable <- rownames(pls_pl14_zn_plant.sub)
pls_pl22_zn_plant.sub$variable <- rownames(pls_pl22_zn_plant.sub)
pls_pl27_zn_plant.sub$variable <- rownames(pls_pl27_zn_plant.sub)

pls_pl35_zn_plant.sub1 <- pls_pl35_zn_plant.sub[, c(9,13)]
pls_pl14_zn_plant.sub1 <- pls_pl14_zn_plant.sub[, c(9,13)]
pls_pl22_zn_plant.sub1 <- pls_pl22_zn_plant.sub[, c(9,13)]
pls_pl27_zn_plant.sub1 <- pls_pl27_zn_plant.sub[, c(9,13)]

colnames(pls_pl35_zn_plant.sub1) <- c("pl35_zn_plant_coeff", "variable")
colnames(pls_pl14_zn_plant.sub1) <- c("pl14_zn_plant_coeff", "variable")
colnames(pls_pl22_zn_plant.sub1) <- c("pl22_zn_plant_coeff", "variable")
colnames(pls_pl27_zn_plant.sub1) <- c("pl27_zn_plant_coeff", "variable")

zn_plant_merge <- merge(pls_pl14_zn_plant.sub1, 
                      pls_pl35_zn_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

zn_plant_merge <- merge(zn_plant_merge, 
                      pls_pl22_zn_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

zn_plant_merge <- merge(zn_plant_merge, 
                      pls_pl27_zn_plant.sub1, 
                      by = "variable",
                      all.x = TRUE,
                      all.y = TRUE)

write.table(zn_plant_merge, 
            file = "Results/PLS/final_results_VIP1.0/zn_plant_merge_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

cd_zn_plant_merge <- merge(cd_plant_merge,
                         zn_plant_merge, 
                         by = "variable",
                         all.x = TRUE,
                        all.y = TRUE)

baf_plant <- merge(cd_zn_baf_merge,
                   cd_zn_plant_merge, 
                   by = "variable",
                   all.x = TRUE,
                   all.y = TRUE)

write.table(baf_plant, 
            file = "Results/PLS/final_results_VIP1.0/baf_plant_merge_final_06222022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

#### Import files for Zn and Cd all models ####
## Import Zn VIP file
zn_vip <- read.table("Results/PLS/pls_files_for_subset/Zn_P_all_VIP_07142022.txt", 
                          header = T, row.names = 1, sep = "\t")
zn_vip  <- zn_vip[order(row.names(zn_vip)), ] 

## Import Std coeffiencient for Zn
zn_coeff <- read.table("Results/PLS/pls_files_for_subset/Zn_P_all_stdcoeff_07142022.txt", 
                            header = T, row.names = 1, sep = "\t")
dim(zn_coeff)

zn_coeff <- zn_coeff[order(row.names(zn_coeff)), ] 

## Check if rownames match for VIP & std coeffcients
all(rownames(zn_vip)== rownames(zn_coeff))

## Merge the three files
pls_zn <- cbind(zn_vip, zn_coeff) #189 12

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_zn.sub <- pls_zn %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_coeff > 0 & Upper_bound_coeff > 0 |
           Lower_bound_coeff < 0 & Upper_bound_coeff < 0)

write.table(pls_zn.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_zn_all_final_07142022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)

## Import Cd VIP file
cd_vip <- read.table("Results/PLS/pls_files_for_subset/Cd_P_all_VIP_07142022.txt", 
                     header = T, row.names = 1, sep = "\t")
cd_vip  <- cd_vip[order(row.names(cd_vip)), ] 

## Import Std coeffiencient for Cd
cd_coeff <- read.table("Results/PLS/pls_files_for_subset/Cd_P_all_stdcoeff_07142022.txt", 
                       header = T, row.names = 1, sep = "\t")
dim(cd_coeff)

cd_coeff <- cd_coeff[order(row.names(cd_coeff)), ] 

## Check if rownames match for VIP & std coeffcients
all(rownames(cd_vip)== rownames(cd_coeff))

## Merge the three files
pls_cd <- cbind(cd_vip, cd_coeff) #189 12

## Filter VIP1 or VIP 2 > 1.0 & Std coeffcient values
## Plant hyperaccumulation
pls_cd.sub <- pls_cd %>%
  filter(VIP1 > 1.0 | VIP2 > 1.0) %>%
  filter(Lower_bound_coeff > 0 & Upper_bound_coeff > 0 |
           Lower_bound_coeff < 0 & Upper_bound_coeff < 0)

write.table(pls_cd.sub, 
            file = "Results/PLS/final_results_VIP1.0/pls_cd_all_final_07142022.txt",
            sep = "\t", quote = F, row.names = T, col.names = NA)