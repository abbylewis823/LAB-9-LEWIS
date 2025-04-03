# 1. Data Integration
# B. Read Data

# Reaed Inventory Data
u2_data <- read.csv(paste0("U2_2017data.csv"))

# Remove missing values in DBH and Code, keep only overstory trees
u2_data <- subset(u2_data, DBH != "" & Code != "" & Class == "O")

# Read species code data
SppCode <- read.csv(paste0("Species_Codes.csv"))

# C. Merge Datasets
trees_merge <- merge(u2_data, SppCode, by.x = "Code", by.y = "SppCode", all.x = TRUE)

# D. Create a New Dataset
trees <- trees_merge[c("Plot", "Code", "Genus", "Common.name", "DBH", "Chojnacky_Code")]

# 2. Calculations - EF, BA and TPA
# A. Calculate Expansion Factor 
plot_radius <- 58.5
plot_area_acres <- (pi * plot_radius^2) / 43560
EF <- round(1 / plot_area_acres)  # Round to nearest whole number
trees$EF <- 4

# B. Calculate Basal Area and Trees Per Acre
# Convert DBH to feet
trees$dia_ft <- as.numeric(trees$DBH) / 12

# Calculate basal area (BA)
trees$BA <- pi * (trees$dia_ft / 2)^2
trees$BA_pa <- trees$BA * trees$EF

# Trees per acre (TPA)
trees$TPA <- 1 * trees$EF

# C. Summarize Data
sum_u2_TPA <- aggregate(trees$TPA, by = list(trees$Plot), FUN = sum)
names(sum_u2_TPA) <- c("Plot", "TPA")

sum_u2_BA <- aggregate(trees$BA_pa, by = list(trees$Plot), FUN = sum)
names(sum_u2_BA) <- c("Plot", "BA")

sum_u2 <- merge(sum_u2_TPA, sum_u2_BA, by = "Plot")

max_BA_plot <- sum_u2[which.max(sum_u2$BA), "Plot"]
print(max_BA_plot)

# 3. Calculations- Biomass
# A. Load Biomass Equations 
Bm_equa <- read.csv(paste0("Biomass Equation.csv"))
Bm_equa <- Bm_equa[c('b0', 'b1', 'Chojnacky_Code')]

# B. Merge Biomass Equations with Tree Data 
trees <- merge(trees, Bm_equa, by = "Chojnacky_Code", all.x = TRUE)

# C. Calculate Biomass
trees$biomass <- exp(trees$b0 + trees$b1 * log(trees$DBH * 2.54))

# D. Filter and Aggregate Biomass
trees <- subset(trees, b0 != 0 & b1 != 0)

sum_u2_bm <- aggregate(biomass ~ Plot, data = trees, sum)
sum_u2_bm$bm_pa <- sum_u2_bm$biomass * unique(trees$EF)

sum_u2 <- merge(sum_u2, sum_u2_bm, by = "Plot")

max_bm_plot <- sum_u2[which.max(sum_u2$bm_pa), "Plot"]
print(max_bm_plot)

# 4. Calculations- Species Dominance and Richness
# A. Count Species Abundance
library(dplyr)
tree_cnt <- trees %>% group_by(Plot, Code) %>% tally()

# B. Identify Dominant Species
dom_cnt <- tree_cnt %>% group_by(Plot) %>% filter(n == max(n))

nrow(dom_cnt)
nrow(sum_u2)

# C. Calculate Total Trees and Species Richness 
tree_total <- aggregate(tree_cnt$n, by = list(tree_cnt$Plot), FUN = sum)
names(tree_total) <- c("Plot", "Ttl_Trees")

dom_cnt <- merge(dom_cnt, tree_total, by = "Plot")

tree_cnt$Species <- 1
richness <- aggregate(Species ~ Plot, data = tree_cnt, FUN = sum)
names(richness) <- c("Plot", "richness")

#6 
dom_cnt %>% filter(Plot == "D5")

# D. Calculate Relative Abundance
dom_cnt$rel_abd <- (dom_cnt$n / dom_cnt$Ttl_Trees) * 100
dom_cnt$rel_abd <- round(dom_cnt$rel_abd, 1)

#7 
tree_total %>% filter(Plot == "A5")
#8
richness %>% filter(Plot == "D1")

# 5
sum_u2 <- merge(sum_u2, dom_cnt, by = "Plot")
sum_u2 <- merge(sum_u2, richness, by = "Plot")

sum_u2 <- merge.data.frame(sum_u2, SppCode[,c('SppCode','Common.name')], by.x='Code', by.y='SppCode', all.x=T)

names(sum_u2)[names(sum_u2) == "Code"] <- "Dom_species"
names(sum_u2)[names(sum_u2) == "n"] <- "Abundance"

sum_u2$bm_tonpa <- sum_u2$bm_pa / 1000
sum_u2$Dom_species <- ifelse(sum_u2$rel_abd > 50, sum_u2$Dom_species, "Mixed")
sum_u2$Common.name <- ifelse(sum_u2$rel_abd > 50, sum_u2$Common.name, "Mixed")

sum_u2 <- distinct(sum_u2)

sum_u2 %>% filter(Plot == "D5")

# Save data set
write.csv(sum_u2, paste0("sum_u2.csv"), row.names = FALSE)

# 6 Visualization
library(ggplot2)
ggplot(sum_u2, aes(TPA)) + geom_histogram() + labs(title = "TPA Distribution")
ggplot(sum_u2, aes(BA)) + geom_histogram() + labs(title = "BA Distribution")
ggplot(sum_u2, aes(bm_tonpa)) + geom_histogram() + labs(title = "Biomass Distribution")

ggplot(sum_u2[sum_u2$Common.name != 'Mixed',], aes(fct_infreq(Common.name))) +
  geom_bar() + labs(title = "Dominant Species by Plot")

sum_u2 %>% filter(Common.name == "Sugar Maple") %>% nrow()


