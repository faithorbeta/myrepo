install.packages("usethis")
library(usethis)
use_git_config(user.name = 'faithorbeta', user.email = 'faithmarian.orbeta@students.mq.edu.au')

library(tidyverse)
library(microeco)
library(rstatix)


data = Metadata
data = as.data.frame(data)

existing_families <- families_of_interest[families_of_interest %in% colnames(data)]


families_of_interest = c("Akkermansiaceae [Family]", "Prevotellaceae [Family]", "Bacteroidaceae [Family]", "Muribaculaceae [Family]",
                         "Marinifilaceae [Family]", "Tannerellaceae [Family]", "Rikenellaceae [Family]", "Desulfovibrionaceae [Family]",
                         "Deferribacteraceae [Family]", "Enterobacteriaceae [Family]", "Butyricicoccaceae [Family]",
                         "Peptococcaceae [Family]", "Ruminococcaceae [Family]", "Acholeplasmataceae [Family]", "Anaerovoracaceae [Family]",
                         "Eggerthellaceae [Family]", "Lachnospiraceae [Family]", "Bifidobacteriaceae [Family]", "Sutterellaceae [Family]",
                         "Monoglobaceae [Family]", "Oscillospiraceae [Family]", "Lactobacillaceae [Family]", "Enterococcaceae [Family]",
                         "Clostridiaceae [Family]", "Erysipelotrichaceae [Family]", "Erysipelatoclostridiaceae [Family]")



family_data = data[, c("Individual", "Timepoint", families_of_interest)]

library(tidyr)
library(dplyr)

long_data <- family_data %>%
  pivot_longer(cols = all_of(families_of_interest), names_to = "Family", values_to = "Abundance")



# Use only the families that exist
subset_data <- data_filtered[, c("Individual", "Timepoint", "Treatment", existing_families)]

# Reshape to long format
long_data <- subset_data %>%
  pivot_longer(cols = all_of(existing_families),
               names_to = "Family",
               values_to = "Abundance")

treatment_plot = ggplot(long_data, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Abundance of Families Pre- and Post-Treatment")

ggsave("treatment_plot.png", plot = treatment_plot, width = 10, height = 6, dpi = 300, bg="white")

  
group_by(family, Timepoint) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE),
            sd_abundance = sd(Abundance, na.rm = TRUE),
            .groups = "drop")


#filter for pre and post
data_filtered <- data %>%
  filter(Timepoint %in% c("Pre-treatment", "Post-treatment"))


antibiotics_of_interest <- c("Metronidazole", "Neomycin", "Vancomycin")
data_filtered <- data_filtered %>%
  filter(Treatment %in% antibiotics_of_interest)  




long_data <- subset_data %>%
  pivot_longer(cols = all_of(families_of_interest),
               names_to = "Family",
               values_to = "Abundance")

family_treatment = ggplot(long_data, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_grid(Family ~ Treatment, scales = "free_y") +
  theme_minimal() +
  labs(title = "Family-level Abundance Pre- and Post-Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("family treatment.png", plot = family_treatment, width = 10, height = 6, dpi = 300, bg="white")

# Plot for Metronidazole
metro = ggplot(filter(long_data, Treatment == "Metronidazole"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Metronidazole: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")

ggsave("metronidazole_plot.png", plot = metro, width = 10, height = 6, dpi = 300, bg="white")

# Plot for Neomycin
neo = ggplot(filter(long_data, Treatment == "Neomycin"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Neomycin: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("neomycin_plot.png", plot = neo, width = 10, height = 6, dpi = 300, bg="white")

# Plot for Vancomycin
vanco = ggplot(filter(long_data, Treatment == "Vancomycin"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Vancomycin: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("vancomycin_plot.png", plot = vanco, width = 10, height = 6, dpi = 300, bg="white")


#### FOR METABOLITES
metabolites_of_interest <- c("Succinate", "Butyrate", "DMF")


# For Butyrate
data_butyrate <- data %>%
  filter(Treatment == "Butyrate") %>%
  filter(Timepoint %in% c("Pre-treatment", "Post-treatment"))

# Reshape to long format for Butyrate
long_data_butyrate <- data_butyrate %>%
  pivot_longer(cols = all_of(families_of_interest),  # Reshape for all families
               names_to = "Family",  # Create a new column for Family
               values_to = "Abundance")  # Store abundance values
# Plot for Butyrate (All Families)
butyrate = ggplot(long_data_butyrate, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +  # Facet by Family
  theme_minimal() +
  labs(title = "Butyrate: Family-level Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("butyrate_plot_all_families.png", plot = butyrate, width = 10, height = 6, dpi = 300, bg="white")


# Succinate
data_succinate <- data %>%
  filter(Treatment == "Succinate") %>%
  filter(Timepoint %in% c("Pre-treatment", "Post-treatment"))

long_data_succinate <- data_succinate %>%
  pivot_longer(cols = all_of(families_of_interest),  # Reshape for all families
               names_to = "Family",  # Create a new column for Family
               values_to = "Abundance")  # Store abundance values

# Plot for Succinate (All Families)
succinate = ggplot(long_data_succinate, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +  # Facet by Family
  theme_minimal() +
  labs(title = "Succinate: Family-level Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")

ggsave("succinate_plot_all_families.png", plot = succinate, width = 10, height = 6, dpi = 300, bg="white")


# DMF
data_dmf <- data %>%
  filter(Treatment == "DMF") %>%
  filter(Timepoint %in% c("Pre-treatment", "Post-treatment"))

long_data_dmf <- data_dmf %>%
  pivot_longer(cols = all_of(families_of_interest),  # Reshape for all families
               names_to = "Family",  # Create a new column for Family
               values_to = "Abundance")  # Store abundance values

# Plot for DMF (All Families)
dmf_plot = ggplot(long_data_dmf, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +  # Facet by Family
  theme_minimal() +
  labs(title = "DMF: Family-level Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")

ggsave("dmf_plot_all_families.png",plot = dmf_plot,  width = 10, height = 6, dpi = 300, bg="white")


## heatmap 
# https://rpubs.com/lumumba99/1026665
library(dplyr)
library(tidyr)

# Prepare data: reshape to long format
long_data <- data %>%
  filter(Timepoint %in% c("Pre-treatment", "Post-treatment")) %>%
  filter(Treatment %in% c("Vancomycin", "Neomycin", "Metronidazole", "DMF", "Butyrate", "Succinate")) %>%
  pivot_longer(cols = all_of(families_of_interest),
               names_to = "Family",
               values_to = "Abundance")

# Compute mean pre- and post-treatment abundances for each family and treatment
fold_change_df <- long_data %>%
  group_by(Treatment, Family, Timepoint) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Timepoint, values_from = mean_abundance) %>%
  mutate(log2FC = log2((`Post-treatment` + 1e-6) / (`Pre-treatment` + 1e-6)))  # Add small constant to avoid log(0)

library(ggplot2)

heatmap = ggplot(fold_change_df, aes(x = Treatment, y = Family, fill = log2FC)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "log2FC") +
  theme_minimal() +
  labs(title = "Log2 Fold Change in Abundance (Post vs Pre)",
       x = "Treatment", y = "Family") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("heatmap.png", plot = heatmap,width = 10, height = 6, dpi = 300, bg="white")

# Color scale:Red = increased abundance post-treatment; Blue = decreased abundance post-treatment; 
#White/neutral = little to no change

#t-test for metronidazole timepoint 
# Function: Paired t-test per family for Metronidazole
run_ttest_by_family <- function(data, families, treatment_name) {
  df <- data %>%
    filter(Treatment == treatment_name) %>%
    filter(Timepoint %in% c("Pre-treatment", "Post-treatment")) %>%
    select(Individual, Timepoint, all_of(families)) %>%
    pivot_longer(cols = all_of(families), names_to = "Family", values_to = "Abundance") %>%
    pivot_wider(names_from = Timepoint, values_from = Abundance)
  
  results <- df %>%
    group_by(Family) %>%
    summarise(
      p_value = tryCatch(
        t.test(`Pre-treatment`, `Post-treatment`, paired = TRUE)$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(p_adj = p.adjust(p_value, method = "fdr"))
  
  return(results)
}

# Run for Metronidazole
ttest_results_metronidazole <- run_ttest_by_family(data, families_of_interest, "Metronidazole")

# View the results
print(ttest_results_metronidazole)


library(dplyr)
library(tidyr)

# Function to run Wilcoxon signed-rank test per family
run_wilcoxon_by_family <- function(data, families, treatment_name) {
  # Filter data to the selected treatment and timepoints
  df <- data %>%
    filter(Treatment == treatment_name) %>%
    filter(Timepoint %in% c("Pre-treatment", "Post-treatment")) %>%
    select(Individual, Timepoint, all_of(families)) %>%
    pivot_longer(cols = all_of(families), names_to = "Family", values_to = "Abundance") %>%
    pivot_wider(names_from = Timepoint, values_from = Abundance)
  
  # Run test for each family
  results <- df %>%
    group_by(Family) %>%
    summarise(
      p_value = tryCatch(
        wilcox.test(`Pre-treatment`, `Post-treatment`, paired = TRUE)$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(p_adj = p.adjust(p_value, method = "fdr"))
  
  return(results)
}

wilcoxon_results <- run_wilcoxon_by_family(data, families_of_interest, treatment_name = "Vancomycin")
print(wilcoxon_results)

#calculate alpha diversity 
library(vegan)
if(!requireNamespace("BiocManager")){
  install.packages("BiocManager")
}
BiocManager::install("phyloseq")
library(phyloseq)
library(tidyverse)
library(patchwork)
library(agricolae)
library(FSA)
library(rcompanion)

data_otu <- read.table("data_loue_16S_nonnorm.txt", header = TRUE)
data_grp <- read.table("data_loue_16S_nonnorm_grp.txt", header=TRUE, stringsAsFactors = TRUE)
data_taxo <- read.table("data_loue_16S_nonnorm_taxo.txt", header = TRUE)

treatment = data$Treatment

sum(is.na(data))           # Total NAs
colSums(is.na(data))       # NAs per column

#is there a difference in pre-treatment and post-treatment in ASV1


#corresponse analysis

library(vegan)

cascores = cca(ASV)$CA$u

plot(cascores)

which(cascores[,1]>4)

#vancomycin post treatment not good ..

which(cascores[,2]>3.5)
#metronidazole post treatment good

#PCoA principal coordinates analysis

pcoascores = cmdscale(vegdist(ASV),k=10)
plot(pcoascores)

which(pcoascores[,1]>.6)
which(pcoascores[,1]>.25 & pcoascores[,1]<.6)

cmdscale(vegdist(ASV),eig=TRUE)

#regression 

summary(lm(pcoascores[,1] ~ data$Treatment))
summary(lm(pcoascores[,1] ~ data$Timepoint))

#pretreatment less bacteria

#intrxn
summary(lm(pcoascores[,1] ~ data$Timepoint*data$Treatment))
#lower abundance after treatment

summary(lm(pcoascores[,2] ~ data$Timepoint*data$Treatment))
#metro has high and vanco has low 

summary(lm (as.integer(data$Timepoint)~pcoascores[,1:4]))
#diff group of bacteria that can predict whether they have been treated or not

#bubbleplot 

#timepoint

plot(pcoascores, cex = as.integer(data$Timepoint)/1.5, col="gray80",bg = hsv(h=as.integer(data$Treatment)/8), pch = 21, 
     xlab = "PCoA axis 1", ylab = "PCoA axis 2", xaxs="i", yaxs ="i", bty = "l")

plot(cascores, cex = as.integer(data$Timepoint)/1.5, col="gray80",bg = hsv(h=as.integer(data$Treatment)/9), pch = 21, 
     xlab = "PCoA axis 1", ylab = "PCoA axis 2", xaxs="i", yaxs ="i", bty = "l")
#small = post


#violin plot 

library(vioplot)


#treatment for pre and treatment for post


vioplot(pcoascores[,1]~data$Treatment, col = hsv(1:8/9))
t= which(data$Timepoint=="Post-treatment")
vioplot(pcoascores[t,1]~data$Treatment[t], col = hsv(1:8/9))

t1= which(data$Timepoint=="Pre-treatment")
vioplot(pcoascores[t1,1]~data$Treatment[t1], col = hsv(1:8/9))




table(data$Treatment)

# Alpha diversity ####
microeco = `Faith microeco data`

microeco$cal_alphadiv()

