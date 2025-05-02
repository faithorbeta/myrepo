install.packages("usethis")
library(usethis)
use_git_config(user.name = 'faithorbeta', user.email = 'faithmarian.orbeta@students.mq.edu.au')

install.packages("tidyverse")
library(tidyverse)

install.packages("microeco")
library(microeco)

install.packages("rstatix")
library(rstatix)


data = Metadata
head = data

data = as.data.frame(data)
colnames(data)

existing_families <- families_of_interest[families_of_interest %in% colnames(data_filtered)]


families_of_interest = c("Akkermansiaceae [Family]", "Prevotellaceae [Family]", "Bacteroidaceae [Family]", "Muribaculaceae [Family]",
                         "Marinifilaceae [Family]", "Tannerellaceae [Family]", "Rikenellaceae [Family]", "Desulfovibrionaceae [Family]",
                         "Deferribacteraceae [Family]", "Enterobacteriaceae [Family]", "Butyricicoccaceae [Family]",
                         "Peptococcaceae [Family]", "Ruminococcaceae [Family]", "Acholeplasmataceae [Family]", "Anaerovoracaceae [Family]",
                         "Eggerthellaceae [Family]", "Lachnospiraceae [Family]", "Bifidobacteriaceae [Family]", "Sutterellaceae [Family]",
                         "Monoglobaceae [Family]", "Oscillospiraceae [Family]", "Lactobacillaceae [Family]", "Enterococcaceae [Family]",
                         "Clostridiaceae [Family]", "Erysipelotrichaceae [Family]", "Erysipelatoclostridiaceae [Family]")
#######
# See what columns are available in your data
colnames(data_filtered)

# Check which of your target families actually exist
existing_families <- families_of_interest[families_of_interest %in% colnames(data_filtered)]

# And which are missing
missing_families <- setdiff(families_of_interest, existing_families)

# Print results
cat("✅ Existing families:\n")
print(existing_families)

cat("\n❌ Missing families (not found in your data):\n")
print(missing_families)



family_data = data[, c("Individual", "Timepoint", families_of_interest)]

library(tidyr)
library(dplyr)

long_data <- family_data %>%
  pivot_longer(cols = all_of(families_of_interest), names_to = "Family", values_to = "Abundance")



# Use only the families that exist
subset_data <- data_filtered[, c("SampleID", "Timepoint", "Treatment", existing_families)]

# Reshape to long format
long_data <- subset_data %>%
  pivot_longer(cols = all_of(existing_families),
               names_to = "Family",
               values_to = "Abundance")

ggplot(long_data, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Abundance of Families Pre- and Post-Treatment")

  
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

ggplot(long_data, aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_grid(Family ~ Treatment, scales = "free_y") +
  theme_minimal() +
  labs(title = "Family-level Abundance Pre- and Post-Treatment",
       x = "Timepoint", y = "Abundance")

# Plot for Metronidazole
ggplot(filter(long_data, Treatment == "Metronidazole"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Metronidazole: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")

ggsave("metronidazole_plot.png", width = 10, height = 6, dpi = 300)

# Plot for Neomycin
ggplot(filter(long_data, Treatment == "Neomycin"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Neomycin: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("neomycin_plot.png", width = 10, height = 6, dpi = 300)

# Plot for Vancomycin
ggplot(filter(long_data, Treatment == "Vancomycin"),
       aes(x = Timepoint, y = Abundance, fill = Timepoint)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~Family, scales = "free_y") +
  theme_minimal() +
  labs(title = "Vancomycin: Family Abundance Pre vs Post Treatment",
       x = "Timepoint", y = "Abundance")
ggsave("vancomycin_plot.png", width = 10, height = 6, dpi = 300)





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



#pre and post treatment

## quality control and filtering
# check for missing values (NA)
# filter low abundance
#normalisation 

## visualise 
# bar blot X-axis: Treatment Y-axis: Relative abundance (%)

#compare all treatments vs. ASV1


#separate variables, ASV, and phylum, class, order, etc into different files? 


library(ggplot2)
colnames(data)[colnames(data) == "ASV_1 [ASV]"] <- "ASV1"

hist (data$ASV1)
hist(scale(data$ASV1))

hist(data$`ASV_2 [ASV]`)
hist(scale(data$`ASV_2 [ASV]`))

hist(data$`ASV_3 [ASV]`)
hist(scale(data$`ASV_3 [ASV]`))


# Make sure Treatment is a factor to control the order on the x-axis
data$Treatment <- factor(data$Treatment, levels = unique(data$Treatment))

# Plot mean ASV1 abundance per treatment
ggplot(data, aes(x = Treatment, y = ASV1, fill = Treatment)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  ylab("Mean Relative Abundance of ASV1 (%)") +
  xlab("Treatment") +
  ggtitle("ASV1 Abundance Across Treatments") +
  theme_minimal() +
  theme(legend.position = "none")


levels(data$Treatment)

data$Timepoint <- as.factor(data$Timepoint)
levels(data$Timepoint)

# Perform a t-test between Pre and Post treatment
t_test_result <- t.test(ASV1 ~ Timepoint, data = data, subset = Timepoint %in% c("Pre-treatment", "Post-treatment"))

# Print t-test result
print(t_test_result)

# p value = 0.5397 > 0.05 
