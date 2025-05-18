install.packages("usethis")
library(usethis)
use_git_config(user.name = 'faithorbeta', user.email = 'faithmarian.orbeta@students.mq.edu.au')

library(tidyverse)
library(microeco)
library(rstatix)

library(lme4)
library(MuMIn)
library(lmerTest)

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

#t-test for treatment vs. timepoint ####
# Function: Paired t-test per family 
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
print(ttest_results_metronidazole)
# Not significant: Acholeplasmataceae [Family], Bacteroidaceae [Family], Muribaculaceae [Family], Rikenellaceae [Family] 

# Run for Vancomycin
ttest_results_vancomycin <- run_ttest_by_family(data, families_of_interest, "Vancomycin")
print(ttest_results_vancomycin)
#Not significant: Clostridiaceae [Family], Erysipelatoclostridiaceae [Family], Lactobacillaceae [Family], Peptococcaceae [Family], Tannerellaceae [Family] 


#Run for Neomycin
ttest_results_neomycin <- run_ttest_by_family(data, families_of_interest, "Neomycin")
print(ttest_results_neomycin)
#Significant: Akkermansiaceae [Family], Bacteroidaceae [Family], Bifidobacteriaceae [Family], Deferribacteraceae [Family], Desulfovibrionaceae [Family], 
#Eggerthellaceae [Family], Enterococcaceae [Family], Erysipelotrichaceae [Family], Lachnospiraceae [Family], Lactobacillaceae [Family], Monoglobaceae [Family], Tannerellaceae [Family] 

#Run for Succinate
ttest_results_succinate <- run_ttest_by_family(data, families_of_interest, "Succinate")
print(ttest_results_succinate)
#Significant: Butyricicoccaceae [Family], Lachnospiraceae [Family], Oscillospiraceae [Family],  Prevotellaceae [Family], Rikenellaceae [Family], Tannerellaceae [Family] 

#Run for Butyrate
ttest_results_butyrate <- run_ttest_by_family(data, families_of_interest, "Butyrate")
print(ttest_results_butyrate)
# No significant difference in any family 

#Run for DMF
ttest_results_dmf <- run_ttest_by_family(data, families_of_interest, "DMF")
print(ttest_results_dmf)
# Significant:  Eggerthellaceae [Family] 


#wilcoxon for treatment vs. timepoint ####
# Function: wilcoxon per family 
run_wilcox_by_family <- function(data, families, treatment_name) {
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
        wilcox.test(`Pre-treatment`, `Post-treatment`, paired = TRUE)$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(p_adj = p.adjust(p_value, method = "fdr"))
  
  return(results)
}

# Run for Metronidazole
wilcox_results_metronidazole <- run_wilcox_by_family(data, families_of_interest, "Metronidazole")
print(wilcox_results_metronidazole)

# Run for Vancomycin
wilcox_results_vancomycin <- run_wilcox_by_family(data, families_of_interest, "Vancomycin")
print(wilcox_results_vancomycin)

#Run for Neomycin
wilcox_results_neomycin <- run_wilcox_by_family(data, families_of_interest, "Neomycin")
print(wilcox_results_neomycin)

#Run for Succinate
wilcox_results_succinate <- run_wilcox_by_family(data, families_of_interest, "Succinate")
print(wilcox_results_succinate)

#Run for Butyrate
wilcox_results_butyrate <- run_wilcox_by_family(data, families_of_interest, "Butyrate")
print(wilcox_results_butyrate)


#Run for DMF
wilcox_results_dmf <- run_wilcox_by_family(data, families_of_interest, "DMF")
print(wilcox_results_dmf)





#correspondense analysis ####

library(vegan) 

cascores = cca(ASV)$CA$u 
#sample scores; 191 rows = 191 samples

plot(cascores)
#12 samples outliers from 5 onwards (x axis), we want to know what those are, use which command
#6 samples outliers from 4 onwards (y axis), use which function again

which(cascores[,1]>4)
#180-191 which is post-treatment vancomycin from metadata 
#bacteria not happy with vancomycin
#vancomycin post treatment not good ..

which(cascores[,2]>3.5)
#159 160 165 166 167 168 which is post treatment metronidazole
#metronidazole post treatment good

#PCoA principal coordinates analysis ####

pcoascores = cmdscale(vegdist(ASV),k=10)
plot(pcoascores)
#again showing similar from coa scores above, use which again to confirm
which(pcoascores[,1]>.6)
#180-191 
which(pcoascores[,1]>.25 & pcoascores[,1]<.6)
#159 160 165 166 167 168

#same with cascores, metronidazole and vancomycin

cmdscale(vegdist(ASV),eig=TRUE) #not sure what this is
#look at eig values 
#see what groups are, reduce info to few variables that we can use as y variables inregression

#check metadata, choose treatment, timepoint
#regression ####

summary(lm(pcoascores[,1] ~ data$Treatment))
#this takes first in alphabetical order and makes that as the baseline, in this case butyrate, and checks if treatment is different from baseline(butyrate) 
# p value very strong effect in antibiotics which refelcts what we expect to see from data 

summary(lm(pcoascores[,1] ~ data$Timepoint))
#default is post -treatment; -.18 pre treatemnt less bacteria? but is there any interaction?

#pretreatment less bacteria

#interaction in regression
summary(lm(pcoascores[,1] ~ data$Timepoint*data$Treatment))
#lower abundance after treatment
#most not significant except for 3 treatments which are the antibiotics 

summary(lm(pcoascores[,2] ~ data$Timepoint*data$Treatment))
#metro has high abuncnce in axis 2 and vanco has low abundance in axis 2 

#hard to analyse? 


#what predicts treatment effect?
summary(lm (as.integer(data$Timepoint)~pcoascores[,1:4])) #not working
#diff group of bacteria that can predict whether they have been treated or not

#bubbleplot 

#timepoint

plot(pcoascores, cex = as.integer(data$Timepoint)/1.5, col="gray80",bg = hsv(h=as.integer(data$Treatment)/8), pch = 21, 
     xlab = "PCoA axis 1", ylab = "PCoA axis 2", xaxs="i", yaxs ="i", bty = "l") #not working

plot(cascores, cex = as.integer(data$Timepoint)/1.5, col="gray80",bg = hsv(h=as.integer(data$Treatment)/9), pch = 21, 
     xlab = "PCoA axis 1", ylab = "PCoA axis 2", xaxs="i", yaxs ="i", bty = "l") #not working
#small = post


#violin plot ####

library(vioplot)

#treatment for pre and treatment for post


vioplot(pcoascores[,1]~data$Treatment, col = hsv(1:8/9))

#STROONG NEGATIVE EFFECT on top of the plot and right
t= which(data$Timepoint=="Post-treatment")
vioplot(pcoascores[t,1]~data$Treatment[t], col = hsv(1:8/9))
#all ttreated vancom crowded in corner 
#iilustrates the effect



t1= which(data$Timepoint=="Pre-treatment")
vioplot(pcoascores[t1,1]~data$Treatment[t1], col = hsv(1:8/9))
#not much variation, as expected this is only pre treatment 




table(data$Treatment)

# Alpha diversity ####
microeco = `Faith microeco data`

microeco$cal_alphadiv()

#principal component

princomp((family_data[,3:28]))$loadings

#comp.1 = lachano - one value thats wacky (rlly high value)
#make sure each family gets equal weight in the analysis by scaling 
princomp(scale(family_data[,3:28]))$loadings
#comp1 diff numbers bc all responses 
#tells us that familes do diffrent things 


# but we want to know if there is common response
factanal(scale(family_data[,3:28]),factor=4)$loadings

factanal(scale(family_data[,3:28]),factor=1)$loadings
#factor =1 mix of + and - they dont respond the same, they dont agree with each other

factanal(scale(family_data[,3:28]),factor=2)$loadings

#variance explained = proportion var 
plot(factanal(scale(family_data[,3:28]),factor=2)$loadings)


#see if there is commonality in response; is there a drug killing a set of family? 
#look at indiv, which mice respond to treatment 
#which mice are responding to similar pattern
factanal_scores = factanal(scale(family_data[,3:28]),factor=2,scores="regression")$scores
plot(factanal_scores, cex=.5,pch=19, col =hsv(h=untreated/4))
#Some mice respond differenty, check 
#text(factanal_scores,labels=Metadata$Treatment)
#vancomycin and metro have huge effects; they are hitting something hard, some dont have much effects
#from here we strong effects on diff bacteria

#make untreated and control group
untreated = rep(0,191) 
control = rep(0,191)
untreated[Metadata$Treatment=="Untreated"]=1
control[Metadata$Timepoint=="Pre-treatment"]=1

factanal_scores = factanal(scale(family_data[,3:28]),factor=2,scores="regression")$scores
plot(factanal_scores[,c(1,2)], cex=0,pch=19, col =hsv(h=untreated/4))
text(factanal_scores[,c(1,2)],labels=Metadata$Treatment)



#factor 4 = neo; 1 and 3 = metro ; 2 = vanco

factanal(scale(family_data[,3:28]),factor=10,scores="regression")
#proportion var = amouint of variance = big number = lot (1) 0 = doesnt tell you anything

#factor 1
#factor 2 = we see 0.9 very high vancomycon strong for some families; this cant be seen in t test bc it onlt says no or yes effect
#this allows you to summarise quickly 
#first factor strongest, 2nd about the same, 3rd. in between, 4th really weak 
# we have reduced the matrix to 4 plots 

#mixed effects regression
abundance=rowSums(family_data[,3:28])
hist(abundance)
model_output=lmer(factanal_scores[,1]~abundance+(1|untreated)+(1|control)+(1|Metadata$Individual))
summary(model_output)
#very significant effect
rand(model_output)
#untreated and control not as important as the abundance in determining
#Factor 1 conterolled by a lot or little, doesnt matter how ytou got there, just how much

ranef(model_output)
r.squaredGLMM(model_output)
#marginal from fixed effect

#FACTOR 2
model_output=lmer(factanal_scores[,2]~abundance+(1|untreated)+(1|control))
summary(model_output)
rand(model_output)

ranef(model_output)
r.squaredGLMM(model_output)

#subtle difference in response pattern; co variance in which drug works in which families 

#abundance 
model_output=lmer(abundance~factanal_scores[,1]+(1|untreated)+(1|control))
summary(model_output)
rand(model_output)

ranef(model_output)
r.squaredGLMM(model_output)
# control not imp to determine variance explained in the baundance we got 

model_output=lmer(abundance~factanal_scores[,2]+(1|untreated)+(1|control))
summary(model_output)
rand(model_output)

ranef(model_output)
r.squaredGLMM(model_output)
#control now significant  
#vancomycin =affected by presence of control 
#control is high in axis 2