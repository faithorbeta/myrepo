install.packages("usethis")
library(usethis)
use_git_config(user.name = 'faithorbeta', user.email = 'faithmarian.orbeta@students.mq.edu.au')


data = Dataset_for_Faith
head = data

data = as.data.frame(data)
colnames(data)

ASV = ASV_dataset
head(ASV)
class = Class_dataset
family = Family_dataset
genus = Genus_dataset
order = Order_dataset
phylum = Phylum_dataset

treatment = data$Treatment

sum(is.na(data))           # Total NAs
colSums(is.na(data))       # NAs per column

#is there a difference in pre-treatment and post-treatment in ASV1


#corresponse analysis

library(vegan)

cascores = cca(ASV)$CA$u

plot(cascores)

which(cascores[,1]>4)

#vancomycin post treatment not good

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
