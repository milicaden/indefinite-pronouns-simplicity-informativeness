library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(minpack.lm)

source("./Indefinites_functions.R")

####################################
# PARETO FRONT
####################################
Folder = "../data/"

# Import the estimated dominant languages file
dominant = read.csv(paste0(Folder, "RSApareto_dominant.csv"), header = TRUE)
dominant$type ="dominant"
#dominant = dominant %>% distinct(new_col, .keep_all = TRUE)#keep only unique dominant points
names(dominant)[names(dominant)=="RSAcostoflanguages"] <- "costoflanguages"

# Obtain the Pareto front by fitting a curve through dominant languages
fit<-nlsLM(complexityoflanguages~a/(b+costoflanguages), data = dominant, start = list(a=1,b=0))
summary(fit)

# Make a data frame with cost (x) and complexity (y) of a 10000 points on the frontier
pareto <- data.frame(costoflanguages = seq(from = 0.89, to = 14.5, length.out = 10000))
pareto_comp <- predict(fit, newdata = pareto, se.fit = TRUE)
pareto$complexityoflanguages <- pareto_comp
pareto$type = "Pareto"

####################################
# EXPERIMENT 1 PLOT
####################################
# Experiment 1 languages (matched for synonymy and coverage)
allfinal_exp1 = read.csv(paste0(Folder, "all_complexity_cost_exp1.csv"))
allfinal_exp1$costoflanguages = NULL
names(allfinal_exp1)[names(allfinal_exp1)=="RSAcostoflanguages"] <- "costoflanguages"

#Plot
p1 <- ggplot(allfinal_exp1, aes(x=costoflanguages, y=complexityoflanguages)) +
  scale_color_manual(name="Language", values=c('#E69F00', '#293352'))+
  scale_shape_manual(name="Language", values=c(16, 17))+
  geom_point(aes(shape=type, color=type), position = "jitter", alpha = 0.8, size = 1) + xlab("Communicative cost of languages") + ylab("Complexity of languages") + scale_x_continuous(limits = c(0, 15))
print(p1)

# Add Pareto front to the plot
# warning due to not ploting irrelevant pareto points
png("Experiment1_pragmatic_basiclot.png", width = 140, height = 90, units='mm', res = 300)
p2 <- p1 + 
  geom_line(data = pareto, size = 1.5) + ylim(0, 18)
print(p2)
dev.off()

####################################
# EXPERIMENT 2 PLOT
####################################
# Experiment 2 languages (matched for synonymy and coverage)
allfinal_exp2  = read.csv(paste0(Folder, "all_complexity_cost_exp2.csv"))
allfinal_exp2$costoflanguages = NULL
names(allfinal_exp2)[names(allfinal_exp2)=="RSAcostoflanguages"] <- "costoflanguages"


# Plot
p3 <- ggplot(allfinal_exp2, aes(x=costoflanguages, y=complexityoflanguages)) +
  scale_color_manual(name="Language", values=c('#293352', '#E69F00'))+
  scale_shape_manual(name="Language", values=c(17, 16))+
  geom_point(aes(shape=type, color=type), position = "jitter", alpha = 0.8, size = 1) + scale_x_continuous(limits = c(0, 15)) +  xlab("Communicative cost of languages") + ylab("Complexity of languages")
print(p3)

# Add Pareto front to the plot
png("Experiment2_pragmatic_basiclot.png", width = 140, height = 90, units='mm', res = 300)
p4 <- p3 + 
  geom_line(data = pareto, size = 1.5) + ylim(0, 35)
print(p4)
dev.off()


####################################
# EXPERIMENT 1 STATISTICAL ANALYSES
####################################
natural_distances = paste0(Folder, "natural_distances_pareto_RSA.csv")
artificial_distances = paste0(Folder, "artificial_distances_pareto_RSA.csv")

generate = TRUE

if(generate) {
  # Natural languages distances to Pareto
  natural = subset(allfinal_exp1, type == "natural")
  natural = min.euc.dist(natural, pareto)
  write.csv(natural, natural_distances, row.names=FALSE)

  # Artificial languages distances to Pareto
  artificial = subset(allfinal_exp1, type == "artificial")
  artificial = min.euc.dist(artificial, pareto)
  write.csv(artificial, artificial_distances, row.names=FALSE)
} else {
  natural <- read.csv(natural_distances)
  artificial = read.csv(artificial_distances)
}

t.test(natural$minimal, artificial$minimal)

####################################
# EXPERIMENT 2 STATISTICAL ANALYSES
####################################
Haspok_distances = paste0(Folder, "Haspok_distances_pareto_RSA.csv")
Haspnotok_distances = paste0(Folder, "Haspnotok_distances_pareto_RSA.csv")

if(generate) {
  # Haspel ok languages distances to Pareto
  Haspok <- subset(allfinal_exp2, type == "Haspel-ok")
  Haspok = min.euc.dist(Haspok, pareto)
  write.csv(Haspok, Haspok_distances, row.names=FALSE)
  
  # Not Haspel ok languages distances to Pareto
  Haspnotok <- subset(allfinal_exp2, type == "Not Haspel-ok")
  Haspnotok = min.euc.dist(Haspnotok, pareto)
  write.csv(Haspnotok, Haspnotok_distances, row.names=FALSE)
} else {
  Haspok <- read.csv(Haspok_distances)
  Haspnotok = read.csv(Haspnotok_distances)
}

t.test(Haspok$minimal, Haspnotok$minimal)

