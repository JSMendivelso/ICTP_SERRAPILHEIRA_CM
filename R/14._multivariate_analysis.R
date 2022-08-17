#Introduction to multivariate analysis in ecology

library(vegan)
library(palmerpenguins)
library(cluster)
library(dplyr)
#data(penguins)
#dim(penguins)
#hclust(d = penguins[,-1])

#Loading the dataset
data(dune)
data(dune.env)
table(dune.env$Management)

#Cluster analysis of the dune vegetation
#We calculate 2 dissimilarities indices between sites (Bray-Curtis and Chord distances)
bray_distance <- vegdist(dune)
#decostand provides standardization methods for community ecologists
chord_distance <- dist(decostand(dune, "norm")) #chord: euclidean normalized to 1

#using cluster, default method is complete, we want to use average

b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

#Plotting side to side
par(mfrow = c(1,2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1,1))

#Ordination methods

is(chord_distance)
norm <- decostand(dune, "norm")
pca <- rda(norm) #redundancy analysis

plot(pca)
summary(pca)
#plot(pca, choices = c(2,3))

#PCA for an enviromental matrix
names(dune.env)
apply(dune.env, 2, class)
#Because all the variables are character
dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)
pca_env <- rda(dune.env[, c('A1', 'Moisture', 'Manure')])
plot(pca_env)
cor(dune.env)

