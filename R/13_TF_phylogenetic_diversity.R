#Diversity metrics, back to CESTES data

#Loading necessary libraries
library(tidyverse)
library(reshape2)
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)

files_path <- list.files(path = "data/raw/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

file_names <- gsub(".csv", "", basename(files_path), fixed = TRUE)

for (i in 1:length(files_path)) {
  data <- read.csv(files_path[[i]])
  assign(file_names[i], data)
}


#Transforming column into the rownames of the data frame
head(comm)[,1:6]
rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1]) #Concatenate string
comm <- comm[,-1]
head(comm)[,1:6]

head(traits)[,1:6]
rownames(traits)[1:6]
rownames(traits) <- paste0(traits[,1])
traits <- traits[-1]
head(traits)[,1:6]

#Species richness
richness <- vegan::specnumber(comm)
#Taxonomic diversity
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

#Functional diversity
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
identical(gow,gow2) #Checking equivalence
class(gow)
class(gow2) #They're different because they are from different classes
plot(gow, gow2, asp = 1) #Same values

#Rao's quadratic entropy calculations
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson, fun$FunRao, pch = 19, asp = 1, xlab = "Simpson", ylab = "FunRao")
abline(a = 0, b = 1)

#Diversity indices with package PD
#We can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
names(FuncDiv1)

#Calculation using the matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)
names(FuncDiv)

#---------------
classification_data <- classification(splist$TaxonName, db = "ncbi")
str(classification)
length(classification)

classification_data$'Arisarum vulgare'
classification_data[[1]]
classification_data[[4]]

tible_ex <- classification_data[[1]] %>%
  filter(rank == "family") %>%
  select(name) #return a data.frame

#Function to extract the family
extract_family <- function(x) {
  if(!is.null(dim(x))) {
    y <- x %>%
      filter(rank == "family") %>%
      select(name)
    return(y)
  }
}

#extract_family(classification_data[[1]]) #Checking if the function is working

families <- list()
#list of data frames :'v
for (i in 1:length(classification_data)) {
  families[[i]] <- extract_family(classification_data[[i]])#extracting the family
}

#using vectors

families2 <- vector()
for (j in 1:length(classification_data)) {
  f <- extract_family(classification_data[[j]])
  if (length(f) > 0) families2[j] <- f
}

#families2
