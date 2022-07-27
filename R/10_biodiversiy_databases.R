library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)

species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species,
                   limit = 100000) #Search for GBIF occurrences
names(occs)
myrsine.data <- occs$data
colnames(myrsine.data)
#dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)

sort(unique(myrsine.data$scientificName)) #Sorting by nane
table(myrsine.data$taxonomicStatus) #Checking the currently accepted taxonomy
table(myrsine.data$scientificName, myrsine.data$taxonomicStatus) #Checking the names that are accepted

species.names <- unique(myrsine.data$scientificName) #Creating list with unique species names
dim(species.names)
tax.check <- TPL(species.names)
tax.check

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")

dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)

#Visualizing coordinates of the data
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1) #Check all the parameters
map(, , , add = TRUE)

#Cleaning species records. clean_coordinates() check for common errors in coordinates
#HAVING SPECIFIC ID CODE FOR EACH OBSERVATION IS ESSENTIAL

#This is for not to have NAs in latitude or longitude
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]

# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean") #value=clean returns only the
                                                #potential coordinates
par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
map(, , , add = TRUE)
par(mfrow = c(1, 1))

myrsine.new.geo <- clean_coordinates(x = myrsine.coord,
                                     lon = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     species = "species",
                                     value = "spatialvalid")
# merging with original data
myrsine.new.geo2 <- merge(myrsine.data, myrsine.new.geo,
                          all.x = TRUE,
                          by = "key")
plot(decimalLatitude ~ decimalLongitude, data = myrsine.new.geo, asp = 1)
map(, , , add = TRUE)

#Exporting the data after coordinate check
write.csv(myrsine.new.geo2,
          "data/processed/myrsine_coordinate_check.csv",
          row.names = FALSE)

