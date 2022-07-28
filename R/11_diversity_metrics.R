library(tidyverse)
library(reshape2)

comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])

abund <- mapply(sum,comm[,-1])
most_abund <- sort(abund,decreasing=T)

#richness=species/site
#site=columnas

#Turning into binary to calculate richness
comm2<-comm
comm2[comm2>0] <- 1

#rich <- mapply(sum,comm2[-1,])
#rich
rich <- apply(comm2[,-1],1,sum)
most_rich <-sort(rich,decreasing=T)

#Calculating most abundant species per site

most_abundant_site <- c()

for (i in 1:nrow(comm)){
  index=which.max(comm[i,-1])
  most_abundant_site <- c(most_abundant_site,index)
}




