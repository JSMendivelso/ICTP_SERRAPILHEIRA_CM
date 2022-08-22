library(tidyverse)
library(reshape2)

comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])

abund <- mapply(sum,comm[,-1])
most_abund <- sort(abund,decreasing=T)

#richness=species/site
#site=columns

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

#---CREATING FUNCTIONS FOR SHANNON AND SIMPSON DIVERSITY IN COMM DATASET
#---------------Shannon


ShannonCalc <- function (x){
  ShannonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    nozero <- fila[fila!=0]
    multi <- (nozero/totalFila)*log(nozero/totalFila)
    sumita <- sum(multi)
    ShannonFila[i] <- -sumita
  }
  return(ShannonFila)
}
commNum <- comm[,2:57]
ShannonCalc (commNum)
comm$H <- ShannonCalc (commNum)
View(comm)
#Testing
diversity (commNum, "shannon")

#---------------------------Simpson
SimpsonCalc <- function (x){
  SimpsonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    prob <- fila/totalFila
    elevado <- prob^2
    sumita <- sum(elevado)
    SimpsonFila[i] <- 1-sumita}
  return(SimpsonFila)
}

comm$Simp <- SimpsonCalc (commNum)
View(comm)
#Testing
diversity (commNum, "simpson")

#-----------------InvSimpson
InvSimpsonCalc <- function (x){
  InvSimpsonFila <- c()
  for (i in 1:nrow(x)){
    fila <- x[i,]
    totalFila <- sum(fila)
    prob <- fila/totalFila
    elevado <- prob^2
    sumita <- sum(elevado)
    InvSimpsonFila[i] <- 1/sumita}
  return(InvSimpsonFila)
}

comm$InvSimp <- InvSimpsonCalc (commNum)
diversity (commNum, "invsimpson")


