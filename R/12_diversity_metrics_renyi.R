#August 3/2022
#Understanding which community is more diverse is not always straightforward

Community.A <- c(10,6,4,1)
Community.B <- c(17, rep(1,7)) #rep replicates elements of vectors and lists.

library(vegan)

diversity(Community.A, "shannon")
diversity(Community.B, "shannon")
diversity(Community.A, "invsimpson")
diversity(Community.B, "invsimpson")

ren_comA <- renyi(Community.A)
ren_comB <- renyi(Community.B)

ren_AB <- rbind(ren_comA,ren_comB)
matplot(t(ren_AB)) #Matoplot is made for matrices. Plot the columns of one matrix against the columns of another. t command is the transpose
matplot(t(ren_AB), ylab = "Rényi diversity", type = 'l', axes = F) #Using lines in the plot and eliminating axes
box() #Adding box to the plot
axis(side=2)
axis(side=1, labels = c(0,0.25,0.5,1,2,4,8,16,32,64,"Inf"), at=1:11)

legend("topright",
       legend = c("Community A", "Community B"), lty=c(1,2),col=c(1,2))

ren_comB_Hill <- renyi(Community.B, hill = T)
