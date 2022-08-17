# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Implementing population models
# First Version 2022-07-21
# --------------------------------------------------#

# Loading needed packages
library(deSolve) #because we will solve ODE
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

#Logistic growth model
source("./logGrowth.R")
#Creating a function for logistic growth
#logGrowth<-function(t,y,p) {
#  N<-y[1]
#  with(as.list(p),{#as.list() convert objects into lists
#    dN.dt<-r*N*(1-a*N)
#    return(list(dN.dt))
#  })
#}
#We will specify the parameters and then solve the ODE
p <- c(r = 1, a = 0.001) #Named vector with parameters
y0 <- c(N = 10)  #Initial condition
t <- 1:20 #Time steps
#We can use ode() to solve the equation
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)
#Plotting
df_log <- as.data.frame(out_log)
ggplot(df_log) +
  geom_line(aes(x = time, y = N))+
  theme_classic()

#LOKTA-VOLTERRA COMPETITION MODEL

LVComp <- function(t,y,p){
  N <- y
  with(as.list(p), {
    dN1.dt <- r[1] * N[1] * (1 - a[1, 1] * N[1] - a[1, 2] * N[2])
    dN2.dt <- r[2] * N[2] * (1 - a[2, 1] * N[1] - a[2, 2] * N[2])
    return(list(c(dN1.dt, dN2.dt)))
  })
}
#Solving the system. For this particular case, we need to define
#the α matrix with the competition coefficients

a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3) #from tidyr to organize the information as we want
                                                    #cols: columns that I want to collapse
ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = 'Time', y = "N", color = "Species")+
  theme_classic()
