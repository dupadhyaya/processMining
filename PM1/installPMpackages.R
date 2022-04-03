#install packages

packages <- c("ggplot2", "dplyr", "Hmisc", "lme4", "arm", "lattice", "lavaan")

setdiff(packages, rownames(installed.packages()))

install.packages(setdiff(packages, rownames(installed.packages())))  
?setdiff

pmPackages <- c("bupaR", "edeaR", 'eventdataR', 'processmapR', 'processanimateR', 'processmonitR', 'xesreadR', 'petrinetR')
setdiff(pmPackages, rownames(installed.packages()))
install.packages(setdiff(pmPackages, rownames(installed.packages())))  

library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processanimateR)
library(xesreadR)
library(petrinetR)


install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("petrinetR")


