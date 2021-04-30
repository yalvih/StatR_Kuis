#Nama : Yalvi Hidayat
#NPM  : 6181801044

#Nama : Michael Alvino W. D.
#NPM  : 6181801041

##############################

#install library
library(ggplot2)
library(datasets)
library(dplyr)
library(TeachingDemos)
library(mlbench)

#dbInit
data("BostonHousing")
View(BostonHousing)
BostonHousing$rad <- as.factor(BostonHousing$rad)
factor(BostonHousing$rad, levels=c("1","2","3","4","5","6","7","8","24"))

