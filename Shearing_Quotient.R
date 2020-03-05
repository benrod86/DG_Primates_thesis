setwd("D:/My_stuff/UT_Austin/Masters_thesis/R_Code/DG_Primates_thesis/SQ")
library(caper)
library(ape)
library(geiger)
library(nlme)
library(tidyverse)

## add data and remove fossil taxa
upperM1 <- read.csv("Upper_M1_shear.csv", header = T, row.names = 1)
upperM1_extant <- upperM1[-14,]
lowerm1 <- read.csv("Lower_m1_shear.csv", header = T, row.names = 1)
lowerm1_extant <- lowerm1[-c(19,20,21,22),]
lowerm2 <- read.csv("Lower_m2_shear.csv", header = T, row.names = 1)
lowerm2_extant <- lowerm2[-c(30,31),]