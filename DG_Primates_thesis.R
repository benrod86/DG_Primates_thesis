
### SHEARING QUOTIENT ###

setwd("D:/My_stuff/UT_Austin/Masters_thesis/R_Code/DG_Primates_thesis/SQ")
library(caper)
library(ape)
library(geiger)
library(nlme)
library(tidyverse)

# add data and remove fossil taxa
upperM1 <- read.csv("upperM1means.csv", header = T, row.names = 1)
upperM1_extant <- upperM1[-c(14,15,16),]
lowerm1 <- read.csv("lowerm1means.csv", header = T, row.names = 1)
lowerm1_extant <- lowerm1[-c(19,20,21,22,23),]
lowerm2 <- read.csv("lowerm2means.csv", header = T, row.names = 1)
lowerm2_extant <- lowerm2[-c(30,31),]


# Data with ONLY Frugivores
upperM1_frugivores <- read.csv("upperM1means_frugivores.csv", header = T, row.names = 1)
lowerm1_frugivores <- read.csv("lowerm1means_frugivores.csv", header = T, row.names = 1)
lowerm2_frugivores <- read.csv("lowerm2means_frugivores.csv", header = T, row.names = 1)



# Vectors for the Mesial distal length and crest length of extant taxa for each tooth
M1_length <- upperM1_extant$Mean.M.D.Length[upperM1_extant$extant_fossil == "extant"]
m1_length <- lowerm1_extant$Mean.M.D.Length[lowerm1_extant$extant_fossil == "extant"]
m2_length <- lowerm2_extant$Mean.M.D.Length[lowerm2_extant$extant_fossil == "extant"]
M1_crest <- upperM1_extant$Mean.Crest.Length[upperM1_extant$extant_fossil == "extant"]
m1_crest <- lowerm1_extant$Mean.Crest.Length[lowerm1_extant$extant_fossil == "extant"]
m2_crest <- lowerm2_extant$Mean.Crest.Length[lowerm2_extant$extant_fossil == "extant"]


# Vectors for the Mesial distal length and crest length of extant taxa for each tooth
M1_length_frugivores <- upperM1_frugivores$Mean.M.D.Length
m1_length_frugivores <- lowerm1_frugivores$Mean.M.D.Length
m2_length_frugivores <- lowerm2_frugivores$Mean.M.D.Length
M1_crest_frugivores <- upperM1_frugivores$Mean.Crest.Length
m1_crest_frugivores <- lowerm1_frugivores$Mean.Crest.Length
m2_crest_frugivores <- lowerm2_frugivores$Mean.Crest.Length



# Read in the tree for each model
Upper_M1_tree <- read.nexus("Upper_M1.nex")
Lower_m1_tree <- read.nexus("Lower_m1.nex")
Lower_m2_tree <- read.nexus("Lower_m2.nex")

# Frugivore ONLY trees
Upper_M1_tree_frugivores <- read.nexus("Upper_M1_frugivores.nex")
Lower_m1_tree_frugivores <- read.nexus("Lower_m1_frugivores.nex")
Lower_m2_tree_frugivores <- read.nexus("Lower_m2_frugivores.nex")


# Build PGLS REGRESSIONS
## ALL extant species

### Model for UPPER MOLAR 1
bmupperm1<-corBrownian(1,Upper_M1_tree)
bmupperm1

model_upperM1_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length), data=upperM1_extant, correlation=bmupperm1)
summary(model_upperM1_log)

### Model for LOWER MOLAR 1
bmlowerm1<-corBrownian(1,Lower_m1_tree)
bmlowerm1

model_lowerm1_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length), data=lowerm1_extant, correlation=bmlowerm1)
summary(model_lowerm1_log)

### Model for LOWER MOLAR 2
bmlowerm2<-corBrownian(1,Lower_m2_tree)
bmlowerm2

model_lowerm2_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length), data=lowerm2_extant, correlation=bmlowerm2)
summary(model_lowerm2_log)


## ONLY Frugivores models 

### Model for UPPER MOLAR 
bmupperm1_frugivores<-corBrownian(1,Upper_M1_tree_frugivores)
bmupperm1_frugivores

model_upperM1_frugivores_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length),
                                  data=upperM1_frugivores, correlation=bmupperm1_frugivores)
summary(model_upperM1_frugivores_log)

### Model for LOWER MOLAR 1
bmlowerm1_frugivores<-corBrownian(1,Lower_m1_tree_frugivores)
bmlowerm1_frugivores

model_lowerm1_frugivores_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length),
                                  data=lowerm1_frugivores, correlation=bmlowerm1_frugivores)
summary(model_lowerm1_frugivores_log)

### Model for LOWER MOLAR 2
bmlowerm2_frugivores<-corBrownian(1,Lower_m2_tree_frugivores)
bmlowerm2_frugivores

model_lowerm2_frugivores_log<-gls(log10(Mean.Crest.Length)~log10(Mean.M.D.Length), data=lowerm2_frugivores, correlation=bmlowerm2_frugivores)
summary(model_lowerm2_frugivores_log)


# Plot the crest length and molar length with both PGLS and regular old linear regressions to examine for best fit

### Upper M1 data with all log transformed regressions
upM1_frug <- lm(data = upperM1_frugivores, log10(upperM1_frugivores$Mean.Crest.Length) ~ log10(upperM1_frugivores$Mean.M.D.Length))
upM1_frug
upM1_extant <- lm(data = upperM1_extant, log10(upperM1_extant$Mean.Crest.Length) ~ log10(upperM1_extant$Mean.M.D.Length))
upM1_extant

plot(x = log10(upperM1$Mean.M.D.Length), y = log10(upperM1$Mean.Crest.Length), col = upperM1$Diet)
abline(b = .7781908, a = .4275948, col = "blue") # pgls frugivores - This seems to best fit the data
abline(b = 1.0656883, a = .2497916, col = "red") # pgls all extant
abline(b = .9051, a = .3518, col = "green") # log10 frugivores
abline(b = 1.1662, a = 0.1865, col = "purple") # log10 all extant




### Lower m1 data with all log transformed regressions
lowm1_frug <- lm(data = lowerm1_frugivores, log10(lowerm1_frugivores$Mean.Crest.Length) ~ log10(lowerm1_frugivores$Mean.M.D.Length))
lowm1_frug
lowm1_extant <- lm(data = lowerm1_extant, log10(lowerm1_extant$Mean.Crest.Length) ~ log10(lowerm1_extant$Mean.M.D.Length))
lowm1_extant

plot(x = log10(lowerm1$Mean.M.D.Length), y = log10(lowerm1$Mean.Crest.Length), col = lowerm1$Diet)
abline(b = .9427692, a = .2888592, col = "blue") # pgls frugivores 
abline(b = 1.0387669, a = .2305891, col = "red") # pgls all extant
abline(b = .9442, a = .2885, col = "green") # log10 frugivores
abline(b = 1.0474, a = .2308, col = "purple") # log10 all extant


### Lower m2 data with all log transformed regressions
lowm2_frug <- lm(data = lowerm2_frugivores, log10(lowerm2_frugivores$Mean.Crest.Length) ~
                   log10(lowerm2_frugivores$Mean.M.D.Length))
lowm2_frug
lowm2_extant <- lm(data = lowerm2_extant, log10(lowerm2_extant$Mean.Crest.Length) ~ log10(lowerm2_extant$Mean.M.D.Length))
lowm2_extant

plot(x = log10(lowerm2$Mean.M.D.Length),
     y = log10(lowerm2$Mean.Crest.Length),
     col = lowerm2$Diet)
abline(b = .8693324, a = .3156598, col = "blue") # pgls frugivores 
abline(b = .8649776, a = .3707330, col = "red") # pgls all extant
abline(b = 1.0159, a = .2408, col = "green") # log10 frugivores
abline(b = .9901, a = .2968, col = "purple") # log10 all extant





############   CODE FOR PLOTS ##############

## Data for plotting

setwd("D:/My_stuff/UT_Austin/Masters_thesis/R_Code/SQ")
par(mfrow = c(1,1))
plotdata <- read.csv("Alldataforplots.csv", header = T)
str(plotdata)

## Aetup color palette for consistent coloration

MyPalette <- c(Folivore = "#32CD32", Frugivore = "#FF0000",
               Insectivore = "#FFA500", Gummivore =  "#FF69B4",
               Seed_predator = "#8B4513", Diablomomys_dalquesti = "#7FF000",
               Omomys_carteri = "#0000FF", Mescalerolemur_horneri = "#CC0066",
               Taxon_A = "#FF00FF", Taxon_B = "#8470FF")

MySymbols <- c(Folivore = 20, Frugivore = 20, Insectivore = 20,
               Gummivore =  20, Seed_predator = 20, Diablomomys_dalquesti = 17,
               Omomys_carteri = 17, Mescalerolemur_horneri = 17,
               Taxon_A = 17, Taxon_B = 17)


plotdata$Diet <- factor(plotdata$Diet, levels = c("Insectivore",
                                                  "Folivore", "Frugivore", "Gummivore", "Seed_predator",
                                                  "Omomys_carteri", "Diablomomys_dalquesti", "Mescalerolemur_horneri",
                                                  "Taxon_A", "Taxon_B"))

str(plotdata)


## SHEARING QOTIENT REGRESSION PLOTS

### UPPER M1 ###

axistitle <- element_text(face = c("bold"), color = "black", size = 18)
axistext <- element_text(color = "black", size = 12)
plottitle <- element_text(face = "bold", color = "black", size = 20)
UM1plot <- ggplot(plotdata[1:16, ], aes(x  = log10(Mean.M.D.Length),
                                         y = log10(Mean.Crest.Length), shape = Diet, col = Diet)) +
  geom_abline(intercept = .4275948,  slope = .7781908, size = 2,
              color = "black") + geom_point(size =10) +
  scale_colour_manual(values = MyPalette) +
  scale_shape_manual(values = MySymbols) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = axistext, axis.title = axistitle,
        axis.text.y = axistext, title = plottitle, legend.title=element_blank()) + 
  xlab("log10 M1 length") +
  ylab("log10(Upper M1 shearing crest length)") + 
  labs(title = "Upper M1 length and shearing crest length")
UM1plot

### Lower m1 ###

axistitle <- element_text(face = c("bold"), color = "black", size = 18)
axistext <- element_text(color = "black", size = 12)
plottitle <- element_text(face = "bold", color = "black", size = 20)

lm1plot <- ggplot(plotdata[17:39, ], aes(x  = log10(Mean.M.D.Length),
                                         y = log10(Mean.Crest.Length), shape = Diet, col = Diet)) +
  geom_abline(intercept = .2888592, slope = .9427692,
              size = 2, color = "black") +
  geom_point(size = 10) +
  scale_colour_manual(values = MyPalette) +
  scale_shape_manual(values = MySymbols) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = axistext, axis.title = axistitle,
        axis.text.y = axistext, title = plottitle, legend.title=element_blank()) +
  xlab("log10 m1 length") +
  ylab("log10(Lower m1 shearing crest length)") +
  labs(title = "Lower m1 length and shearing crest length")
lm1plot

### LOWER m2 ###

axistitle <- element_text(face = c("bold"), color = "black", size = 18)
axistext <- element_text(color = "black", size = 12)
plottitle <- element_text(face = "bold", color = "black", size = 20)

lm2plot <- ggplot(plotdata[41:69, ], aes(x  = log10(Mean.M.D.Length),
                                        y = log10(Mean.Crest.Length), shape = Diet, col = Diet)) +
  geom_abline(intercept = .3156598 , slope = .8693324,
              size = 2, color = "black") +
  geom_point(size = 10) + scale_colour_manual(values = MyPalette) +
  scale_shape_manual(values = MySymbols) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = axistext, axis.title = axistitle,
        axis.text.y = axistext, title = plottitle, legend.title=element_blank()) +
  xlab("log10 m2 length") +
  ylab("log10(Lower m2 shearing crest length)") +
  labs(title = "Lower m2 length and shearing crest length")
lm2plot



## SHEARING QUOTIENT BOXPLOTS



### UPPER M1 ###

## Data for plotting
par(mfrow = c(1,1))
plotdata_M1 <- read.csv("AllDataforplots_UpperM1.csv", header = T)
str(plotdata_M1)

## Setup color palette for consistent coloration

MyPalette <- c(Folivore = "#32CD32", Frugivore = "#FF0000",
               Seed_predator = "#8B4513", Diablomomys_dalquesti = "#7FF000",
               Omomys_carteri = "#0000FF", Mescalerolemur_horneri = "#CC0066")

MySymbols <- c(Folivore = 20, Frugivore = 20,
               Seed_predator = 20, Diablomomys_dalquesti = 17,
               Omomys_carteri = 17, Mescalerolemur_horneri = 17)

levels(plotdata_M1$Diet) <-c("Folivore", "Frugivore", 
                          "Seed_Predator", "Omomys_carteri", "Diablomomys_dalquesti", 
                          "Mescalerolemur_horneri")


UM1Xtext <- element_text(face = c("bold", "bold", "bold", "bold.italic", "bold.italic", "bold.italic"), color = "black", size = 16)
UM1Ytext <- element_text(face = "bold", color = "black", size = 16) 
UM1Ytext2 <- element_text(face = "bold", color = "black", size = 12)
UM1title <- element_text(face = "bold", color = "black", size = 18)
UM1box <- ggplot(plotdata_M1[1:16,], aes(x= Diet_factor,
                                      y= Log10_PGLS_sq_frugs, fill = Diet_factor, name = Diet)) + 
  geom_boxplot() + scale_fill_manual(values = c("#32CD32", "#FF0000", "#8B4513",
                                                "#7FF000", "#0000FF", "#CC0066")) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = UM1Xtext, axis.title = UM1Ytext,
        axis.text.y = UM1Ytext2, title = UM1title) +
  xlab("") +
  ylab("Shearing Quotient") +
  labs(title = "Upper M1 Shearing Quotient") +
  scale_x_discrete(labels = c("Folivores", "Frugivores", "Seed Predators",
                              "D. dalquesti", "M. horneri", "O. carteri"))
UM1box



####### LOWER m1 attempt 2

plotdata_m1 <- read.csv("AllDataforplots_Lowerm1.csv", header = T)


MyPalette <- c(Folivore = "#32CD32", Frugivore = "#FF0000",
               Seed_predator = "#8B4513", Omomys_carteri = "#0000FF", 
               Diablomomys_dalquesti = "#7FF000", Mescalerolemur_horneri = "#CC0066",
               Taxon_A = "#FF00FF", Taxon_B = "#8470FF")

lM1Xtext <- element_text(face = c("bold", "bold", "bold",
                                  "bold.italic","bold.italic","bold.italic", "bold.italic", "bold.italic"),
                         color = "black", size = 16)
lM1Ytext <- element_text(face = "bold", color = "black", size = 16) 
lM1Ytext2 <- element_text(face = "bold", color = "black", size = 12)
lM1title <- element_text(face = "bold", color = "black", size = 18)
lm1box <- ggplot(plotdata_m1[1:23,], aes(x= Diet_factor,
                                       y= Log10_PGLS_sq_frugs, fill = Diet_factor, name = Diet_factor)) + 
  geom_boxplot() + scale_fill_manual(values = c("#32CD32", "#FF0000", "#8B4513",
                                                "#0000FF", "#7FF000", "#CC0066","#FF00FF","#8470FF")) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = lM1Xtext, axis.title = lM1Ytext,
        axis.text.y = lM1Ytext2, title = lM1title) +
  xlab("") +
  ylab("Shearing Quotient") +
  labs(title = "Lower m1 Shearing Quotient") +
  scale_x_discrete(labels = c("Folivores", "Frugivores", "Seed Predators",
                              "O. carteri", "D. dalquesti","M. horneri", "Taxon A", "Taxon B"))
lm1box




### LOWER m2

plotdata_m2 <- read.csv("AllDataforplots_Lowerm2.csv", header = T)

MyPalette <- c( Insectivore = "#FFA500", Folivore = "#32CD32", Frugivore = "#FF0000",
               Gummivore =  "#FF69B4", Omomys_carteri = "#0000FF", Taxon_B = "#8470FF")

MySymbols <- c(Insectivore = 20, Folivore = 20, Frugivore = 20,
               Gummivore =  20, Omomys_carteri = 17, Taxon_B = 17)

levels(plotdata_m2$Diet) <-c("Insectivore", "Folivore", "Frugivore", 
                          "Gummivore", "Omomys_carteri", "Taxon_B")


lM2Xtext <- element_text(face = c("bold", "bold", "bold", "bold",
                                  "bold.italic", "bold.italic"), color = "black", size = 16)
lM2Ytext <- element_text(face = "bold", color = "black", size = 16) 
lM2Ytext2 <- element_text(face = "bold", color = "black", size = 12)
lM2title <- element_text(face = "bold", color = "black", size = 18)
lm2box <- ggplot(plotdata_m2[1:30,], aes(x= Diet_factor,
                                       y= Log10_PGLS_sq_frugs, fill = Diet_factor)) + 
  geom_boxplot() + scale_fill_manual(values = c( "#FFA500","#32CD32",
                                                 "#FF0000", "#FF69B4", "#0000FF", "#8470FF")) +
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = lM2Xtext, axis.title = lM2Ytext,
        axis.text.y = lM2Ytext2, title = lM2title) +
  xlab("") +
  ylab("Shearing Quotient") +
  labs(title = "Lower m2 Shearing Quotient") +
  scale_x_discrete(labels = c("Insectivores", "Folivores", "Frugivores",
                              "Gummivores", "O. carteri", "Taxon B"))
lm2box



par(mfrow = c(2,3))
UM1plot
lm1plot
lm2plot
UM1box
lm1box
lm2box










# DENTAL TOPOGRAPHY

library(molaR)
library(tidyverse)



## Run the Winchester dataset without laplacian smoothing ##
setwd("D:/My_stuff/UT_Austin/Masters_thesis/Winchester2014/Reduced_smoothed")
molaR_Batch(pathname = getwd(), filename = "molaR_Batch.csv",
            DNE = TRUE, RFI = TRUE, OPCr = TRUE, OPC = TRUE, Slope = TRUE,
            Details = TRUE, DNE_outliers = 0.1, DNE_BoundaryDiscard = "Leg",
            RFI_alpha = 0.01, OPCr_steps = 8, OPCr_stepSize = 5.625,
            OPCr_minimum_faces = 5, OPCr_minimum_area = 0, OPC_rotation = 0,
            OPC_minimum_faces = 5, OPC_minimum_area = 0, Slope_Guess = FALSE,
            Parameters = FALSE)



## Run the the Devil's Graveyard dataset without laplacian smoothing at default values ##
setwd("D:/My_stuff/UT_Austin/Masters_thesis/3-D_images/STLs/Dental_Topography/Reorient2/molaR")
molaR_Batch(pathname = getwd(), filename = "molaR_Batch.csv",
            DNE = TRUE, RFI = TRUE, OPCr = TRUE, OPC = TRUE, Slope = TRUE,
            Details = TRUE, DNE_outliers = 0.1, DNE_BoundaryDiscard = "Leg",
            RFI_alpha = 0.01, OPCr_steps = 8, OPCr_stepSize = 5.625,
            OPCr_minimum_faces = 5, OPCr_minimum_area = 0, OPC_rotation = 0,
            OPC_minimum_faces = 5, OPC_minimum_area = 0, Slope_Guess = FALSE,
            Parameters = FALSE)



## Run the Winchester dataset WITH laplacian smoothing at default values ##
setwd("D:/My_stuff/UT_Austin/Masters_thesis/Winchester2014/Reduced_smoothed/Laplacian")
molaR_Batch(pathname = getwd(), filename = "molaR_Batch.csv",
            DNE = TRUE, RFI = TRUE, OPCr = TRUE, OPC = TRUE, Slope = TRUE,
            Details = TRUE, DNE_outliers = 0.1, DNE_BoundaryDiscard = "Leg",
            RFI_alpha = 0.08, OPCr_steps = 8, OPCr_stepSize = 5.625,
            OPCr_minimum_faces = 5, OPCr_minimum_area = 0, OPC_rotation = 0,
            OPC_minimum_faces = 5, OPC_minimum_area = 0, Slope_Guess = FALSE,
            Parameters = FALSE)



## Run MY dataset WITH laplacian smoothing at default values ##
setwd("D:/My_stuff/UT_Austin/Masters_thesis/3-D_images/STLs/Dental_Topography/Reorient2/molaR")
molaR_Batch(pathname = getwd(), filename = "molaR_Batch.csv",
            DNE = TRUE, RFI = TRUE, OPCr = TRUE, OPC = TRUE, Slope = TRUE,
            Details = TRUE, DNE_outliers = 0.1, DNE_BoundaryDiscard = "Leg",
            RFI_alpha = 0.08, OPCr_steps = 8, OPCr_stepSize = 5.625,
            OPCr_minimum_faces = 5, OPCr_minimum_area = 0, OPC_rotation = 0,
            OPC_minimum_faces = 5, OPC_minimum_area = 0, Slope_Guess = FALSE,
            Parameters = FALSE)




## Code to make visualizations and run individual statistics on individual teeth from Winchester dataset ##
setwd("D:/My_stuff/UT_Austin/Masters_thesis/Winchester2014/Reduced_smoothed/Laplacian")
molar = vcgPlyRead(file = "amnh_mammals_m-100632_M168-101_Daubentonia_madagascariensis_second_mandibular_molar.smooth.ply")
DNE(molar)
DNEmolar = DNE(molar)
DNE3d(DNEmolar)
rfimolar = RFI(molar)
RFI3d(rfimolar)
opcrmolar = OPCr(molar)
opcmolar = OPC(molar)
OPC3d(opcmolar)


molar1 <- vcgPlyRead(file = "amnh_mammals_m-106650_M414-335_Nycticebus_coucang_coucang_second_mandibular_molar.smooth.ply")
rfimolar1 = RFI(molar1, alpha = .12)
RFI3d(rfimolar1, displacement = 2)
Check2D(rfimolar1, FootColor = "red", TriPointsColor = "black")
DNE(molar1)
DNEmolar = DNE(molar1)
DNE3d(DNEmolar)

