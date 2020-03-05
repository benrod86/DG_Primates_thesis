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
molar = vcgPlyRead(file = "AMNH_M_461707_M842-721_Chiropotes_albinasus_second_mandibular_molar.ply")
DNE(molar)
DNEmolar = DNE(molar)
DNE3d(DNEmolar)
rfimolar = RFI(molar)
RFI3d(rfimolar)
opcrmolar = OPCr(molar)
opcmolar = OPC(molar)
OPC3d(opcmolar)
