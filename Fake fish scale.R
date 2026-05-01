library(geomorph) #Key package for GMM
library(readr) #Read fcsv files exported from 3D Slicer.
library(tidyverse) #A basic package includes basic functions for data cleaning.
library(tools)
library(Rvcg)
library(Morpho)

# Scale modelling -----------------------------------------------------------------------
#After using other codes to get 1) average shape of overall ganoid scale across all specimens, and 2) average AP, DP of each size class, and 3) average SB of size classes that share similar SB geometry, we are going to assemble them together into the scale model for 3D printing.

#####Highlight: Before running the code below, make sure to organize the AP, DP and SB files (both landmark csv file and model ply file) that you want to assemble together into same folder, along with the average intact scale as your template in that folder as well, and set it as your working directory for this R script #####

#The code below help us reorient the position of the scale parts, align them together at right angle and distance for assembly.

ap_idx   <- c(1, 2, 9, 10, 11) #In the total 20 landmarks, these landmarks are used to describe AP.
body_idx <- c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) #In the total 20 landmarks, these landmarks are used to describe SB.
dp_idx <- c(2, 3, 4, 12, 13) #In the total 20 landmarks, these landmarks are used to describe DP.

#First, create a function to read fcsv files.
read_fcsv_xyz <- function(path) {
  df <- read_csv(
    file = path,
    comment = "#", #Tell R you start reading the fcsv file from the line starts with "#"
    col_names = c("id","x","y","z","ow","ox","oy","oz", "vis","sel","lock","label","desc","associatedNodeID"), #Tell R you want it to read these columns of data.
    show_col_types = FALSE #I don't need R to tell me the type of data for each column.
  )
  
  as.matrix(df[, c("x","y","z")]) #After reading all columns in each fcsv file, the function will extract the columns including the xyz coordinates of each landmark and remove other unnecessary information.
}

#Read the 20 landmarks describing the full fish scale - AdultLoc1, which will be the reference template for scale parts alignment.
full_scale <- read_fcsv_xyz("AdultLoc1.fcsv") 

ap_target   <- full_scale[ap_idx,   ] #Pick the landmarks in the full scale that describe AP. 
body_target <- full_scale[body_idx, ] #Pick the landmarks in the full scale that describe SB. 
dp_target <- full_scale[dp_idx, ] #Pick the landmarks in the full scale that describe DP. 

ap_fake <- read_fcsv_xyz("StripelingLoc1_AP.fcsv"
)   #change the text inside the quotation marks to the csv file for landmarks for fake scale AP.

body_fake <- read_fcsv_xyz("AdultLoc1_SB.fcsv"
)   #change the text inside the quotation marks to the csv file for landmarks for fake scale SB.

dp_fake <- read_fcsv_xyz("StripelingLoc1_DP.fcsv"
) #change the text inside the quotation marks to the csv file for landmarks for fake scale DP.

rot_AP <- rotonto(
  x       = ap_target,  # target = where AP should end up (on full mean)
  y       = ap_fake,    # source = current fake AP
  scale   = TRUE,       # match size to target
  reflection = FALSE
)

ap_fake_on_full <- rot_AP$yrot #ap_mean landmarks are now placed into the coordinate system of the full-scale mean.

rot_body <- rotonto(
  x       = body_target,  # target = body subset on full-scale mean
  y       = body_fake,    # source = current body mean
  scale   = TRUE,
  reflection = FALSE
)

body_fake_on_full <- rot_body$yrot

rot_DP <- rotonto(
  x = dp_target,
  y = dp_fake,
  scale = TRUE,
  reflection = FALse
)

dp_fake_on_full <- rot_DP$yrot

#Convert these Procrusted transofrms into 4x4 matrices.
trafo_AP   <- getTrafo4x4(rot_AP)
trafo_body <- getTrafo4x4(rot_body)
trafo_DP <- getTrafo4x4(rot_DP)

#Read the separate average scale part meshes
AP_mesh   <- vcgImport("StripelingLoc1_AP.ply",  updateNormals = TRUE, clean = TRUE) #change the text inside the quotation marks to the ply file for fake AP.
Body_mesh <- vcgImport("AdultLoc1_SB.ply",  updateNormals = TRUE, clean = TRUE) #change the text inside the quotation marks to the ply file for fake SB.
DP_mesh   <- vcgImport("StripelingLoc1_DP.ply",  updateNormals = TRUE, clean = TRUE) #change the text inside the quotation marks to the ply file for fake DP.

#Apply the transforms to move each part mesh into full-scale mean space
AP_mesh_on_full   <- applyTransform(AP_mesh,   trafo_AP)
Body_mesh_on_full <- applyTransform(Body_mesh, trafo_body)
DP_mesh_on_full   <- applyTransform(DP_mesh,   trafo_DP)

#Save the reorientated and rescaled scale parts, import them to Blender to final assembly.
dir.create("outputs_fake_scale", showWarnings = FALSE)

vcgPlyWrite(AP_mesh_on_full,
            file = "outputs_fake_scale/AP_on_full_fake.ply",
            writeCol = FALSE)

vcgPlyWrite(Body_mesh_on_full,
            file = "outputs_fake_scale/Body_on_full_fake.ply",
            writeCol = FALSE)

vcgPlyWrite(DP_mesh_on_full,
            file = "outputs_fake_scale/DP_on_full_fake.ply",
            writeCol = FALSE)