#part of the Shiny app
#for global variables
#and initial set up

#clear all existing objects
rm(list=ls())

#Need truncated distributions function from the 
#TruncatedDistributions package developed by 
#Rob Carnell <bertcarnell@gmail.com>
#found on R-Forge and not CRAN
#options(repos = c("R-Forge"="http://R-Forge.R-project.org", "CRAN"="https://cran.rstudio.org"))
#install.packages("TruncatedDistributions")

#load libraries
library(rgdal); library(plyr)
library(shiny); library(DT); library(readxl)
library(leaflet); library(tidyr); library(tidyselect)
library(TruncatedDistributions); library(reshape2)
library(rmapshaper)

##Parameters defined here are made Global (<<-)
#set number of Monte Carlo simulations for the Joe model
MC.sims<<-100

#stressor and stressor-response files
file.name<<-paste0(getwd(),"/ARTR_Joe_model/stressor-response_fixed_ARTR.xlsx")
file.name.2<<-paste0(getwd(),"/ARTR_Joe_model/stressor_magnitude_fixed_ARTR.xlsx")
#ShapeFile directory, filenames and HUC name within ShapeFiles
Shapefile.directory<<-"/ARTR_Joe_model/ARTR_HUC10_shapefile"
Shapefile.name<<-"ARTR_Range_HUC10_all"
HUC_Name<<-"HUC_10"

#scenarios to run
scn.run<<-"base"
#Boolean to trigger doses read from file
read.dose<<-TRUE

#read in shapefile as OGR
HUC.Map<-readOGR(paste0(getwd(),Shapefile.directory),Shapefile.name)
#simplify polygons to speed up plotting using rmapshaper::ms_simplify
HUC.Map <- ms_simplify(HUC.Map)
#transform to leaflet projection
HUC.Map <- spTransform(HUC.Map, CRS("+proj=longlat +ellps=WGS84"))

#create reactiveValue for updating tables figures and srressors
rv.model<-reactiveValues(update=0)
rv.map<-reactiveValues(update=0)
rv.stress<-reactiveValues(update=0)

#read in the different script files
source("Joe_functions.R")
source("Joe_plots.R")
source("Joe_curve.R")

#Run Joe model
scn.run.num<<-1
source("Joe_model.R")



