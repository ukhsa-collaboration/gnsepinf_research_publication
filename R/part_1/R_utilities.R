## utlities for SGSS linkage scripts

# renv::restore()

## PACKAGES #####################################################################
library(odbc)
library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(stringi)
library(stringr)
library(forcats)
library(readxl)
library(lubridate)
library(phonics)
library(janitor)

## STORED FUNCTIONS #############################################################
source("./R/valid_nhs.R")
source("./R/patient_id.R")
source("./R/patient_episode.R")
source("./R/monomicrobial_episode.R")

## PHE Colour Palette ###########################################################
phe_colours <- c("#822433","#00B092","#EAAB00",
                 "#003087","#425563","#8CB8C6",
                 "#330072","#006747","#E8EDEE",
                 "#005EB8","#FAE100","#DAD7CB",
                 "#0072CE","#E9994A","#78BE20")

## UTILITY FUNCTIONS ############################################################
