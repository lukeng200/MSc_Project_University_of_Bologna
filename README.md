# MSc_Coastal_Drainage_Analysis
Script, Data, and Table related to MS.c. Research

library(ggplot2)
library(magrittr)
library(ggpubr)
library(imputeTS)

Quinto_Annual <- read.delim("D:/Bologna Project/Land_Reclamation_Excel_Data/Fosso_Ghiaia.R/Quinto Basin/Quinto_Annual.txt")
#View(Quinto_Annual)
Rasponi_Annual <- read.delim("D:/Bologna Project/Land_Reclamation_Excel_Data/Rasponi.R/Rasponi Basin/Rasponi_Annual.txt")
#View(Quinto_Annual)
Savitale_Annual <- read.delim("D:/Bologna Project/Land_Reclamation_Excel_Data/Sanvitale.R/SanVitale Basin/Savitale_Annual.txt")
#View(Quinto_Annual)