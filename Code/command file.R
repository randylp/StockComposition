rm(list=ls()) #Clear the workspace and load necessary packages
setwd("S:/PST Chinook/CTCRPrograms/StockComposition/")
##########
##########
# GLOBAL #
##########
##########
#load libraries
  library(RColorBrewer)
  library(ggplot2)
  library(gridExtra)
#load local library
 source("Code/StackedAreaFunLibrary.R")
#other tidbits
 catchtype = "model-lc"
 ifpdf = FALSE
 caps_on = FALSE
 whichstocks = "9806"

#################################
# 2014 CLB & ERA Report Figures #
#################################
#params
 maxyear = 2013
#input files that YOU provide
 #auxiliary data
  auxcat     = read.delim("Data\\clb1402\\CLB1402auxCatch.txt",header=TRUE)
 #data from CLB directory
  fisherymap = read.delim("Data\\clb1402\\9806fisherymap.txt",header=TRUE)
  stockmap   = read.delim("Data\\clb1402\\9806stockmap.txt",header=TRUE)
  aabmcat    = read.csv("Data\\clb1402\\1402P_fish_AABM_CCC.csv", header=TRUE)
  isbmcat    = read.csv("Data\\clb1402\\1402P_fish_ISBM_CCC.csv", header=TRUE)
  obscat     = read.csv("Data\\clb1402\\CLB1402obsCatch - corrected.csv", header=TRUE)
#calculations
 #w/o seak addon
  stockcomp = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
 #w/ seak addon
  #not implemented until the 2016 clb & era report
#output directories
 if(!dir.exists("Results/2014clb_and_era_report"))               dir.create("Results/2014clb_and_era_report")
 if(!dir.exists("Results/2014clb_and_era_report/no_seak_addon")) dir.create("Results/2014clb_and_era_report/no_seak_addon")
#figures
 #w/o seak addon
  StackedAreaFigures(stockcomp, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2014clb_and_era_report/no_seak_addon", caps_on=caps_on)
#readme
 fileConn<-file("Results/2014clb_and_era_report/readme.txt")
 writeLines("NOTE: seak addon directory DNE b/c it was not imlemented until the 2017 clb & era report", fileConn)
 close(fileConn)

#################################
# 2016 CLB & ERA Report Figures #
#################################
#params
 maxyear = 2015
#input files that YOU provide
 #auxiliary data
  auxcat     = read.delim("Data\\clb1601\\CLB1601auxCatch.txt",header=TRUE)
 #data from CLB directory
  fisherymap = read.delim("Data\\clb1601\\9806fisherymap.txt",header=TRUE)
  stockmap   = read.delim("Data\\clb1601\\9806stockmap.txt",header=TRUE)
  aabmcat    = read.csv("Data\\clb1601\\1601P_fish_AABM_CCC.csv", header=TRUE)
  isbmcat    = read.csv("Data\\clb1601\\1601P_fish_ISBM_CCC.csv", header=TRUE)
  obscat     = read.csv("Data\\clb1601\\CLB1601obsCatch - corrected.csv", header=TRUE)
#calculations
 #w/o seak addon
  stockcomp = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
 #w/ seak addon
  #not implemented until the 2016 clb & era report
#output directories
 if(!dir.exists("Results/2016clb_and_era_report"))               dir.create("Results/2016clb_and_era_report")
 if(!dir.exists("Results/2016clb_and_era_report/no_seak_addon")) dir.create("Results/2016clb_and_era_report/no_seak_addon")
#figures
 #w/o seak addon
  StackedAreaFigures(stockcomp, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2016clb_and_era_report/no_seak_addon", caps_on=caps_on)
#readme
 fileConn<-file("Results/2016clb_and_era_report/readme.txt")
 writeLines("NOTE: seak addon directory DNE b/c it was not imlemented until the 2017 clb & era report", fileConn)
 close(fileConn)

#################################
# 2017 CLB & ERA Report Figures #
#################################
#params
 maxyear = 2016
#input files that YOU provide
 #auxiliary data
  auxcat     = read.delim("Data\\clb1702\\CLB1702auxCatch.txt",header=TRUE)
  seakaddon  = read.delim("Data\\clb1702\\CLB1702SEAKaddon.txt",header=TRUE)
 #data from CLB directory
  fisherymap = read.delim("Data\\clb1702\\9806fisherymap.txt",header=TRUE)
  stockmap   = read.delim("Data\\clb1702\\9806stockmap.txt",header=TRUE)
  aabmcat    = read.csv("Data\\clb1702\\1702P_fish_AABM_CCC.csv", header=TRUE)
  isbmcat    = read.csv("Data\\clb1702\\1702P_fish_ISBM_CCC.csv", header=TRUE)
  obscat     = read.csv("Data\\clb1702\\obsCatch.csv", header=TRUE)
#calculations
 #w/o seak addon
  stockcomp = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
 #w/ seak addon
  stockcomp_seakaddon = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL, seakaddon = seakaddon)
#output directories
 if(!dir.exists("Results/2017clb_and_era_report"))               dir.create("Results/2017clb_and_era_report")
 if(!dir.exists("Results/2017clb_and_era_report/seak_addon"))    dir.create("Results/2017clb_and_era_report/seak_addon")
 if(!dir.exists("Results/2017clb_and_era_report/no_seak_addon")) dir.create("Results/2017clb_and_era_report/no_seak_addon")
#figures
 #w/o seak addon
  StackedAreaFigures(stockcomp, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2017clb_and_era_report/no_seak_addon", caps_on=caps_on)
 #with seak addon
  StackedAreaFigures(stockcomp_seakaddon, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2017clb_and_era_report/seak_addon", caps_on=caps_on)
  #optional (and crudely implemented): remove all figures in the seak addon except for the seak figures to avoid duplicates
   mylist = list.files("Results/2017clb_and_era_report/seak_addon", full.names=TRUE)
   for(i in 1:length(mylist)) if(!any(unlist(lapply(list("7Alaska Net.jpg", "1Alaska Troll.jpg", "18Alaska Sport.jpg"), grepl, x=mylist[i])))) file.remove(mylist[i])

#################################
# 2018 CLB & ERA Report Figures #
#################################
#params
 maxyear = 2017
#input files that YOU provide
 #auxiliary data
  auxcat     = read.delim("Data\\clb1804\\CLB1804auxCatch.txt",header=TRUE)
  seakaddon  = read.delim("Data\\clb1804\\CLB1804SEAKaddon.txt",header=TRUE)
 #data from CLB directory
  fisherymap = read.delim("Data\\clb1804\\9806fisherymap.txt",header=TRUE)
  stockmap   = read.delim("Data\\clb1804\\9806stockmap.txt",header=TRUE)
  aabmcat    = read.csv("Data\\clb1804\\1804P_fish_AABM_CCC.csv", header=TRUE)
  isbmcat    = read.csv("Data\\clb1804\\1804P_fish_ISBM_CCC.csv", header=TRUE)
  obscat     = read.csv("Data\\clb1804\\obsCatch.csv", header=TRUE)
#calculations
 #w/o seak addon
  stockcomp = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
 #w/ seak addon
  stockcomp_seakaddon = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL, seakaddon = seakaddon)
#output directories
 if(!dir.exists("Results/2018clb_and_era_report"))               dir.create("Results/2018clb_and_era_report")
 if(!dir.exists("Results/2018clb_and_era_report/seak_addon"))    dir.create("Results/2018clb_and_era_report/seak_addon")
 if(!dir.exists("Results/2018clb_and_era_report/no_seak_addon")) dir.create("Results/2018clb_and_era_report/no_seak_addon")
#figures
 #w/o seak addon
  StackedAreaFigures(stockcomp, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2018clb_and_era_report/no_seak_addon", caps_on=caps_on)
 #with seak addon
  StackedAreaFigures(stockcomp_seakaddon, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/2018clb_and_era_report/seak_addon", caps_on=caps_on)
  #optional (and crudely implemented): remove all figures in the seak addon except for the seak figures to avoid duplicates
   mylist = list.files("Results/2018clb_and_era_report/seak_addon", full.names=TRUE)
   for(i in 1:length(mylist)) if(!any(unlist(lapply(list("7Alaska Net.jpg", "1Alaska Troll.jpg", "18Alaska Sport.jpg"), grepl, x=mylist[i])))) file.remove(mylist[i])

#########################################
# Phase 2 Model (BPC V1-25 AC6) Figures #
#########################################
#params
 maxyear = 2017
#input files that YOU provide
 #auxiliary data
  auxcat     = read.delim("Data\\BPCV1-25_AC6\\bpP2auxCatch.txt",header=TRUE)
  seakaddon  = read.delim("Data\\BPCV1-25_AC6\\bpP2SEAKaddon.txt",header=TRUE)
 #data from CLB directory
  fisherymap = read.delim("Data\\BPCV1-25_AC6\\bpP2fisherymap.txt",header=TRUE)
  stockmap   = read.delim("Data\\BPCV1-25_AC6\\bpP2stockmap.txt",header=TRUE)
  aabmcat    = read.csv("Data\\BPCV1-25_AC6\\2018P_fish_AABM_CCC.csv", header=TRUE)
  isbmcat    = read.csv("Data\\BPCV1-25_AC6\\2018P_fish_ISBM_CCC.csv", header=TRUE)
  obscat     = read.csv("Data\\BPCV1-25_AC6\\obsCatch.csv", header=TRUE)
#calculations
 #w/o seak addon
  stockcomp = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL)
 #w/ seak addon
  stockcomp_seakaddon = StockCompData(stockmap = stockmap, fisherymap = fisherymap, auxcat = auxcat, obscat = obscat, aabmcat = aabmcat, isbmcat = isbmcat, write_output = NULL, seakaddon = seakaddon)
#output directories
 if(!dir.exists("Results/BPCV1-25_AC6"))               dir.create("Results/BPCV1-25_AC6")
 if(!dir.exists("Results/BPCV1-25_AC6/seak_addon"))    dir.create("Results/BPCV1-25_AC6/seak_addon")
 if(!dir.exists("Results/BPCV1-25_AC6/no_seak_addon")) dir.create("Results/BPCV1-25_AC6/no_seak_addon")
#figures
 #w/o seak addon
  StackedAreaFigures(stockcomp, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/BPCV1-25_AC6/no_seak_addon", caps_on=caps_on)
 #with seak addon
  StackedAreaFigures(stockcomp_seakaddon, whichstocks = whichstocks, ifpdf = ifpdf, out_dir = "Results/BPCV1-25_AC6/seak_addon", caps_on=caps_on)
  #optional (and crudely implemented): remove all figures in the seak addon except for the seak figures to avoid duplicates
   mylist = list.files("Results/BPCV1-25_AC6/seak_addon", full.names=TRUE)
   for(i in 1:length(mylist)) if(!any(unlist(lapply(list("9Alaska Net.jpg", "1Alaska Troll.jpg", "27Alaska Sport.jpg"), grepl, x=mylist[i])))) file.remove(mylist[i])

#################################
# 2019 CLB & ERA Report Figures #
#################################
#params
 maxyear = 2018
#...

#########################################
# Phase 2 Model (BPC V1-27 AC6) Figures #
#########################################
#params
 maxyear = 2018
#...

#######
#######
# END #
#######
#######
rm(list=ls()) #Clear the workspace

