
{#setup
  rm(list=ls())
  #checking for required packages and installing if missing
  required.packages <- c("tidyverse","data.table")#"mdpop","optimParallel","MortCast",,"readxl"
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  # data("UNlocations",package= "wpp2024" )   
  #these loads all required packages
  invisible(lapply(required.packages, library, character.only = TRUE))
  
  SSP.name = "SSP2" #Medium scenario
  iscen = "baseline"
  iscen_text = "reg_ori_edu_mort_fert_dom_mig"
  iscen_fullname = paste("e01",iscen,SSP.name,iscen_text,sep="_")
  
  baseline = T #baseline T to generate efert and emort
  
  #ignore
  efert = F #estimate differential fertility OR not
  iruneduasfr= F #whether to run the eduasfr optmization 
  newfert = F#if T then some information here
  
  emort = F # Now efert and iruneudasfr both are not needed 
  irunedult =  F #whether to run the edult optmization
  newmort = F#if T then some information here
  
  edu = F
  newedu = F#if T then some information here
  
  emig = T #this is THE FIRST TIME WE ARE BRINGING THE MIGRATION 
  newmig = F # use Dilek's input for the first time
  
  edom = T#
  
}#setup

# A5G2E6S35R2 (multi-dimensional multi-state)
# r statespace
# 17 autocomm* 5 origins*21 agegroups*2sex*4edu 
# 14280 boxes * 10 timesteps [2021 to 2071]
# 17* 5*21*2*4

#how many transitions?
# stochastic: 1 mort x (5*1edu) x 69 internal_mig x 1international_mig x Fertility x SRB
# deterministic: age_transition x no_sex_transition 
# New_members: births [35*2*7*1*6 = 2940]

source("statespace.r")#empty if baseline is F, then the statespace can already take the baseline value

source("fillstatespace.r")#

#transitions
source("main indproj V1.r")
# See "Report WIC3.Rmd"