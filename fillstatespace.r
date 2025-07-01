
# Here, we start filling the empty cells. We will first fill it with the WIC2 values and assumptions and update the projection
# dttosave
# [1] "sxdt"   "asfrdt" "emrdt" "imrdt"  "idmrdt" (in) "odmrdt" (out) "popdt"  "propdt" "srbdt"  
if(F) {#newSSP [review for Spain]
  # stop()
  ssp.var <- read.csv("../data/India SSP variants sub-national, V2.csv")
  setDT(ssp.var)
  ssp.var[variant=="national",variant:= "nat"]
  
  ssp.var <- ssp.var[variant=="new"]
  
  ssp.var[ssp==3&region=="IN.AN_urban"]
  
  
  temp.var <- data.frame(variant=rep("H",18),
                         ssp.adj = c(seq(1.05, 1.2, 0.05), seq(1.2125, 1.25, 0.0125), rep(1.25, 10)),
                         Time = seq(2011,2096,by=5))
  
  fert.var = rbind(temp.var,
                   temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.95, 0.8, -0.05), seq(0.7875, 0.75, -0.0125), rep(0.75, 10))),
                   temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))
  
  mort.var = list(h = -0.5,l = 0.5)
  
  intmig.var = rbind(temp.var%>%mutate(variant="H",ssp.adj = c(seq(1.125, 1.5, 0.125), rep(1.5, 14))),
                     temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.875, 0.5, -0.125), rep(0.5, 14))),
                     temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))
  
  reclass.var = rbind(temp.var%>%mutate(variant="H",ssp.adj = c(seq(1.125, 1.5, 0.125), rep(1.5, 14))),
                      temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.875, 0.5, -0.125), rep(0.5, 14))),
                      temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))
  
}

if(iscen=="baseline"){

  
  # popdt -------------------------------------------------------------------
  id.cols <- names(popdt)[1:6]
  
  # base-year population to be update to 2021
  #popdt
  #edu = Non applicable (e1) --> Non applicable is transformed to Low, already in the excel 
  
  input <- readxl::read_xlsx("../data/Baseline.xlsx")%>%data.table()
  #  names(input) <- tolower(names(input))
  #  input[,unique(age)]
  input <- input[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  
  id.cols.here <- intersect(id.cols,names(popdt))
  popdt[,pop:=-.00009][input,  pop:=i.pop, on = id.cols.here]
  
  #check (currently for 2021)
  popdt[Time==2021&agest==15]%>%spread(edu,pop)
  popdt[pop>0,sum(pop)]#47.400.201
  #input[pop>0 & agest==0 & region == "reg1" & sex == "m" & origin == "ori1", by =.(edu),sum(pop)]
  #popdt[pop>0 & agest==0 & region == "reg1" & sex == "m" & origin == "ori1", by =.(edu),sum(pop)]
  
  
  # sxdt --------------------------------------------------------------------
  #eduspecific nsx
  sxdt[,sx:=0] #[setDT(input),  sx:=value, on = id.cols.here]
  
  
  ### Sx_0
  ### Sx_converging --> Survival growth, converging education differential in 2071
  ### Sx_maintaing --> --> Survival growth, maintaining education differential in 2071
  
  
  survival <- readxl::read_xlsx("../data/Sx_MED.xlsx")%>%data.table()
  #  names(input) <- tolower(names(input))
  #  input[,unique(age)]
  survival <- survival[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][age=="0",agest := -5
  ][age=="1-4",agest := 0
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  # The -5?!
  id.cols.here <- intersect(id.cols,names(sxdt))
  sxdt[,sx:=-0.00009][survival,  sx:=i.sx, on = id.cols.here]
  sxdt[agest==100, sx:=0]
  sxdt[sx<0]
  
  # asfrdt ------------------------------------------------------------------
  
  # #Note: for the first few periods, population by mother's edu is not available
  # #asfrs (7)
  # # asfrdt
  # input <- data1%>%rename(Time=period)%>%
  #             filter(var=="asfr")%>%
  #             gather(region,asfr,contains("_"))%>%select(-var)%>%
  #             mutate(sex=substr(sex,1,1),Time = Time+1)%>%
  #             rename(agest=age)
  # setDT(input)
  # head(input)
  # check
  # xx <- unique(input$region)
  # length(intersect(xx,regions))
  if(F){
  #Run it once independently
  # if(F) source("india fertility pnas scenarios, v3.R")
  if(F) source("fertility calc.r") #independently run
  input <- read.csv(paste("../data/fertility/ASFR pattern 1 final 20230404.csv",sep="")) #830: added India and a correction in e1_e4 (pred1 was there instead of pred1.4m)
  setDT(input)
  input[,area:=stcodes$HASC[match(area,tolower(stcodes$area0))]][
    ,region:=paste0(area,"_",tolower(residence))
  ][,Time:=yr-2.5][,agest:=age-2.5][,sex:="f"]
  id.cols.here <- intersect(id.cols,names(popdt))
  asfrdt[,asfr:=-999][input,  asfr:=i.asfr, on = id.cols.here]
  #check (currently for 2011)
  # input[agest==15&region=="IN.AN_urban",.(agest,sex,edu,Time,asfr)]
  # popdt[agest==15&Time==2011&region=="IN.AN_rural",.(agest,sex,edu,Time,pop)]
  
  #fert ssp correction
  issp.fert.var <- ssp.var[ssp==substr(SSP.name,4,4),.(region,fert)]
  setnames(issp.fert.var,"fert", "variant")
  
  asfrdt[issp.fert.var,on=.(region),variant:=variant][
    fert.var,on=.(Time,variant),asfr:=asfr*ssp.adj][
      ,`:=`(variant=NULL)]     
  
  #srb 
  # srbdt ??think about changing srb for Indian states.. [Fengqing Chao, KC...]
  data1[,unique(var)]
  input <- data1%>%
    filter(var=="sexr")%>%
    gather(region,srb,contains("_"))%>%select(-var)%>%
    mutate(sex=substr(sex,1,1))%>%select(region,srb)
  setDT(input)
  
  
  
  asfrdt[,asfr:=0] 
  asfrdt[, agest := as.numeric(agest)]
  }
  
  

  fert <- readxl::read_xlsx("../data/Fertility_growth_projection_MED.xlsx")%>%data.table()
  #  names(input) <- tolower(names(input))
  #  input[,unique(age)]
  
  fert <- fert[,agest:=(tstrsplit(agest,"-",keep=1))
  ][,agest:=as.numeric(agest)
  ][,origin := origins[origin]
  ][,edu := educodes[edu]
  ][,region := regions[region]]
  
  id.cols.here <- intersect(id.cols,names(asfrdt))
  asfrdt[,asfr:=-.0009][fert,  asfr:=i.asfr, on = id.cols.here]
  
  #check
  asfrdt[asfr < 0]
  
  fert[region == "reg9"]
  fert[region == "reg1" ]%>%spread(edu,asfr)
  
  asfrdt[agest == 15]
  asfrdt[asfr < 0 & agest > 10 & agest <50 & region == "reg2",]%>%spread(agest,asfr)
  
  # srbdt -------------------------------------------------------------------
  
  # head(input)
  srbdt[,srb:=1.05]
  
  # SEX RATIO FROM THE UN !!!!
  
  # propdt ------------------------------------------------------------------
  
  propdt <- readxl::read_xlsx("../data/propdt.xlsx")%>%data.table()
  propdt <- propdt[,setnames(.SD,"time","Time")]
  
  # migration ---------------------------------------------------------------
  
  intimmi <- readxl::read_xlsx("../data/International immigration_MED.xlsx")%>%data.table()
  #  names(input) <- tolower(names(input))
  #  input[,unique(age)]
  
  intimmi <- intimmi[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  id.cols.here <- intersect(id.cols,names(imrdt))
  imrdt[,imr:=0][intimmi, imr:=i.imr, on = id.cols.here]
  
  imrdt[imr>0,sum(imr)]
  
  
  #international emigration
  
  intemi <- readxl::read_xlsx("../data/International emigration_MED.xlsx")%>%data.table()
  
  intemi <- intemi[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  id.cols.here <- intersect(id.cols,names(emrdt))
  #emrdt[,emr:=0][intemi, emr:=i.emr, on = id.cols.here]
  
  emrdt <- merge(
    emrdt,
    intemi[, c(id.cols.here, "emr"), with = FALSE],
    by = id.cols.here,
    all.x = TRUE)
  emrdt[is.na(emr), emr := 0]
  
  emrdt[emr>0,sum(emr)]
  
  # internal immigration
  
  domimmi <- readxl::read_xlsx("../data/Internal immigration_MED.xlsx")%>%data.table()

  domimmi <- domimmi[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  
  id.cols.here <- intersect(id.cols,names(dimrdt))
  #dimrdt[,dimr:=0][domimmi, dimr:=i.dimr, on = id.cols.here]
  
  dimrdt <- merge(
    dimrdt,
    domimmi[, c(id.cols.here, "dimr"), with = FALSE],
    by = id.cols.here,
    all.x = TRUE)
  dimrdt[is.na(dimr), dimr := 0]
  
  dimrdt[dimr>0,sum(dimr)]
  
  #internal emigration
  
  domemi <- readxl::read_xlsx("../data/Internal emigration_MED.xlsx")%>%data.table()
  #  names(input) <- tolower(names(input))
  #  input[,unique(age)]
  
  domemi <- domemi[,agest:=(tstrsplit(age,"-",keep=1))
  ][age=="100+",agest := 100
  ][,agest:=as.numeric(agest)
  ][,age:=NULL
  ][,sex := sexcodes[sex]
  ][,edu := educodes[edu]
  ][,region := regions[region]
  ][,origin := origins[origin]]  
  
  id.cols.here <- intersect(id.cols,names(demrdt))
  # demrdt[,odmr:=0][domemi, odmr:=i.odmr, on = id.cols.here]
  
  demrdt <- merge(
    demrdt,
    domemi[, c(id.cols.here, "demr"), with = FALSE],
    by = id.cols.here,
    all.x = TRUE)
  demrdt[is.na(demr), demr := 0]
  
  demrdt[demr>0,sum(demr)]
  
  
  
  
  
  # NON Baseline ------------------------------------------------------------
  
}  else {#end if baseline  
  
  id.cols <- c(names(popdt)[1:5],"tob")
  # popdt - no change required
  
  # New Mort ----------------------------------------------------------------
  
  
  if(newmort){}#newmort
  
  # New Fert ----------------------------------------------------------------
  
  
  if(newfert) {
    print("asfrdt changes required")
  }#newfert  
  # srbdt - no change required
  
  # New Edu -----------------------------------------------------------------
  if(newedu) {
  }#newedu
  
  
  # New Mig -----------------------------------------------------------------
  
  
} #end if baseline  





# End ---------------------------------------------------------------------


# #For Graphics
# input.asfr <- read.csv("../data/fertility/asfr wic3.csv")%>%
#   rename(Time=year,agest=age,region=country_code)%>%
#   mutate(region = paste("reg",region,sep=""))%>%
#   mutate(asfr=value*pasfr*1000/5/100)%>%select(-pasfr,-value)%>%
#   arrange(region,Time,agest)
# setDT(input.asfr)
# input.tfr <- input.asfr[,.(tfr=sum(.SD$asfr,na.rm=T)/200),by=.(region,Time)][,edu:="all_input"]

