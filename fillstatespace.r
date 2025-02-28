
# Here, we start filling the empty cells. We will first fill it with the WIC2 values and assumptions and update the projection
# dttosave
# [1] "sxdt"   "asfrdt" "emrdt" "imrdt"  "idmrdt" (in) "odmrdt" (out) "popdt"  "propdt" "srbdt"  
if(T) {#newSSP
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
    # How to prepare the education distribution for 2020?
    #?? For countries with no new baseline data, we start the projection from 2015
    ## but use the population distribution from 2020 (agest and sex) from wpp2019?
    ## When will the wpp2022 present?
    # id.cols <- c(names(popdt)[1:5],"tob") #time of birth
    

# popdt -------------------------------------------------------------------
    id.cols <- names(popdt)[1:5]
    
    # base-year population to be update to 2015
    #popdt
    data1<-read_csv("../data/input_data_pnas/india_AGSRE_Baseline_state_space.csv")#
    setDT(data1)
    # data1[,table(var)]
    # data1%>%filter(period==2015,sex=="female",age==15,var=="pop")%>%select(edu,cc108)%>%
    #   mutate(prop=prop.table(cc108))
    
    #reg instead of cc #Time; sex m-f; edu e1-6; agest -5,0,5 statespace] - 2010 means 2011
    input <- data1[,setnames(.SD,c("period","age"),c("Time","agest"))][Time==2010&var=="pop"][,var:=NULL]
    input <- melt(input,id.vars = names(input)[1:4],
                  variable.name = "region",value.name="pop")%>%data.table()
    input[,`:=`(sex=substr(sex,1,1),Time = 2011)]
    
    # check for 70
    # xx <- unique(input$region)
    # length(intersect(xx,regions))
    
    id.cols.here <- intersect(id.cols,names(popdt))
    popdt[,pop:=-.00009][input,  pop:=i.pop, on = id.cols.here]
    
    #check (currently for 2011)
    popdt[Time==2011&agest==15]%>%spread(edu,pop)
    popdt[pop>0,sum(pop)]#1,210,827,147
    popdt[pop<0,table(agest,Time)]#
    popdt[agest==70&Time==2011&region=="IN.RJ_rural",.(agest,sex,edu,Time,pop)]
    

# sxdt --------------------------------------------------------------------
   #eduspecific nsx
    sxdt[,sx:=0] #[setDT(input),  sx:=value, on = id.cols.here]
    # H:\OneDriveIIASA\ruban new pnas V1\MSDem_0.0.2\R
    # C:\Users\kc\OneDrive - IIASA\DISPERSE\India\ruban\proj_work\Rfiles\mort {mx ax prepared here} - maybe there is direct nsx ..
    # ?? sx from ??
    
    #Run the following one time, it results in age-sex specific sx.final and also saves logit std lx
    if(F) source("mortality calc.r") #problem with dplyr and plyr
    #Here we prepare sx for age-sex-education specific [SSP2, SSP1-L and SSP3-H]
    if(F) source("mortality differential calculation.r") #save sx by edu
    
    #mort
    ##Moradhvaj prepared these
    issp.mort.var <- ssp.var[ssp==substr(SSP.name,4,4),.(region,mort)]
    setnames(issp.mort.var,"mort", "variant")
    
    ## we have run standard SSPs first for high low mortality
    ifiles <- dir("../data/mortality/",pattern="lt final edudiff.*rda",full.names = T)#[1:2] #ssp1 and ssp3
    names(ifiles) <- c("M","H","L")#ssp2 ssp3 ssp5
    #SSP3 = H mortality
    #SSP5 = L mortality
    
    ##collect
    sx.final.col <- NULL
    # imort = "L"
    for(imort in issp.mort.var[,unique(variant)]){
        print("We have e1 to e7 check once")
        lt.temp <- get(load(ifiles[imort]))
        setDT(lt.temp)
        lt.temp[region=="AP",region:="AD"]
        lt.temp[,region:=paste0("IN.",region,"_",tolower(residence))
         ][,Time:=midPer-2.5][,sex:=substr(sex,1,1)][,edu:=paste0("e",edu)][,agest:=agesx-5]
        lt.temp <- lt.temp[region%in%issp.mort.var[variant==imort,region]][,.(Time, region,sex, agest,edu, nsx)]
      sx.final.col <- rbind(sx.final.col,lt.temp)
    }
    #check for 70
     # sx.final.col[,unique(region)]
    # #TG is not separated
    # setdiff(sx.final[,unique(region)],regions.nm)
    # setdiff(regions.nm,sx.final[,unique(region)])
    # # id.cols
    
    sxdt[sx.final.col,on=id.cols,sx:=i.nsx]
    #check for empty data.table
    sxdt[sx==0]
    

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
    
    
# srbdt -------------------------------------------------------------------

    # head(input)
    if(F){
      srbdt[,srb:=-999][setDT(input), srb := 1000/i.srb, on = .(region)]
      #no sex differentials!!
      fun.srbdt <- function(df1){
      df1 <<- df1
      # stop()
      srbcorr = (1.05 - df1$srb[1]) / 8 #2051
      srbcorr = c(0,c(1:8,rep(8,9))*srbcorr)
      df2 <- copy(df1)[, srb := srb + srbcorr]
      return(df2)
    }
      srbdt <- srbdt[,by=.(region),fun.srbdt(.SD)]
      #check for constant values
      srbdt%>%spread(region,srb)
    }
    
    state.region.srb = read.csv("../data/fertility/state region final.csv")
    setDT(state.region.srb)
    # source("srb ml regression.r")
    load("../data/fertility/logistic model by region for srb.rda")
    educodes.srb = c("UptoPrimary", "LowerSecondary", "UpperSecondary")[c(1,1,1,2,3,3)]
    names(educodes.srb) = edunames
    
# propdt ------------------------------------------------------------------

    if(F){#run it once
      
      #In case of other SSPs, we are starting with SSP2's statespace and changing it
        eaprdt <- readxl::read_xlsx("../../ruban new pnas V1/education/ass_edu_ssps, V2_ms.xlsx")
        setDT(eaprdt)
        setnames(eaprdt,c("edu...13","agefin","yr"),c("edu","age","period"))
        
      for(SSP.name in  paste0("SSP",1:5)[-1]) {  
        
        # temp.eapr <- melt(ass.edu.dt,id.vars = c("period","sex","age","edu","var","areasex"),variable.name = "region")
        
        # eaprdt[,unique(edu)]
        # eaprdt[residence=="Rural"&sex=="female"&ID=="IN.WB"&agegr==0]
        # 
        if(SSP.name=="SSP4") eaprdt[edu%in%c("e12","e23","e34"),eapr.ssp4 := eapr.ssp4*0.9]
        
        # xx <- eaprdt[residence=="Rural"&sex=="female"&ID=="IN.BR"&edu=="e23"]
        # eaprdt[,pattern:=paste(sex,age,edu,ID,tolower(residence),sep="_")]
        # var.sel = c("period","pattern",paste0("eapr.",tolower(SSP.name)))
        # eaprdt <- eaprdt[,..var.sel]
        # setnames(eaprdt,paste0("eapr.",tolower(SSP.name)),"eapr")
        # ass.edu.dt[eaprdt,on = .(period,pattern),value:=eapr]
        # xy <- ass.edu.dt[residence=="rural"&sex=="female"&region=="IN.BR"&edu=="e23"]
      #########################
      
      
       var.sel = c("period","sex","age","edu","ID","residence",paste0("eapr.",tolower(SSP.name)))
        ieapr.final.col <- eaprdt[region!="india"&residence!="Total"][,..var.sel
                                  ][,region:=paste(ID,tolower(residence),sep="_")
                                    ][,`:=`(ID=NULL,residence=NULL)][,setnames(.SD,c("period","age",paste0("eapr.",tolower(SSP.name))),c("Time","agest","eapr"))
                                      ][,`:=`(sex=substr(sex,1,1),edu=substr(edu,1,2),Time=Time+1)][,tob := Time-agest-5]
          
          
        
    # ##edu (turn these into EAPRs) - problem with the data
    # ieapr.final.col <- data1%>%rename(Time=period)%>%
    #   filter(var=="eapr"&!(edu%in%c("e24","e35","e46"))&age>10)%>%
    #   gather(region,eapr,contains("_"))%>%select(-var)%>%
    #   mutate(sex=substr(sex,1,1),edu=substr(edu,1,2),Time=Time+1,tob = Time-age-5)%>%
    #   rename(agest=age)
    # setDT(ieapr.final.col)
    # 
    # ieapr.2106 <- copy(ieapr.final.col)[Time==2106]
    # 
    # for(i in 1:3){
    #   ieapr.xxx <- copy(ieapr.2106)[,Time:=Time+5*i][,tob:=tob+5*i]  
    #   ieapr.final.col <- rbind(ieapr.final.col,ieapr.xxx)
    #   }
    # 
    # 
    # setDT(ieapr.final.col)
    
    
    age.ult.edu <- c(15,15,20,25,30)
    names(age.ult.edu) <- edunames[-nedu]
    age.ult.edu.dtt <-as.data.table(data.frame(edu=edunames[-nedu],agest = age.ult.edu))
    
    fun.eapr.fill <- function(idf,iby){
      
      idf <<- idf
      iby <<- iby
      # stop("..")
      i <<- i + 1
      print(i)
      isex = iby$sex
      icc = iby$region
      
      {#get the baseline ratios (or eapr!!) from observations
        
        t = 2011 #base_year period
        iprop.base <- copy(popdt)[Time==t&region==icc&sex==isex,
                                  ][,setnames(.SD,"pop","value")
                                    ][,by=.(agest),value:=prop.table(.SD$value)]#%>%spread(edu,pop)
        # iprop.base.test <-iprop.base[agegr<10][,value:=round(value*100,0)]%>%spread(edu,value)
        ieapr <- iprop.base[,eapr(.SD),by=.(agest)][,tob:=Time-agest-5]
        
        # placeholder
        iprop.edu.lower.age <- data.table(edu=edunames[c(4,5,5,6,6,6)-1], #origin_edu
                                          agest=c(15,15,20,15,20,25),
                                          prop=0)
        
        
        #eapr might be changing with cohort. but prop might not
        #e.g. e4 - 15-19 (agegr = 4)
        irow = 2
        for(irow in 1:nrow(iprop.edu.lower.age)){
          
          #ieapr
          iedu.lm = iprop.edu.lower.age[irow,edu]
          #prop
          iedu.prop <-educodes[match(iedu.lm,educodes)+1]
          
          #active age
          iagest = iprop.edu.lower.age[irow,agest]
          #active age's itob
          itob <- t-iagest-5
          
          #ult age
          iult.age = age.ult.edu[iedu.lm]
          
          #cumprop of at least iedu.prop   e4+ [15, eapr3] e5+ [15, eapr4]
          cumprop_agest <- iprop.base[agest==iagest&
                                        edu%in%educodes[match(iedu.prop,educodes):6],sum(value)]
          # 84% with e4 at age 15_19
          # 51% with e5 at age 15_19
          
          #check if the itob is one of the tobs ending with 0 or 5
    
          ieapr_ult <- copy(idf)[tob==itob & agest<=iult.age]
          #Find ultimate value for itob at age 20-24
          icumprop.ult.edu<- ieapr_ult[eapr>0&edu%in%educodes[1:(match(iedu.lm,educodes))]][,prod(eapr)]
          #85% with e4 at age 20_24
          #66% with e5 at age 25_29
          
          iprop.agest_edu <- cumprop_agest/icumprop.ult.edu
          
          if(iprop.agest_edu>=1) iprop.agest_edu = 1
          iprop.edu.lower.age[edu==iedu.lm&agest==iagest,prop:=iprop.agest_edu]
          
          # iprop.edu.lower.age[,eapr:= paste0(edu,as.numeric(substr(edu,2,2))+1)]
          
        }#cal.prop_in_lower_ages
        
        
        
      }#cal.prop_in_lower_ages
      #now get me all the assumptions
      
      
      #for each cohort, we get their eaprs for younger ages
      
      # #what do we know at the time of max(int)
      # t = t-t%%5
      
      #those 25_29 years and younger.. needs to be projected until 30-34
      cumprop.proj.col <- NULL
      
      #t+5
      
      # itob = 1981# - Why 1980??
      # #agest at time 2020 for each cohort
      # itob = itob+5
      for(itob in seq(t-25-5,2091,by=5)){
        
        # if(itob == 1995) stop()
        
        iage.t = t-itob-5 #<30 for the projection
        
        istep.age = min((30-iage.t)/5,4)
        
        #30 start with ultimate eduage for each cohort
        if(istep.age > 0) {
          #start with ultimate eaprs at age 30-34
          idf1 <- copy(idf)[tob==itob&eapr>0][,agest:=30] #ulti_overall_age 
          
          idf1.missing <- copy(ieapr)[tob==itob&!(edu%in%unique(idf1$edu))
                      ][,setnames(.SD,"value","eapr")][,.(Time,agest,edu,eapr,tob)][,agest:=30]
          
          idf1 <- rbind(idf1.missing,idf1)
          
          idf1 <- idf1[,time:=itob+agest+5 #correspnding time tob = time - age -5
          ][,cumprop:=cumprod(eapr)
          ][,.(edu,tob,agest,time,cumprop)]
          
          cumprop.proj.col <- rbind(cumprop.proj.col,idf1)
          
          istep.age = istep.age-1
          #correcting for probable error
          #here we adjust - such that the cumprop for lower education is slightly higher than the cumprop of the next higher education 
          if(any(idf1[,diff(.SD$cumprop)]>0)){
            for (j in 1:4){
              if(idf1[j,cumprop] < idf1[j+1,cumprop]) idf1[j,cumprop] <-  idf1[j+1,cumprop]+.0001
            }
          }
          
          #25 [and then apply the proportions for younger ages..]
          i = 0
          while(istep.age>0){
            
            i = i + 1
            
            #age 
            idf1x <- copy(idf1)[,agest:=agest-5*i #go back one age
              ][,time:=time-5*i #go back one time
              ][iprop.edu.lower.age,on=.(edu,agest),cumprop:=cumprop*prop]
            
            if(any(idf1x[,diff(.SD$cumprop)]>0)){
              #here we adjust - such that the cumprop for lower education is slightly higher than the cumprop of the next higher education 
              for (j in 1:4){
                if(idf1x[j,cumprop] < idf1x[j+1,cumprop]) idf1x$cumprop[j] <-  idf1x[j+1,cumprop]+.0001
              }
              
            } 
            
            cumprop.proj.col <- rbind(cumprop.proj.col,idf1x)
            istep.age = istep.age-1
          }
        }#istep.age >0
        
      }#for itob
      
      
      # cumprop.proj.col[,table(agest,time)]
      
      
      # #15-34
      # idf_1534 <- cumprop.proj.col[time==2016]
      # 
      # if(min(idf_1534$tob)=="Inf") stop()
      # 
      # #35+
      # tob_3034_in2020 <- idf_1534[,min(tob)]
      # idf_35p <- copy(idf)[tob<tob_3034_in2020][,eapr:=un_logit(y)][
      #   ,cumprop:=cumprod(.SD$eapr),by=.(agest)][,.(edu,tob,agest,time,cumprop)]
      # # idf_35p[agest==35]
      # cumprop.proj.int.col <- rbind(cumprop.proj.col,idf_35p)
      # 
      # #check
      # fun.fix.negative <- function(dfx,iby){
      #   # dfx <<- dfx
      #   # iby <<- iby
      #   # stop()
      #   if(any(dfx[,diff(.SD$cumprop)]>0)) stop()
      # }
      # cumprop.proj.int.col[,fun.fix.negative(dfx=.SD,iby=.BY),by=.(tob,agest)]
      # 
      # 
      # 
      # cumprop.proj.int.col <- rbind(cumprop.proj.int.col,
      #                               if(t<2020)ieapr.base.test.save[,tob:=time-agest-5])
      # 
      return(cumprop.proj.col)
      
    }#fun.eapr.fill
    
    #collect for 2020 (input_prop) and for the future (15_30, 2016 onwards)
    i=0; icumprop.final.col <- ieapr.final.col[,fun.eapr.fill(.SD,iby=.BY),by=.(region,sex)]
    
    #check i = 400
    save(icumprop.final.col,file = paste0("../data/education/icumprop.final.col ",SSP.name,".rda"))
    }#ssps
    
    }#run it once
    # rm(icumprop.final.col)
    
    load(file = paste0("../data/education/icumprop.final.col ",SSP.name,".rda"))
    
    
    iprop.final.col <- icumprop.final.col%>%spread(edu,cumprop)
    setDT(iprop.final.col)
    iprop.final.col <-copy(iprop.final.col)[,`:=`(e6=e5,e5=e4-e5,e4=e3-e4,e3=e2-e3,e2=e1-e2,e1=1-e1)]
    
    #check for 
    iprop.final.col[,tot:=rowSums(.SD),.SDcols = e1:e6]
    any(abs(iprop.final.col[,tot]-1)>0.00001)
    iprop.final.col[,tot:=NULL]
    
    # save(iprop.final.col,file="../data/education/iprop.final.col.rda")
    
    iprop.final.col <- iprop.final.col%>%gather(edu,prop,e1:e6)
    setDT(iprop.final.col)
    
    propdt[,prop:=0][iprop.final.col[,setnames(.SD,"time","Time")],on=id.cols,prop:=i.prop]
    #check
    propdt[Time==2016,summary(prop)]
    

# migration ---------------------------------------------------------------
    #migration input
    if(F) source("mig calc.r") #NOTE:  smoothing validation is needed!!
    
    migOD_AG <- read.csv("../data/migration/mig final all variables 20240907.csv")
    setDT(migOD_AG)
    migOD_AG[,sum(mig04)]
    
    migOD_AG[,`:=`(dest=tolower(dest),orig=tolower(orig))][]
    
    migOD_AG[, c("cstnm", "cresi") := tstrsplit(dest, "_", fixed=TRUE)]
    migOD_AG[, c("lstnm", "lresi") := tstrsplit(orig, "_", fixed=TRUE)]
    migOD_AG[,cstnm:=stcodes$HASC[match(cstnm,tolower(stcodes$area0))]]
    migOD_AG[,lstnm:=stcodes$HASC[match(lstnm,tolower(stcodes$area0))]]
    
    migOD_AG <- migOD_AG[,.(lstnm,lresi,cstnm,cresi,agegr,sex,mrate.pred)][
      ,`:=`(dest=paste(cstnm,cresi,sep='_'),region=paste(lstnm,lresi,sep='_'))][
        ,agest:=(agegr-1)*5
        ][region==dest,mrate.pred:=0
        ][,.(region,dest,sex,agest,mrate.pred)
          ][,agest:=agest-5]#to match age at the beginning of the projection period
    # %>%spread(dest,mrate.pred)
    
    #int.mig
    issp.dom.var <- ssp.var[ssp==substr(SSP.name,4,4),.(region,dommig)]
    setnames(issp.dom.var,"dommig", "variant")
    print("will deal when we reach the point ")
    
    dom.ssp = data.table(expand.grid(region = issp.dom.var$region,Time = seq(2011,2096,5)))
    dom.ssp[issp.dom.var,on=.(region),variant:=variant][
      intmig.var,on=.(variant,Time),ssp.adj:=ssp.adj][
        ,`:=`(variant=NULL)]     
    print("Dom will be changed during the projection")
    
    # period    sex age edu      origin destination       value
    # 1:   2010   male   0  e1 IN.AN_urban IN.AN_urban 9.275084016
    # 2:   2010   male   0  e1 IN.AD_urban IN.AN_urban 0.054831155
    # 3:   2010   male   0  e1 IN.AR_urban IN.AN_urban 0.007690488
    # 4:   2010   male   0  e1 IN.AS_urban IN.AN_urban 0.005288164
    # 5:   2010   male   0  e1 IN.BR_urban IN.AN_urban 0.008869242
    # ---                                                          
    #   907376:   2010 female  70  e6 IN.TR_rural       World 0.000000000
    # 907377:   2010 female  70  e6 IN.UT_rural       World 0.000000000
    # 907378:   2010 female  70  e6 IN.UP_rural       World 0.000000000
    # 907379:   2010 female  70  e6 IN.WB_rural       World 0.000000000
    # 907380:   2010 female  70  e6       World       World 0.000000000
    
    #idmrdt
    id.cols.here <- intersect(id.cols,names(imrdt))
    
    idmrdt[,imr:=0]#[setDT(input),  imr:=mrate, on = id.cols.here]
    
    #edmrdt
    id.cols.here <- intersect(id.cols,names(imrdt))
    imrdt[,imr:=0]#[setDT(input),  imr:=mrate, on = id.cols.here]

    if(F){ 
      data2<-read_csv("../data/input_data_pnas/india_AGSRE_Baseline_mig.csv")#
      setDT(data2)
      # 2001 rates
      # age 0 - 70+ overall_age 
      # 0 international migration..
      # mrate per 1000
      data2 <- melt(data2[,var:=NULL],id.vars = c("period","sex","age","edu","origin"),variable.name = "destination")
    }
    
    #imr
    #imrdt
    id.cols.here <- intersect(id.cols,names(imrdt))
    imrdt[,imr:=0]#[setDT(input),  imr:=mrate, on = id.cols.here]
    
    #emrdt
    ##replace with new data ?? MnM has provided 16 countries
    id.cols.here <- intersect(id.cols,names(emrdt))
    emrdt[,emr:=0]#[setDT(input), emr:=mrate, on = id.cols.here]
    
        
    #urbanization variable ...
    # !!! Maybe it is easier to run the projection directly for other SSPs !!!
    #education component
    
    #SSPs


# reclass -----------------------------------------------------------------
data1[,unique(var)]
reclass.dt <-    data1[var%in%c("reclasstr","gap","perural")]

#get the model

if (nrow(reclass.dt) > 0) {
  m.vars <- names(reclass.dt)[-c(1:which(names(reclass.dt) == "var"))]
  reclass.dt <- melt(reclass.dt, measure.vars = m.vars)
  reclass.dt <- reclass.dt[grep("rural", reclass.dt$variable), ]
  reclass.dt <- dcast(reclass.dt, factor(variable) ~ var, value.var = "value")
  names(reclass.dt)[1] <- "region"
  reclass.dt$state <- sapply(reclass.dt$region, function(x) sub("_rural", "", x))
  setDT(reclass.dt)
  regions.oldtr <- reclass.dt[reclasstr > 0.05 & perural > 0.4 | sapply(reclass.dt$reclasstr, function(x) identical(all.equal(x, 0), TRUE)), 
                            .(state = state, reclasstr)]
  load("../data/urbanization/glmMigrReclass.rda")
  
  
} else {
  rm(reclass.dt)
}
    
#reclass
issp.reclass.var <- ssp.var[ssp==substr(SSP.name,4,4),.(region,reclass)]
setnames(issp.reclass.var,"reclass", "variant")
print("will deal with reclass when we reach the point ")

reclass.ssp = data.table(expand.grid(region = issp.reclass.var$region,Time = seq(2011,2096,5)))
reclass.ssp[issp.reclass.var,on=.(region),variant:=variant][
  reclass.var,on=.(variant,Time),ssp.adj:=ssp.adj][
    ,`:=`(variant=NULL)]     

area.vars = c("state","residence")
reclass.ssp <- unique(reclass.ssp[, c(area.vars) := tstrsplit(region, "_")][,.(Time,ssp.adj)])
print("Reclassification during the projection")


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
  
  