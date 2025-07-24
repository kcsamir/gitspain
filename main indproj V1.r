# Projection 
icheck = T #to collect the sums at each stage (sx_propedu, mig,migC,migfinal,)
final = copy(popdt)[,`:=`(births=0,pop1=0,idom=0,odom=0,emi=0,imm=0,deaths=0,edutran=0)]
unlink(dir(path_scen,full.names = T))#delete all files in the output folder

{ 
  iper = 2021
  # iper = iper+5
   # print("Until 2091")
  for (iper in seq(initime,fintime-ts,ts))
     {
    print(iper)
    
    #adult sx and edu
    {
    #pop1
    final.temp = copy(final)[Time == iper]
    
    # final.temp[,sum(pop),by=.(agest)]
    # final.temp[,sum(pop)]
    
    final.temp <- final.temp[sxdt,on=id.cols,`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
    # final.temp[,sum(deaths)] #2,254,790
    # final.temp[is.na(pop1)]
    # final.temp[hht=="IN.BR_rural"&agest==0]
    
    # final.temp[pop1 < 0 ]
    #eduprop [new transitions]
    # time and age as initime+5, corrected to match initime
    ieduprop <- copy(propdt)[Time==iper+5][
      ,`:=`(Time=Time-5,agest=agest-5)][prop<0,prop:=0.001][
      ,.(edu=edu,prop=prop.table(prop)),by=setdiff(id.cols,"edu")]

      # xx <- final.temp[sex=="f"&agest==15,.(edu,pop)][,prop:=prop.table(.SD$pop)]
    
    #agr pop 
    pop1agr <- copy(final.temp)[agest%in%10:20][,.(pop1=sum(pop1)),by=setdiff(id.cols,"edu")]
    pop1agr[,sum(pop1)] #7,312,340 [pop 10-24 years old in 2021 after applying mortality]
    #merge
    ieduprop[pop1agr,on=setdiff(id.cols,"edu"),`:=`(pop1=pop1*prop)]
    ieduprop[,sum(pop1)] #7,312,340
    #check
    ieduprop[pop1<0]
    
    ieduprop[agest == 10]
    #check (0.9999??)
    #bring pop1edu into pop1 (update)    
    final.temp[,sum(pop1)] #45,145,411
    final.temp[,pop1temp:=pop1
                ][agest%in%10:20,pop1:=0
                 ][ieduprop,on=id.cols,`:=`(pop1=i.pop1)][,edutran := pop1temp-pop1
                   ]
    # [,pop1temp:=NULL]
    
    final.temp[,by=.(agest),.(sum(pop1),sum(pop1temp,na.rm=T))]
    
    final.temp[,by=.(agest),.(sum(edutran))]
    final.temp[agest==10,]
    
    # final.temp[,sum(pop1)] #45145411 - 37833071
     final.temp[pop1<0 & agest>=0][,unique(edu)]

    #pick no id.cols 
    vars = names(final.temp)[7:15]
    rm(ieduprop,pop1agr)
    }

  #check (edutran = 0)
     if(icheck){ 
       final.summ <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time
                                                                    )][,pop1:=pop+births-deaths][,stage:="sx_eduprop"]
        print(final.summ)
        print(final.summ)
        
       if(T) { print("reg1")
        final.reg.summ <-final.temp[region=="reg1",lapply(.SD,sum),.SDcols = vars,by=.(Time)
                                    ][,pop1:=pop+births-deaths][,stage:="sx_eduprop"]
        print(final.reg.summ)
        print(final.reg.summ)
        }
        
     }
    
    
     # Time     pop births    pop1 emi imm       edutran   deaths stage
  # 1: 2020 7802030      0 7518767   0   0 -1.729177e-10 283262.2  pop1
  
  #emig0 means use given education-specific migration rate
  #Here in any case - we need to adjust for scenarios + net0 
  
    if(grepl("_mig",iscen_text)){
      print("Check for age and time consistency in migration")
    final.temp[copy(emrdt), #2020-2025, age at 2025, so to match with pop1 (with age at 2015)
               on=id.cols,`:=`(emi=pop1*i.emr)]#end of the period (to be applied, age is not there)
  #check age/time consistency + death (before or after migration)
   #check the scale
    #imm (need to be added)
      
      final.temp[copy(imrdt), #2020-2025, age at 2025, so to match with pop1 (with age at 2015)
                 on=id.cols,`:=`(imm=i.imm)]#end of the period (to be applied, age is not there)    
      
    }   
 
  if(grepl("_dom",iscen_text)){
    #BiRegional implementation
    #-5 to be added to newborns
    # stop("xxxa;'dsflkj")   
    
      # demrdt
      # dimrdt 
      
       #exposure rest of spain
    
    # final.temp[,by=.(origin,Time,sex,edu,agest),pop1C := sum(.SD$pop1)]
    
       final.temp[,by=.(origin,Time,sex,edu,agest),pop1RoC := sum(.SD$pop1)-pop1]
    
    
     final.temp[agest==30]
    
       #out dom mig
       final.temp[copy(demrdt),on=id.cols,`:=`(odom=pop1*i.demr)]
       #in dom mig
       final.temp[copy(dimrdt),on=id.cols,`:=`(idom=pop1RoC*i.dimr)]
       
       #
       dom.temp <- copy(final.temp)[agest>-5,by=.(origin,sex,agest,edu),.(odom=sum(odom),idom=sum(idom))
                                    ][,dom.adj := odom/idom]
       dom.temp[,.(sum(odom),sum(idom))]
       
       
       final.temp[dom.temp,on=setdiff(id.cols,c("region","Time")),idom:=idom*dom.adj
                  ][is.nan(idom),idom:=0]
       
      final.temp[,.(sum(odom),sum(idom))]
       
     }   
     
    
    
  final.temp[,pop1:=pop1-emi+imm-odom+idom]
   # copy(final.temp)[,.(corr = sum(emi)/sum(imm))]
   if(icheck) { 
     final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
        ][,stage:="mig"]
      final.summ = rbind(final.summ,final.summX)
      print(final.summ)
      
      print("reg1")
      final.reg.summX <-final.temp[region=="reg1"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,stage:="mig"]
      final.reg.summ = rbind(final.reg.summ,final.reg.summX)
      print(final.reg.summ)
   }
   
     
  #births # move to spanish

  birthsx <- final.temp[sex=="f"][,`:=`(odom=NULL,idom=NULL,emi=NULL,imm=NULL,edutran=NULL,sex=NULL)][,#this are still empty
      `:=`(popavg=.5*(.SD$pop+shift(.SD$pop1,1,NA,"lag"))),by = .(region,origin,edu)] [,
      `:=`(pop1=NULL,pop=NULL,deaths=NULL)][agest%in%15:49,][asfrdt,#age 15 at the start of iper
       on=setdiff(id.cols,"sex"),#get the asfr  births per woman year of exposure
      `:=`(births=(popavg*ts)*(i.asfr))][#calculate births 
        ,popavg:=NULL]
  
  birthsx[births < 0, births := 0]
  
  asfrdt[,by=.(region,origin,Time,edu),.(sum(asfr)*5)]
  
  # prepare SRB until the end
  birthsx[,`:=`(m=births*1.05/2.05,f=births*1.05/2.05)][,births:=NULL]
  birthsx <- melt(birthsx[,`:=`(pop1temp=NULL,pop1RoC=NULL)],
                  id=setdiff(c(id.cols),"sex"),variable.name = "sex",value.name = "births")%>%data.table()
  
  
  # srb = 110
  # srp = 110/(110+100)
  # ,hhtbig=NULL
  # birthsx[, origin := "ori1"]
  #update births to mother's row (later to update the final initime)
  final.temp[birthsx,on=id.cols,`:=`(births=i.births)]
  # final.summ <-final.temp[hht=="reg356",lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-pop1]
  if(icheck) {
    final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
    ][,pop1:=pop+births-deaths][,stage:="births"]
    final.summ = rbind(final.summ,final.summX)
    print(final.summ)
    
    
    final.reg.summX <-final.temp[region=="reg1"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="births"]
    final.reg.summ = rbind(final.reg.summ,final.reg.summX)
    print(final.reg.summ)
    
  } 
  
  #total births (by edu and sex) will be added to the -5 at final.temp initime
  birthstot = birthsx[,.(pop=sum(births)),by=setdiff(id.cols,"agest")][,agest:=-5]
  
  #update births [ need this for emort e015]
  final.temp[copy(birthstot),on=id.cols,`:=`(pop=i.pop)]
  
  # final.temp[agest==-5,sum(pop)]
  # final.temp[,sum(births)]
  #emort
  # <15 + new born.. get nsx corresponding to the difference in e0_e15
  birthstot <- birthstot[sxdt,on=id.cols,`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
  #add pop pop1 deaths 
  final.temp[copy(birthstot),on=id.cols,`:=`(pop1=i.pop1,deaths=i.deaths)]
  #check this..

  if(icheck) {
    final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
    ][,pop1:=pop+births-deaths][,stage:="births"]
    final.summ = rbind(final.summ,final.summX)
    print(final.summ)
    
    print("reg1")
    final.reg.summX <-final.temp[region=="reg1"][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="births"]
    final.reg.summ = rbind(final.reg.summ,final.reg.summX)
    print(final.reg.summ)
  }
  
  
  #add in final total births initime age -5
  final[copy(birthstot),on=id.cols,`:=`(pop=i.pop)] #pop1 will be updated later

  #Update the final with pop1, births, mig, edu transition
  final[copy(final.temp),on=id.cols,
        `:=`(deaths=i.deaths,births=i.births,imm=i.imm,emi=i.emi,odom=i.odom,idom=i.idom,edutran=i.edutran)] #new
  
  
  #End of the period age and Time
  #prepare for the next year [5+]
  final.temp.end<-copy(final.temp)[,`:=`(Time=Time+ts,agest=agest+ts,pop=pop1,pop1=NULL)]
  final.temp.end[agest>=100,agest:=100][,pop:=sum(pop),by=id.cols]
  
  # final.temp[,sum(pop)]
  # final.temp.end[,sum(pop)]
   
  #add end of the year population to the final
  final[final.temp.end,on=id.cols,`:=`(pop = i.pop)]

  }#loop of iper
}#for single country 

stop("..")

{

final <- final[pop==-999,pop:=-0.00001]#for year 2100 births
save(final,file=paste0(path_scen,"res_",iscen_fullname,".RData",sep=""))

final[agest>-5,by=.(Time),.(pop=sum(pop))]

final[agest > -5 & region == "reg14" & origin == "ori4", 
      by = .(region, Time), 
      .(pop = sum(pop))]

# final[,by=.(Time,sex),.(births=sum(births))]%>%spread(sex,births)%>%mutate(srb=m/f)

# xxx[state=="IN.KL"&ruban=="urban"]
# write.csv(xxx,"../results/James total popualtion by urban and rural states V2.csv")

vars = setdiff(names(final),id.cols)


final.summ.temp <-final[,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)][
  ,`:=`(pop=pop-births)][,pop1:=NULL] #births are already in 'pop'
print("get absolute edu transitions")

# final.summ.temp <-final[hht%in%sel.area][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time,hht)][
#   ,`:=`(pop=pop-births)][,pop1:=NULL] #births are already in 'pop'
 
#final.summ.temp[,by=.(Time),.(pop[2]/sum(pop))]

# if(final.summ.temp[,sum(emi)]==0){
#   print(round(final.summ.temp[,edutran:=NULL][,emi:=NULL][,imm:=NULL],0))
# } else {
#   print(round(final.summ.temp[,edutran:=NULL],0))
# }

# library(gt)
# round(final.summ.temp,0)%>%
#   gt() %>%
#   tab_header(
#     title = iscen_fullname,
#     # subtitle = glue::glue("{Global Population} to {}")
#   )%>% gtsave(paste0("../results/summary table",iscen_fullname,".png"))

#save results
# username = "kc"
#these files will be loaded for running different scenarios

#save dttosave
for(ifile in dttosave) {
  xxx<-get(ifile);save(xxx,file=paste(path_scen,ifile,".RData",sep=""))
  # if(username=="kc") save(xxx,file=paste(pdrive_path_scen,ifile,".RData",sep=""))
}  


#quick pyramid
if(F){
  # final
  dir(path_scen)
  dir(path_scen,pattern = "res_")
  # load(file=paste(path_scen,"res_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  final<-final[,scen:="Med"][Time<2096]
  source("funstack from mcbm.r")
  hhts
  ihht = "IN.MH_urban"#hhts[1]
  icnt =  ihht
  
  # function(figval,ivar,iage,isex,ihhts,icnt,itob,iiscen,ipropgraph=F,iscale=1,ihht=ihht)
  
  ggpyr2011<-funpyrwrapper_mcbm(figval = copy(final),
                           ivar="pop",
                           iTime=2011,
                           iiscen="Med",#can be deleted
                           iscale=1000)
  ggpyr2011
  ggpyr.col<-funpyrwrapper_mcbm(figval = copy(final),
                                ivar="pop",
                                iTime = unique(final$Time),
                                iiscen="Med",#can be deleted
                                iscale=1000)
  ggpyr.col
}
}#End Projection
# See "Report WIC3.Rmd"






