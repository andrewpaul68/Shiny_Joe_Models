#Script to run the Joe model

#read in dose table BUT Not if there
#has been slider input (which sets read.dose to false)
if (read.dose){
  #read in stressor doses
  #reading in an excel file
  dose<<-read_xlsx(file.name.2,sheet=scn.run[scn.run.num])
  dose$HUC_ID<-as.character(dose$HUC_ID)
  #for NA sub stressor types replace with the main stressor name
  dose$Sub_Type<-ifelse(dose$Sub_Type=="N/A"|dose$Sub_Type=="NA",
                         dose$Stressor,dose$Sub_Type)
}

#We now have two important objects:
#1) dose - This is a tibble of the doses for 
#each stressor (mean, SD, distribution type, low limit,
#up limit).  At the moment there are only two distribution 
#types allowed for uncertainty in the stressor dose, truncated normal
#and truncated lognormal.

#2) mean.resp.list - This is a list of lists
#The primary list is for each stressor ordered according to the tibble
#main.sheet.  Each list within this list contains
#4 approx functions that calculate the mean, SD, low limit and upper limit
#for system capacity given a dose for that stressor.

#Get unique HUCs and stressors.
hucs<-unique(dose$HUC_ID)
hucs
#overwrite the variable stressors with those from the dose table
#this allows the number of stressors that are run to be driven
#by those defined in the dose table and not the stressor-response functions.
stressors<-unique(dose$Stressor)
stressors


#Output is going to be stored in a 3D array to start.
#Each row will be a HUC, each column is system capacity
#for a stressor and sheets are different Monte Carlo
#simulations.  Order of HUCs (rows) and system capacity per 
#stressor (columns) are given in the vectors hucs and stressors,
#respectively. Make sys.capacity Global
sys.capacity<<-array(NA,dim=c(length(hucs),length(stressors),MC.sims),
                    dimnames = list(hucs,stressors,1:MC.sims))
#the next array and list is more for debugging purposes
dose.values<-array(NA,dim=c(length(hucs),length(stressors),MC.sims),
                   dimnames = list(hucs,stressors,1:MC.sims))
dose.values.list<-array(list(),dim=c(length(hucs),length(stressors)))

for (i in 1:length(hucs)){
  for (j in 1:length(stressors)){
    #find combination of HUC and stressor in the dose table
    pnt.dose<-intersect(grep(hucs[i],dose$HUC_ID),grep(stressors[j],dose$Stressor))
    #find stressor in main.sheet for relationship
    pnt.curv<-grep(stressors[j],main.sheet$Stressors)
    #call system capacity fucntion for each stressor
    temp.list<-sys.cap.func(dose[pnt.dose,],main.sheet[pnt.curv,],
                            stressor.list[[pnt.curv]],
                            mean.resp.list[[pnt.curv]])
    #assign system capacity for each stressor to array. 
    sys.capacity[i,j,]<-temp.list$sys.cap
    #store dose values as this is good output as well
    dose.values[i,j,]<-temp.list$dose
    #The next dose array stores doses as a list and includes
    #the individual additive doses (i.e., mortality doses)
    dose.values.list[i,j][[1]]<-temp.list$dose.mat
  }#end j
}#end k

#Print error messages if NA appears in the system capacity
#or stressor values arrays
if (any(is.na(sys.capacity))) message("ERROR - At least one NA in system capacity array")
if (any(is.na(dose.values))) message("ERROR - At least one NA in stressor values array")

#change the 3D array to a data frame
#adply is really slow
#system.time(sc.df<-adply(sys.capacity,c(1,2,3),function(x) data.frame(sys.cap=x)))
#melt is rocket fuel!!
#need to add "ID:" to huc names as melt drops "0" at the start of a huc name
row.names(sys.capacity)<-paste0("ID:",row.names(sys.capacity))
system.time(sc.df<-reshape2::melt(sys.capacity))
#do the same for doses as this is useful output
#dose.df<-adply(dose.values,c(1,2,3),function(x) data.frame(dose=x))
row.names(dose.values)<-paste0("ID:",row.names(dose.values))
dose.df<-reshape2::melt(dose.values)
#now delete "ID:" from all the huc names 
row.names(sys.capacity)<-substring(row.names(sys.capacity),4)
sc.df$Var1<-substring(sc.df$Var1,4)
row.names(dose.values)<-substring(row.names(dose.values),4)
dose.df$Var1<-substring(dose.df$Var1,4)
head(sc.df)
head(dose.df)

#rename columns
sc.df<-dplyr::rename(sc.df,HUC=Var1,Stressor=Var2,simulation=Var3,sys.cap=value)
head(sc.df)
#do same for dose values
dose.df<-dplyr::rename(dose.df,HUC=Var1,Stressor=Var2,simulation=Var3,dose=value)
head(dose.df)
#combine SC and dose dataframes (this df is used for output so make global)
sc.dose.df<<-merge(dose.df,sc.df)
head(sc.dose.df)

#add interaction type and link to sc.df data.frame
sc.df$int.type<-main.sheet$Interaction[match(sc.df$Stressor,main.sheet$Stressors)]
sc.df$link<-main.sheet$Linked[match(sc.df$Stressor,main.sheet$Stressors)]
head(sc.df,15)

#calculate cumulative system capacity by multiplying values in each row (HUC)
#the complicating factor is some stressors are linked
#(e.g., take the minimum of the linked and not their product)
#so we'll need to account for this.
#use ddply and the function ce.func to create the cumulative effect
#and make it global
#uses ddply which is pretty slow (use dplyr??)
system.time(ce.df<<-ddply(sc.df,~HUC+simulation,ce.func))
head(ce.df)

#plot distribution in cumulative effect for each HUC
cum.eff.plot(ce.df)

#select a HUC and plot distribution in system capacity
#for each stressor
sel.huc<-as.character(dose$HUC_ID[1])
huc.df<-cbind(simulation=1:length(sys.capacity[1,1,]),data.frame(t(sys.capacity[sel.huc,,])))
huc.df
huc.df<-pivot_longer(huc.df,cols=stressors,
                     names_to="stressor",values_to="sys.cap")
head(huc.df)
huc.plot(huc.df,sel.huc)


