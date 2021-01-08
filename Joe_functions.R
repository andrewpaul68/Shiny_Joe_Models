#functions required for the Joe model

#load libraries for the functions
library(pracma) #needed for fsolve

#Need truncated distributions function from the 
#TruncatedDistributions package developed by 
#Rob Carnell <bertcarnell@gmail.com>
#found on R-Forge and not CRAN
# if(!require("TruncatedDistributions",character.only = TRUE)) {
#   install.packages("TruncatedDistributions", repos="http://R-Forge.R-project.org")
# }else library(TruncatedDistributions)

##################################
#function to estimate beta parameters
#given mean (rates) and SD for the
#the "untruncated" beta distribution
beta_param<-function(mn,sd){
  F <- function(x){
    Fmn <- x[1]/(x[1]+x[2])-mn
    Fvar<- x[1]*x[2]/((x[1]+x[2])^2*(x[1]+x[2]+1))-sd^2
    return(c(Fmn,Fvar))
  }
  ainit<-1  ; binit<-ainit/mn-ainit
  out<-NULL
  try(out<-suppressWarnings(fsolve(F,x0=c(ainit,binit))),silent=T)
  #if a solution is not found sd may be too large for the given mean
  #reduce sd by 10% and try again
  iter<-1
  if (is.null(out)){
    while(is.null(out)){
      if (iter>3) break #only try this 3 times
      sd<-sd*0.9
      print(paste0("try#",iter," mean=",mn," sd=",sd))
      F <- function(x){
        Fmn <- x[1]/(x[1]+x[2])-mn
        Fvar<- x[1]*x[2]/((x[1]+x[2])^2*(x[1]+x[2]+1))-sd^2
        return(c(Fmn,Fvar))
      }
      ainit<-1  ; binit<-ainit/mn-ainit
      try(out<-suppressWarnings(fsolve(F,x0=c(ainit,binit))),silent=T)
      iter<-iter+1
    }
  }
  #if a solution is still not found then set alpha and beta 
  #parameters to the uniform distribution of the beta (i.e., alpha=1,beta=1)
  if (is.null(out)) out$x<-c(1,1)
  return(out$x)
}
##################################

##################################
#function to give N random estimates
#for truncated beta distribution given vectors for mean
#and SD for the untruncated distribution of system capacity
#doesn't like 0 or 1 values
#returns 1 rnd value for each mean and sd value
tbeta_rnd<-function(mn_est,sd_est,low.limit=0,up.limit=1){
  rnd.est<-numeric()
  for (i in 1:length(mn_est)){
    if (sd_est[i]==0|is.na(sd_est[i])){  #no variance in response
      rnd.est[i]<-mn_est[i] #simply return the mean estimates
    }else{
      f_beta<-beta_param(mn_est[i],sd_est[i])
      rnd.est[i]<-rtbeta(1,alpha=f_beta[1],beta=f_beta[2],
                      a=ifelse(is.na(low.limit[i]),0,low.limit[i]),
                      b=ifelse(is.na(up.limit[i]),1,up.limit[i]))
    }
  }
  return(rnd.est)
}
##################################

##################################
#function to calculate cumulative
#system capacity mean and sd
#across simulations for each HUC
#from a series of Monte Carlo sims
mn.sd.huc<-function(df){
  mn<-mean(df$CE)
  sd<-sd(df$CE)
  return(data.frame(mean=mn,sd=sd))
}
##################################

##################################
#ddply function to calculate cumulative
#system capacity, the df contains system capacity 
#for each stressor for a given HUC and simulation
ce.func<-function(df){
  #separate stressors without a minimum interaction 
  sys.cap.no.int<-df$sys.cap[df$int.type!="Minimum"]
  #for those with a minimum interaction take the minimum
  sys.cap.min<-tapply(df$sys.cap[df$int.type=="Minimum"],df$link[df$int.type=="Minimum"],min)
  #AT THIS POINT NO OTHER INTERACTIONS ARE CONSIDERED
  #NOTE - Total mortality is addressed prior to calculation of system capacity 
  #Calculate the product across all cumulative effects
  #accounting for interactions
  ce.df<-data.frame(CE=prod(c(sys.cap.no.int,sys.cap.min)))
  return(ce.df)
}
##################################


##################################
#function to calculate system capacity for 
#each stressor.  Takes dataframes for doses 
#and stressor.list plus a list for the approx functions
#(f. for local list or dataframe)
#note some stressors have multiple doses (Additive interaction)
sys.cap.func<-function(f.dose.df,f.main.df,f.stressor.df,
                       f.mean.resp.list,n.sims=MC.sims){
  sub.stressors<-f.dose.df$Sub_Type
  #go through each stressors and randomly assign a dose
  n.dose<-length(sub.stressors)
  #matrix to store random doses, rows are additive sub types, cols are n.sims
  rnd.dose.mat<-matrix(NA,nrow=n.dose,ncol=n.sims)
  for (i in 1:n.dose){
    #Randomly generate doses based on mean, SD, limits and distribution type
    if (f.dose.df$Distribution[i]=="lognormal") {
      #if mean < 0 change mean to minimum limit (can't have zero or negative on log scale)
      mn.val<-ifelse(f.dose.df$Mean[i]<=0,
                     f.dose.df$Low_Limit[i],f.dose.df$Mean[i])
      #If SD zero adjust to a very small number
      if (f.dose.df$SD[i]==0){
        rnd.dose.mat[i,]<-rep(mn.val,n.sims)
      }else {
        rnd.dose.mat[i,]<-rtlnorm(n.sims,log(mn.val),f.dose.df$SD[i],
                                  a=f.dose.df$Low_Limit[i],b=f.dose.df$Up_Limit[i])
      }
    } else{
      if (f.dose.df$SD[i]==0){
        rnd.dose.mat[i,]<-rep(f.dose.df$Mean[i],n.sims)
      }else {
        rnd.dose.mat[i,]<-rtnorm(n.sims,f.dose.df$Mean[i],f.dose.df$SD[i],
                                 a=f.dose.df$Low_Limit[i],b=f.dose.df$Up_Limit[i])
      }
    }
  }#end i loop for doses
  
  #combine multiple stressors across rows into a single dose vector 
  #multiple stressors must be proportions (ie, conditional mortalities)
  rnd.dose<-1-apply(rnd.dose.mat,2,function(x) prod(1-x))
  
  #Constrain dose to be >= smallest dose and <= largest dose
  #from the curve function (ie., first/last value used for approx. function)
  #IE. extrapolation takes the last given system capacity score
  rnd.dose<-ifelse(rnd.dose<min(f.stressor.df[,1],na.rm=T),
                   min(f.stressor.df[,1],na.rm=T),rnd.dose)
  rnd.dose<-ifelse(rnd.dose>max(f.stressor.df[,1],na.rm=T),
                   max(f.stressor.df[,1],na.rm=T),rnd.dose)
  
  #Change rnd.dose to log values if stress-response relation is logarithmic
  if (f.main.df$Stress_Scale=="log"){
    x.dose<-log(rnd.dose)
  }else {
    x.dose<-rnd.dose
  }   
  
  #calculate system.capacity vector (x.dose is a vector)
  sys.capacity<-tbeta_rnd(mn_est=f.mean.resp.list[[1]](x.dose),
                          sd_est=f.mean.resp.list[[2]](x.dose),
                          low.limit=f.mean.resp.list[[3]](x.dose),
                          up.limit=f.mean.resp.list[[4]](x.dose))
  return(list(sys.cap=sys.capacity,dose=rnd.dose,dose.mat=rnd.dose.mat))
}
##################################


