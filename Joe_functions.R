#functions required for the Joe model

#load libraries for the functions
library(pracma) #needed for fsolve

##################################
#function to estimate beta parameters
#given mean (rates) and SD for the
#the "untruncated" beta distribution
#updated using analytical solution from Kyle Wilson
beta_param<-function(mean,sd){
  #can't have a mean for the beta at 0 or 1, if at boundary
  #shift away by 0.001
  message.mean <- ifelse(mean==0|mean==1,
                         "Warning - Mean for beta at 0 or 1, shifted from boundary by 0.001",
                         "Mean for beta okay") # mean warning
  mean<-ifelse(mean==0,0.001,ifelse(mean==1,0.999,mean)) #crappy nested ifelse statement
  #the sd can't be too large either, use CV to check and use Kyle's truncation of CV
  #if needed
  cv <- sd/mean
  message.sd <- ifelse(((mean*cv)^2)>(mean*(1-mean)),
                       "; Warning - truncating CV for beta at sqrt(mean*(1-mean))/mean",
                       "; CV for beta okay") # sd warning
  cv <- ifelse(((mean*cv)^2)>(mean*(1-mean)),
               sqrt(mean*(1-mean))/mean,cv) # apply a truncation, if needed
  sd <- mean*cv
  alpha <- -((mean*(mean^2+sd^2-mean))/sd^2)
  beta <- alpha/mean-alpha
  return(list(alpha=alpha,beta=beta,message.mean=message.mean,
              message.sd=message.sd))
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
      #if mean < low.limit or > up.limit change to limit (can't have a mean outside the limits)
      mn.val<-ifelse(f.dose.df$Mean[i]<f.dose.df$Low_Limit[i],
                     f.dose.df$Low_Limit[i],f.dose.df$Mean[i])
      mn.val<-ifelse(mn.val>f.dose.df$Up_Limit[i],
                     f.dose.df$Up_Limit[i],mn.val)
      #If SD zero just repeat the mean
      if (f.dose.df$SD[i]==0){
        rnd.dose.mat[i,]<-rep(mn.val,n.sims)
      }else {
        rnd.dose.mat[i,]<-rtlnorm(n.sims,log(mn.val),f.dose.df$SD[i],
                                  a=f.dose.df$Low_Limit[i],b=f.dose.df$Up_Limit[i])
      }
    } else{
      #if mean < low.limit or > up.limit change to limit (can't have a mean outside the limits)
      mn.val<-ifelse(f.dose.df$Mean[i]<f.dose.df$Low_Limit[i],
                     f.dose.df$Low_Limit[i],f.dose.df$Mean[i])
      mn.val<-ifelse(mn.val>f.dose.df$Up_Limit[i],
                     f.dose.df$Up_Limit[i],mn.val)
      #If SD zero just repeat the mean
      if (f.dose.df$SD[i]==0){
        rnd.dose.mat[i,]<-rep(mn.val,n.sims)
      }else {
        rnd.dose.mat[i,]<-rtnorm(n.sims,mn.val,f.dose.df$SD[i],
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


