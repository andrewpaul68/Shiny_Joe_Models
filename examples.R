#clean up workspace
rm(list=ls())

library(rgdal); library(plyr)
library(shiny); library(DT); library(readxl)
library(leaflet); library(tidyr); library(tidyselect)
library(TruncatedDistributions); library(reshape2)
library(rmapshaper)

#example reading in an excel file
file.name<-"stressor-response_example.xlsx"
main.sheet<-read_xlsx(file.name,sheet="Main")
#get rid of NAs in columns of interest and only select columns of interest
main.sheet<-main.sheet[!is.na(main.sheet$Stressors),1:3]
main.sheet

examp.list<-list(NULL)
#example reading in a stressor-response curve
#asssigns the data.frame to a list with the elements named 
#by the stressor (change tibble to data.frame)
examp.list[[1]]<-data.frame(read_xlsx(file.name,sheet=main.sheet$Stressors[1]))
#get rid of NAs and select columns of interest
examp.list[[1]]<-examp.list[[1]][!is.na(examp.list[[1]][,1]),1:4]


#example of using the approxfun and approx
#phosphorus
x<-examp.list[[1]][,1]
y<-examp.list[[1]][,2]/100
sd<-examp.list[[1]][,3]


#create approxfun must log x-axis as it's on log scale
phos.fun<-approxfun(log(x),y)
#try out the function
phos.fun(0.8)

#plot
plot.x<-c(seq(0.01,0.1,length.out=10),seq(0.1,1,length.out=10),
          seq(1,10,length.out=10),seq(10,100,length.out=10),
          seq(100,978,length.out=10))
plot.y<-phos.fun(log(plot.x))
plot(plot.x,plot.y,log="x",xlim=c(0.01,15))

#uncertainty
sims<-1000
plot.y.unc<-matrix(NA,nrow = sims, ncol = length(plot.x))

#let's build an approxfun with uncertainty
approx.unc<-function(dose,x,y,sd,log=F){
  if (log){
    y.out<-approx(log(x),y,xout=log(dose))$y*exp(rnorm(1,0,sd))
  }else {
    y.out<-approx(x,y,xout=dose)$y*exp(rnorm(1,0,sd))
  }
  return(y.out)
}

for (i in 1:sims){
  plot.y.unc[i,]<-approx.unc(dose=plot.x,x=x,y=y,sd=sd,log=T)
}

lines(plot.x,apply(plot.y.unc,2,quantile,probs=0.025),lty=2)
lines(plot.x,apply(plot.y.unc,2,quantile,probs=0.975),lty=2)

#What we want in the end is a 3D array
#where rows correspond to HUCS, columns are system capacity
#scores for each stressor, and sheets (3rd dimension) are replicate
#Monte Carlo simulations
HUCs<-2; stressors<-2; sims<-1
example.array<-array(NA,dim=c(HUCs,stressors,sims))
example.array
#