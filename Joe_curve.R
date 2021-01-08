#File to read and view stressor-response relations

#example reading in an excel file
main.sheet<<-read_xlsx(file.name,sheet="Main")
main.sheet

#Create a list to store each stressor-response relation
stressor.list<<-list(NULL)
stressors<-main.sheet$Stressors
num.stressors<-length(stressors)
#Read in each stressor-response curve and assign to a 
#data.frame for each list element
for (i in 1:num.stressors){
  stressor.list[[i]]<-data.frame(read_xlsx(file.name,sheet=main.sheet$Stressors[i]))
}

#build a list with the stressor-response functions
#each element of the primary list contains 4 approx functions:
#mean, cv, low limit and upper limit.
mean.resp.list<-list(NULL)
for (i in 1:num.stressors){
  if (main.sheet$Stress_Scale[i]=="log"){
    x<-log(stressor.list[[i]][,1])
  }else {
    x<-stressor.list[[i]][,1]
  }
  #func.type sets whether approximation function is continuous or stepped (constant)
  func.type=ifelse(main.sheet[i,5]=="continuous","linear","constant")
  #divide by 100 to convert percents to decimals
  temp.list<-list(approxfun(x,stressor.list[[i]][,2]/100,method=func.type), #mean function
                  approxfun(x,stressor.list[[i]][,3]/100,method=func.type), #SD function
                  approxfun(x,stressor.list[[i]][,4]/100,method=func.type), #low limit function
                  approxfun(x,stressor.list[[i]][,5]/100,method=func.type)) #up limit function
  mean.resp.list[[i]]<-temp.list
  rm(temp.list)
}

#plot the stressor-response relationship
# sel.stressor<-"StressorB"; sim=10 
# sel<-grep(sel.stressor,stressors)
# log.tran<-if (main.sheet[sel,4]=="linear") FALSE else TRUE
# if (log.tran){
#   min.stress<-min(log((stressor.list[[sel]][,1])))
#   max.stress<-max(log((stressor.list[[sel]][,1])))
# }else{
#   min.stress<-min((stressor.list[[sel]][,1]))
#   max.stress<-max((stressor.list[[sel]][,1]))
# }
# stres.resp.plot(mean.resp.list[[sel]],min=min.stress,
#                  max=max.stress,log.boo=log.tran,sims=sim,stress.name=sel.stressor)

